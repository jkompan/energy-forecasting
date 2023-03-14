rm(list=ls())
library(dplyr)
library(tidyr)  # useful spread() and gather() functions
library(lubridate)
library(ggplot2)
library(tsibble)
library(feasts)
library(fable)
library(prophet)
library(splines)
library(readr)
sessionInfo()
search()  # lists everything that is currently attached


# line 20: pre-processing
# line 98: load already pre-processed data

###################### Pre-processing ######################

# import & transform PSE data
energy <- data.frame(matrix(ncol=3))
headers <- c("Time", "Hour", "Demand")
colnames(energy) <- headers
pse_files <- list.files(path="pse-data/", pattern="*.csv", full.names=TRUE, recursive=FALSE)
for (file in pse_files){
    r <- read.csv(file, sep = ";", header = TRUE)[,c(1,2,4)]
    colnames(r) <- headers
    energy <- rbind(energy, r)
}
energy <- energy[-1,]
energy[,3] <- as.numeric(sub(",","",energy[,3])) # remove commas & convert to numeric


# import weather data for Poland
weather <- read_csv("weather.csv", col_names = FALSE,skip = 315577)
weather <- cbind(weather[,c(1,68)])
colnames(weather) <- c("Time","Temperature")


# merge datasets
DATA <- as_tsibble(cbind(weather, Demand=energy$Demand))
rm(list=setdiff(ls(), "DATA"))  # remove everything except "DATA"


# check for nulls
sum(is.na(DATA$Demand))   # 28 missing values of Demand present (out of 35064)
sum(is.na(DATA$Temperature))
DATA$Demand <- forecast::na.interp(DATA$Demand)  # replace missing vals by interpolation
sum(is.na(DATA$Demand))


# add column with day of the week names
DATA$Day_of_week <- wday(DATA$Time, label = TRUE, week_start = getOption("lubridate.week.start", 1))


# add column 'Workday' for working days
holidays_pl <- read_csv("holidays-pl.csv", col_names = "Date") # list of holidays in Poland
DATA$Workday <- !(DATA$Day_of_week %in% c("Sat","Sun")) & !(date(DATA$Time) %in% holidays_pl$Date)


# other dummy variables
DATA$Saturday <- DATA$Day_of_week == "Sat"
DATA$Sunday <- DATA$Day_of_week == "Sun"
isAfterNonWork <- rbind(matrix(FALSE,24,1),!DATA[1:(nrow(DATA)-24),"Workday"])
DATA$Workday_after_nonWork <- DATA$Workday & rbind(isAfterNonWork)
DATA$NonWeekend_holiday <- !DATA$Workday & !(DATA$Day_of_week %in% c("Sat","Sun"))

DATA$Weekend_holiday <- !DATA$Workday & (DATA$Day_of_week %in% c("Sat","Sun"))

isAfterLong <- rbind(matrix(FALSE,24,1),!DATA[1:(nrow(DATA)-24),"Workday"])
isAfterLong2 <- rbind(matrix(FALSE,48,1),!DATA[1:(nrow(DATA)-48),"Workday"])
isAfterLong3 <- rbind(matrix(FALSE,72,1),!DATA[1:(nrow(DATA)-72),"Workday"])

DATA$WdayAfterLong <- DATA$Workday & isAfterLong & isAfterLong2 & isAfterLong3
  
DATA$Workday_before_nonWork <- DATA$Workday & isBeforeNonWork
DATA$BeforeAfter <- DATA$Workday & DATA$Workday_after_nonWork & DATA$Workday_before_nonWork
isBeforeNonWork <- rbind(!DATA[1:(nrow(DATA)-24),"Workday"],matrix(TRUE,24,1))


  
#saveRDS(object = DATA, file = "DATA.rds")
#write.csv(DATA,"data.csv", row.names = FALSE)


# SEPARATE SET FOR DAILY DATA
#dailyDATA <- DATA %>% index_by(Date = ~ as_date(.)) %>% 
#  summarize(Demand=mean(Demand), Temperature=mean(Temperature), Workday=as.logical(mean(Workday)))

#saveRDS(object = dailyDATA, file = "dailyDATA.rds")
#write.csv(dailyDATA,"daily_data.csv", row.names = FALSE)



###################### Import Hourly Data ######################

DATA <- readRDS("DATA.rds")     # load already pre-processed data

# split dataset into train and test sets
train_set <- DATA %>% filter(year(Time) <= 2018)
test_set <- DATA %>% filter(year(Time) > 2018)



###################### Exploratory Data Analysis ######################


# first visualize full data

# plot Demand series
ggplot(DATA,aes(x=Time, y=Demand/1000000))+
  geom_line(size=0.25,show.legend = FALSE) + theme_bw() +
  labs(title="Electricity demand in Poland (2016-2019)", y = "Demand [GW]") + 
  theme(axis.text.x=element_text(size=11))

# plot Temperature series
ggplot(DATA,aes(x=Time, y=Temperature))+
  geom_line(size=0.25,show.legend = FALSE) + theme_bw() +
  labs(title="Temperature in Poland (2016-2019)", y="Temperature [°C]") + 
  theme(axis.text.x=element_text(size=11))


# analysis of training data

# autocorrelation (ACF) and partial autocorrelation (PACF)
train_set %>% ACF(Demand,lag_max=216) %>% 
  autoplot() + labs(title = "Autocorrelation (ACF) of electricity demand ") +
  xlab("lags") + theme_bw()

train_set %>% PACF(Demand,lag_max=216) %>% 
  autoplot() + labs(title = "Partial autocorrelation (PACF) of electricity demand ") +
  xlab("lags") + theme_bw()

# KPSS unit root test
train_set %>%     
  features(Demand, unitroot_kpss)

train_set$Demand %>% forecast::msts(seasonal.periods = c(24,24*7,24*365.25)) %>% 
  forecast::mstl() %>% autoplot()

# STL decomposition
train_set %>%
  model(
    STL(Demand ~ season("1 day", window=30) + 
          season(period = "1 week", window=Inf) +
          season(period = "1 year", window=Inf),
        robust = TRUE)
  ) %>%
  components() %>%
  autoplot() + labs(x = "Observation") + theme_bw()


# label obs as "summer" months and "day" hours
train_set$Summer <- month(train_set$Time, label=TRUE) %in% c("May","Jun","Jul","Aug","Sep")
train_set$Day <- hour(train_set$Time) %in% seq(7,21)


# histograms:
ggplot(train_set, aes(x = Demand/10^6)) + 
  geom_histogram(alpha = 0.5, position = "identity", binwidth=0.15)+
  theme_bw()+labs(x="Demand [GW]",title="Histogram")

ggplot(train_set, aes(x = Demand/10^6, fill = Summer)) + 
  geom_histogram(alpha = 0.75, position = "identity", binwidth=0.15)+
  theme_bw()+labs(x="Demand [GW]",title="Histogram: summer and winter")+
  scale_fill_manual(values = c("cornflowerblue","orange2"))

ggplot(train_set, aes(x = Demand/10^6, fill = Workday)) + 
  geom_histogram(alpha = 0.8, position = "identity", binwidth=0.15)+
  theme_bw()+labs(x="Demand [GW]",title="Histogram: working and non-working days")+
  scale_fill_manual(values = c("#56b1f7","#0C3864"))

ggplot(train_set, aes(x = Demand/10^6, fill = Day)) + 
  geom_histogram(alpha = 0.8, position = "identity", binwidth=0.15)+
  theme_bw()+labs(x="Demand [GW]",title="Histogram: day and night")+
  scale_fill_manual(values = c("#091B44","#78D3F9"))
