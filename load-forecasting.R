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



# Zoom-in series: working vs non-working days

# Full train_set
ggplot(train_set,aes(x=Time, y=Demand/1000000, color=as.integer(!Workday)))+
  geom_line(size=0.25,show.legend = FALSE)+theme_bw()+
  labs(title="Electricity demand in Poland: working vs non-working days", y = "Demand [GW]")+ 
  theme(axis.text.x=element_text(size=9),axis.title=element_text(size=9.5), plot.title=element_text(size=11))

#first 2 weeks of January 2016
window <- train_set %>% filter(date(Time) < "2016-01-15")
ggplot(window,aes(x=Time, y=Demand/1000000,color=as.integer(!Workday)))+
  geom_line(size=1.2, show.legend = FALSE)+
  scale_x_datetime(date_breaks="2 days")+theme_bw()+theme(axis.text.x=element_text(size=10))+
  labs(title="Electricity demand in Poland: first two weeks of January 2016", y = "Demand [GW]")

#last 2 months of 2018
window <- train_set %>% filter(date(Time) > "2018-10-31")
ggplot(window,aes(x=Time, y=Demand/1000000,color=as.integer(!Workday)))+
  geom_line(size=0.75, show.legend = FALSE)+
  scale_x_datetime(date_breaks="2 weeks")+theme_bw()+
  theme(axis.text.x=element_text(size=10))+
  labs(title="Electricity demand in Poland: last two months of 2018", x = "Date", y = "Demand [GW]")



# day of week effect - difference in distribution
train_set %>% index_by(Wday = ~ wday(.,label=TRUE)) %>% summarize(mean(Demand))
ggplot(train_set,aes(x=Day_of_week, y=Demand/1000000,fill=Day_of_week)) +
  geom_boxplot(show.legend = FALSE) + theme_bw() + 
  theme(axis.text.x=element_text(size=11), plot.title=element_text(size=13.2)) +
  labs(title="Day of the week effect: electricity demand boxplots", y = "Demand") +
  scale_fill_brewer(palette = "Set2")

# day of week effect - daily curves
train_set$hm <- format(as.POSIXct(train_set$Time),"%H:%M")
dow_effect <- as.data.frame(train_set) %>% group_by(hm,Day_of_week) %>% summarize(Demand=median(Demand))
ggplot(dow_effect,aes(x = hm, y = Demand/1000000, color = Day_of_week,
                      group = Day_of_week)) +
  geom_line(size=1) + labs(x = "Time of day", y = "Electricity demand [GW]", 
                           title="Day of the week effect: median electricity demand by time of day") + 
  theme_bw()+ theme(axis.text.x=element_text(size=11), plot.title=element_text(size=13.2),
                    legend.text = element_text(size=10.5)) + 
  scale_colour_brewer(palette = "Dark2") +
  scale_x_discrete(breaks=c("00:00","03:00","06:00","09:00","12:00","15:00",
                            "18:00","21:00"))


daily_summer <- as.data.frame(train_set) %>% filter(Summer==TRUE) %>% group_by(hm,Workday) %>% summarize(Demand=median(Demand))
daily_winter <- as.data.frame(train_set) %>% filter(Summer==FALSE) %>% group_by(hm,Workday) %>% summarize(Demand=median(Demand))


hotmonths <- c("May","Jun","Jul")
nothotmonths <- c("Nov","Dec","Jan")
daily_summer <- as.data.frame(train_set) %>% filter(month(Time,label=TRUE) %in% hotmonths) %>% group_by(hm,Workday) %>% summarize(Demand=median(Demand))
daily_winter <- as.data.frame(train_set) %>% filter(month(Time,label=TRUE) %in% nothotmonths) %>% group_by(hm,Workday) %>% summarize(Demand=median(Demand))


# demand by time of day in hottest months
ggplot(daily_summer,aes(x = hm, y = Demand/1000000, color = Workday,
                        group = Workday)) +
  geom_line(size=1) + labs(x = "Time of day", y = "Electricity demand [GW]", 
                           title="Demand in May - July") + 
  theme_bw()+ theme(axis.text.x=element_text(size=11), plot.title=element_text(size=13.2),
                    legend.text = element_text(size=10.5)) +
  scale_colour_manual(values = c("#56b1f7","#0C3864")) +
  scale_x_discrete(breaks=c("06:00","12:00","18:00"))

# demand by time of day in coldest months
ggplot(daily_winter,aes(x = hm, y = Demand/1000000, color = Workday,
                        group = Workday)) +
  geom_line(size=1) + labs(x = "Time of day", y = "Electricity demand [GW]", 
                           title="Demand in November - January") + 
  theme_bw()+ theme(axis.text.x=element_text(size=11), plot.title=element_text(size=13.2),
                    legend.text = element_text(size=10.5)) +
  scale_colour_manual(values = c("#56b1f7","#0C3864")) +
  scale_x_discrete(breaks=c("06:00","12:00","18:00"))


# yearly seasonality - demand by time of day in different months
train_set$Month <- month(train_set$Time,label=TRUE)
daily_month <- as.data.frame(train_set) %>% group_by(hm,Month) %>% summarize(Demand=median(Demand))

ggplot(daily_month,aes(x = hm, y = Demand/1000000, color = Month,
                       group = Month)) +
  geom_line(size=0.9) + labs(x = "Time of day", y = "Electricity demand [GW]", 
                             title="Yearly seasonality effect: demand by time of day") + 
  theme_bw()+ theme(axis.text.x=element_text(size=11), plot.title=element_text(size=13.2),
                    legend.text = element_text(size=10.5))+
  scale_x_discrete(breaks=c("00:00","03:00","06:00","09:00","12:00","15:00",
                            "18:00","21:00")) + scale_colour_manual(values = c("#6087E0","#87C7E7","#8BFEF7","#6CFAA6","#8FE467","#86D800","#C5D800","#EED100","#EEA12A","#9672A6","#A306E7","#4900C0"))



# relationship between Demand and Temperature

# summer vs winter
ggplot(train_set,aes(x = Temperature, y = Demand/1000000, color = Summer)) +
  geom_point(size=0.9, alpha=train_set$Workday) + theme(axis.text=element_text(size=11),
                                                        legend.text = element_text(size=12)) +
  labs(x = "Temperature [°C]",
       y = "Electricity demand [GW]", 
       title="Relationship demand ~ temperature: summer vs. winter") + 
  theme_bw() + 
  scale_colour_manual(values = c("cornflowerblue","orange2"))+
  guides(color = guide_legend(override.aes = list(size=2)))

# workday vs non-workday

ggplot(train_set[4000:26304,],aes(x = Temperature, y = Demand/1000000, color = Workday)) +
  geom_point(size=0.9, alpha=0.55) +theme(axis.text=element_text(size=11))+
  labs(x = "Temperature [°C]",
       y = "Electricity demand [GW]",
       title="Relationship demand ~ temperature: working vs. non-working days")+
  theme_bw()+
  scale_colour_manual(values = c("#56b1f7","#0C3864"))+
  guides(color = guide_legend(override.aes = list(size=2)))

# day vs night (workday only)

work_only <- train_set %>% filter(Workday==FALSE)
work_and_day_only <- train_set %>% filter(Workday==TRUE) %>% filter(hour(Time) %in% seq(1,4))

ggplot(train_set,aes(x = Temperature, y = Demand/1000000, color = Day)) +
  geom_point(size=0.9, alpha=0.6) +theme(axis.text=element_text(size=11))+
  labs(x = "Temperature [°C]",
       y = "Electricity demand [GW]",
       title="Relationship demand ~ temperature (working day only): day vs. night")+
  theme_bw()+
  scale_colour_manual(values = c("#091B44","#78D3F9"))+
  guides(color = guide_legend(override.aes = list(size=2)))

ggplot(work_and_day_only,aes(x = Temperature, y = Demand/1000000)) +
  geom_point(size=1.15, alpha=0.5) +theme(axis.text=element_text(size=11))+
  labs(x = "Temperature [°C]",
       y = "Electricity demand [GW]") + theme_bw()


# relationship between lagged temperature (hour before) and demand
ggplot(train_set,aes(x = lag(Temperature), y = Demand/1000000)) +
  geom_point(size=1.15, color="#0093CE") +theme(axis.text=element_text(size=11))+
  labs(x = "Temperature [°C]",
       y = "Electricity demand [GW]") + theme_bw()

# mean temperature by month
train_set %>% index_by(Month = ~ yearmonth(.)) %>% summarize(mean(Temperature))
temp <- as.data.frame(train_set) %>% group_by(yearmonth(Time),Hour) %>% summarise(min(Temperature))


# fit some regression models to training data

# fit simple linear regression
lm1 <- lm(Demand~Temperature+Workday,data=train_set)
lm2 <- lm(Demand~NonWeekend_holiday,data=train_set)
car::vif(lm2)
summary(lm2)


# fit regression with splines
model1 <- lm(Demand~bs(Temperature, knots = c(16),degree=1),data=train_set)
model2 <- lm(Demand~bs(Temperature, knots = c(14,20,24),degree=2),data=train_set)
model3 <- lm(Demand~Temperature + bs(Temperature, knots = c(-2,16,24),degree=3),data=train_set)

#with interactions
model1Day <- lm(Demand~bs(Temperature, knots = c(-2,12,16,24),degree=1)*Day+Day+bs(Temperature, knots = c(16),degree=1),data=train_set)
model3Day <- lm(Demand~bs(Temperature, knots = c(-2,12,16,24),degree=3)+Day+ Day*bs(Temperature, knots = c(-2,16,20,24),degree=3),data=train_set)


model2wday <- lm(Demand~bs(Temperature, knots = c(-2,12,16,24),degree=1)*Workday+Workday+bs(Temperature, knots = c(16),degree=1),data=train_set)
model4wday <- lm(Demand~bs(Temperature, knots = c(-2,12,16,24),degree=3)+Workday+Workday*bs(Temperature, knots = c(-2,16,20,24),degree=3),data=train_set)

#train_set$Hour <- as.numeric(sub("(H|S).*","",hm(train_set$hm)))
train_set$Hour <- factor(hour(train_set$Time))
day_obs <- train_set %>% filter(Hour %in% seq(7,21))
night_obs <- train_set %>% filter(!Hour %in% seq(7,21))

work_obs <- train_set %>% filter(Workday==TRUE)
nonwork_obs <- train_set %>% filter(Workday==FALSE)


grid(nx = NULL, ny = NULL,
     lty = 1,      # Grid line type
     col = "#E9ECEC", # Grid line color
     lwd = 1)      # Grid line width

points(train_set$Temperature,train_set$Demand/10^6, col=alpha(rgb(0,0,0), 0.25))


points(train_set$Temperature,model3$fitted.values/10^6,col="green",lwd=1.5)
lines(train_set$Temperature,model2$fitted.values/10^6,col="orange",lwd=2)
points(train_set$Temperature,model1$fitted.values/10^6,col="red",lwd=1.5)

points(day_obs$Temperature,day_obs$fittedmodel1/10^6,col="red",lwd=1)
points(night_obs$Temperature,night_obs$fittedmodel1/10^6,col="red",lwd=1)

points(day_obs$Temperature,day_obs$fittedmodel3/10^6,col="green",pch=20)
points(night_obs$Temperature,night_obs$fittedmodel3/10^6,col="green",pch=20)


points(work_obs$Temperature,work_obs$fittedmodel3/10^6,col="red",pch=20)
points(nonwork_obs$Temperature,nonwork_obs$fittedmodel3/10^6,col="green",pch=20)

points(day_obs$Temperature,day_obs$fittedmodel1/10^6,col="green",pch=20)
points(night_obs$Temperature,night_obs$fittedmodel1/10^6,col="red",pch=20)



title("Piecewise linear regression",cex=0.99)
title("Cubic spline regression",cex=0.99)




