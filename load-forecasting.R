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
