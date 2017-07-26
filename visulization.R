#visulize
library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)

setwd("C:\\Users\\Ran\\OneDrive\\cabi-trip-history-data")
rm(list=ls(all=TRUE))
load("FinalDataNew.rdata")
FinalDataNew = FinalData
FinalData.Date = FinalDataNew$StartDate
FinalData.StartStation = FinalDataNew$StartStationNumber

#FinalDataNew2 = FinalDataNew$StartDate
rm("FinalDataNew")
gc()
SummaryHour = hour(ymd_hms(FinalData.Date))
gc()
SummaryDay = day(ymd_hms(FinalData.Date))
gc()
SummaryWeekday = weekdays(ymd_hms(FinalData.Date))
gc()
SummaryMonth = month(ymd_hms(FinalData.Date))
gc()
SummaryYear = year(ymd_hms(FinalData.Date))
gc()
SummaryTime =format(FinalData.Date, "%H:%M")
SummaryTimeNumeric = (as.numeric((FinalData.Date))-300*60)%%86400/60
SummaryDateAndTime = data.frame(SummaryTimeNumeric, SummaryTime, SummaryHour, SummaryDay, SummaryWeekday,
                                 SummaryMonth, SummaryYear)
colnames(SummaryDateAndTime) = c("time_num","time","hour", "day", "weekday", "month", "year")

SummaryDateAndTime$month=as.factor(SummaryDateAndTime$month)
SummaryDateAndTime$hour=as.factor(SummaryDateAndTime$hour)
SummaryDateAndTime$day=as.factor(SummaryDateAndTime$day)
SummaryDateAndTime$weekday=as.factor(SummaryDateAndTime$weekday)
SummaryDateAndTime$year=as.factor(SummaryDateAndTime$year)

gc()
save(SummaryDateAndTime,file="SummaryDateAndTime.rdata")

StartStationinWeek.df = data.frame(FinalData.StartStation, SummaryWeekday)

load("SummaryDateAndTime.rdata")
month_summary = ddply(SummaryDateAndTime,.(month,hour),
                      summarise,N = length(hour))
ggplot(SummaryDateAndTime, aes(x = hour, y = N, colour = month)) +
  geom_point(data = month_summary, aes(group = month)) +
  geom_line(data = month_summary, aes(group = month)) +
  #scale_colour_gradientn(colours=rainbow(4))+
  scale_x_discrete("hour") +
  scale_y_continuous("N") +
  theme_minimal() +
  ggtitle("People rent bikes more from April to June.") + 
  theme(plot.title=element_text(size=18))

year_summary = ddply(SummaryDateAndTime,.(year),
                     summarise,N = length(year))

ggplot(data=year_summary, aes(x=year, y=N)) +
  geom_bar(stat="identity")

time_summary = ddply(SummaryDateAndTime,.(time),
                     summarise,N = length(time))

ggplot(data=time_summary, aes(x=time, y=N)) +
  geom_bar(stat="identity")

time_num_summary = ddply(SummaryDateAndTime,.(time_num),
                     summarise,N = length(time_num))

ggplot(SummaryDateAndTime, aes(x = time_num, y = N)) +
  geom_point(data = time_num_summary) +
  geom_line(data = time_num_summary) +
  #scale_colour_gradientn(colours=rainbow(4))+
  scale_x_continuous("time_num") +
  scale_y_continuous("N") +
  theme_minimal() +
  ggtitle("People rent bikes more from April to June.\n") +
  theme(plot.title=element_text(size=18))

weekday_summary = ddply(SummaryDateAndTime,.(weekday,hour),
                      summarise,N = length(hour))
ggplot(SummaryDateAndTime, aes(x = hour, y = N, colour = weekday)) +
  geom_point(data = weekday_summary, aes(group = weekday)) +
  geom_line(data = weekday_summary, aes(group = weekday)) +
  #scale_colour_gradientn(colours=rainbow(4))+
  scale_x_discrete("hour") +
  scale_y_continuous("N") +
  theme_minimal() +
  ggtitle("Different demand in weekday and weekend.\n") + 
  theme(plot.title=element_text(size=18))

