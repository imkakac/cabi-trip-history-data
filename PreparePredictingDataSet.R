setwd("C:\\Users\\Ran\\OneDrive\\cabi-trip-history-data")
rm(list=ls(all=TRUE))

library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)

# load("FinalDataNew.rdata")
# load("SummaryDateAndTime.rdata")
# load("SummaryEndDateAndTime.rdata")
# 
# Position = !is.na(FinalDataNew$StartStationNumber) &!is.na(FinalDataNew$EndStationNumber) &
#             FinalDataNew$StartStationNumber != 0 &  FinalDataNew$StartStationNumber != 1 &
#             FinalDataNew$StartStationNumber != 2
# 
# DemandPredictionDataSet = cbind.data.frame(FinalDataNew[c(4,9)], SummaryDateAndTime[c(7,6,4,5,3)], SummaryEndDateAndTime[c(1,8,7,5,6,4)])
# DemandPredictionDataSet$year = as.numeric(as.character(DemandPredictionDataSet$year))
# DemandPredictionDataSet$month = as.numeric(as.character(DemandPredictionDataSet$month))
# DemandPredictionDataSet$day = as.numeric(as.character(DemandPredictionDataSet$day))
# DemandPredictionDataSet$hour = as.numeric(as.character(DemandPredictionDataSet$hour))
# DemandPredictionDataSet$end_day = as.numeric(DemandPredictionDataSet$end_day)
# DemandPredictionDataSet$end_hour = as.numeric(DemandPredictionDataSet$end_hour)
# 
# DemandPredictionDataSet = subset(DemandPredictionDataSet, Position)
# save(DemandPredictionDataSet,file="DemandPredictionDataSet.rdata")

load("DemandPredictionDataSet.rdata")
HourlyDemandPrediction.df = count(DemandPredictionDataSet,c(1,3,4,5,7))
HourlyDemandPrediction.end.df = count(DemandPredictionDataSet,c(8,9,10,12,13))
TopStations = count(DemandPredictionDataSet,c('StartStationNumber', 'weekday'))
TopStations = TopStations[order(-TopStations$freq), ]
Top10Stations = as.numeric(as.character(unique(head(TopStations$StartStationNumber,35))))



StationX = Top10Stations[1]
StationX.df = subset(DemandPredictionDataSet, DemandPredictionDataSet$StartStationNumber == StationX)
StationX.summary = ddply(StationX.df,.(month,weekday,hour),
                        summarise,N = length(hour))
StationX.summary.detail = ddply(StationX.df,.(year,month,day,weekday,hour),
                         summarise,N = length(hour))
EndStationX.df = subset(DemandPredictionDataSet, DemandPredictionDataSet$EndStationNumber == StationX)

EndStationX.summary = ddply(EndStationX.df,.(month,weekday,hour),
                                   summarise,N = length(end_hour))
EndStationX.summary.detail = ddply(EndStationX.df,.(end_year,end_month,end_day,end_weekday,end_hour),
                         summarise,N = length(end_hour))
colnames(EndStationX.summary.detail)[1:5] = c('year','month','day','weekday','hour')

StationX.summary.InAndOut = 

StationX.summary2 = ddply(StationX.df,.(weekday,hour),summarise,N = length(hour))
# StationX.DayAndHour = aggregate(StationX.summary[,4], by=list(weekday = StationX.summary$weekday,
                                             # hour = StationX.summary$hour), FUN = sum)
StationX.DayAndHour = aggregate(N ~ weekday + hour, data = StationX.summary, FUN = sum)
ggplot(StationX.DayAndHour, aes(x = hour, y = N, colour = weekday)) +
  geom_point(data = StationX.DayAndHour, aes(group = weekday)) +
  geom_line(data = StationX.DayAndHour, aes(group = weekday)) +
  # scale_colour_gradientn(colours=rainbow(4))+
  #scale_x_discrete("hour") +
  scale_y_continuous("N") +
  theme_minimal() +
  ggtitle(sprintf("Rental demands in Station %d", StationX)) + 
  theme(plot.title=element_text(size=18))+
  theme(plot.title = element_text(hjust = 0.5))

library(plotly)
z1 = xtabs(N ~ hour + month, data = subset(StationX.summary, StationX.summary$weekday == 'Saturday'))

p <- plot_ly(x = StationX.summary$hour, y = StationX.summary$month, z = StationX.summary$N) %>% add_surface()
p <- plot_ly(showscale = FALSE) %>%  add_surface(z = ~z)
p1 <- plot_ly(showscale = TRUE) %>%  add_surface(z = ~z1) %>%

layout(                        # all of layout's properties: /r/reference/#layout
  title = "Rental Demand on Saturdays in Station 31623, x = month, y= hour, z1 = count", # layout's title: /r/reference/#layout-title
  xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
    title = "Time",      # xaxis's title: /r/reference/#layout-xaxis-title
    showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
  yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
    title = "uidx")
)
# test1 = StationX.summary.detail[1:10,]
# test2 = EndStationX.summary.detail[1:10,]
# test3 = merge.data.frame(test1, test2, 
#                          by = c('year','month','day','weekday','hour'),
#                          all = TRUE)
# test3[is.na(test3)] = 0

StationX.demand.detail = merge.data.frame(StationX.summary.detail, EndStationX.summary.detail,
                                   by = c('year','month','day','weekday','hour'),
                                   all = TRUE)
StationX.demand.detail[is.na(StationX.demand.detail)] = 0
colnames(StationX.demand.detail)[6:7] = c('out','In')
StationX.demand.detail$netout = StationX.demand.detail$out - StationX.demand.detail$In

#multi linear and polynomial regression
dataset.out = StationX.demand.detail[c(1:5,6)]
# dataset.out$weekday = factor(dataset.out$weekday,
#                        levels = c('Monday', 'Tuesday', 'Wednesday','Thursday','Friday','Saturday','Sunday'),
#                        labels = c(1, 2, 3, 4, 5, 6, 7))
# levels(dataset.out$weekday) = list('0' = c('1','2','3','4','5'), '1' = c('6','7'))
# dataset.out$hour_sqr = dataset.out$hour^2
# dataset.out = dataset.out[c(1:5,7,6)]
# library(caTools)
# set.seed(123)
# split = sample.split(dataset.out$out, SplitRatio = 0.8)
# training_set = subset(dataset.out, split == TRUE)
# test_set = subset(dataset.out, split == FALSE)
# 
# # Fitting Multiple Linear Regression to the Training set
# regressor = lm(formula = out ~ .,
#                data = training_set)
# 
# # Predicting the Test set results
# y_pred = predict(regressor, newdata = test_set)
# summary(regressor)
# tail(y_pred,24)
# tail(test_set$out,24)
# 
# regressor = lm(formula = out ~ year+month+weekday+hour+hour_sqr,
#                data = training_set)

dataset.out = StationX.demand.detail[c(1:5,6)]
dataset.out$hour_p2 = dataset.out$hour^2
dataset.out$hour_p3 = dataset.out$hour^3
dataset.out$hour_p4 = dataset.out$hour^4
dataset.out$hour_p5 = dataset.out$hour^5
dataset.out$hour_p6 = dataset.out$hour^6
dataset.out$hour_p7 = dataset.out$hour^7
dataset.out$hour_p8 = dataset.out$hour^8
dataset.out$hour_p9 = dataset.out$hour^9
dataset.out$hour_p10 = dataset.out$hour^10
dataset.out$hour_p11 = dataset.out$hour^11
dataset.out$hour_p12 = dataset.out$hour^12
dataset.out$hour_p13 = dataset.out$hour^13
dataset.out$hour_p14 = dataset.out$hour^14
dataset.out$hour_p15 = dataset.out$hour^15

weekend.position = dataset.out$weekday == 'Saturday' | dataset.out$weekday == 'Sunday'
dataset.out = dataset.out[c(1:3,5,7:20,6)]

dataset.out.weekday = subset(dataset.out, weekend.position == FALSE)
dataset.out.weekend = subset(dataset.out, weekend.position == TRUE)

dataset.out.weekday = subset(dataset.out.weekday, dataset.out.weekday$year == 2015)
# Fitting Multiple Linear Regression to the Training set
regressor.weekday = lm(formula = out ~ month+hour+hour_p2+hour_p3+hour_p4+hour_p5+hour_p6+hour_p7+hour_p8+hour_p9+hour_p10
                       +hour_p11+hour_p12+hour_p14,
                       data = dataset.out.weekday)
summary(regressor.weekday)
# regressor.weekday = lm(formula = out ~ .,
#                data = dataset.out.weekday)
# Predicting the Test set results
# y_pred = predict(regressor.weekday, newdata = tail(dataset.out.weekday,24))
# 
# x = c(1:24)
# plot(x,y_pred)
# plot(x,tail(dataset.out.weekday$out,24))
x_grid = seq(0, 23, 0.1)
ggplot() +
  geom_point(aes(x = tail(dataset.out.weekday$hour,1000), y = tail(dataset.out.weekday$out,1000)),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor.weekday,
                                        newdata = data.frame(#year = 2016,
                                                             month = 8,
                                                             hour = x_grid, 
                                                             hour_p2 = x_grid^2,
                                                             hour_p3 = x_grid^3, 
                                                             hour_p4 = x_grid^4,
                                                             hour_p5 = x_grid^5, 
                                                             hour_p6 = x_grid^6,
                                                             hour_p7 = x_grid^7, 
                                                             hour_p8 = x_grid^8,
                                                             hour_p9 = x_grid^9, 
                                                             hour_p10 = x_grid^10,
                                                             hour_p11 = x_grid^11, 
                                                             hour_p12 = x_grid^12,
                                                             # hour_p13 = x_grid^13, 
                                                             hour_p14 = x_grid^14
                                                             ))),
            colour = 'blue') +
  ggtitle('Weekday') +
  xlab('Hour') +
  ylab('Demand')

# Fitting Multiple Linear Regression to the Training set
regressor.weekend = lm(formula = out ~ year+month+hour+hour_p2+hour_p3+hour_p4,
                       data = dataset.out.weekend)
summary(regressor.weekend)
# regressor.weekday = lm(formula = out ~ .,
#                data = dataset.out.weekday)
# Predicting the Test set results
# y_pred1 = predict(regressor.weekend, newdata = tail(dataset.out.weekend,24))

# x = c(1:24)
# plot(x,y_pred1)
# plot(x,tail(dataset.out.weekend$out,24))

# x_grid = seq(0, 23, 0.1)
ggplot() +
  geom_point(aes(x = tail(dataset.out.weekend$hour,170), y = tail(dataset.out.weekend$out,170)),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor.weekend,
                                        newdata = data.frame(year = 2016,
                                                             month = 06,
                                                             hour = x_grid, 
                                                             hour_p2 = x_grid^2,
                                                             hour_p3 = x_grid^3,
                                                             hour_p4 = x_grid^4))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')
