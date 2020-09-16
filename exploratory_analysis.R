rm(list = ls())
#install.packages("lubridate")
library(lubridate)  # for "datetime"

library(ggplot2)
#install.packages("dplyr")
library(dplyr)  # use for manipulating a large datafram
#install.packages("stargazer")
library(stargazer)


getwd()
setwd("/Users/rociozhong/Library/Mobile Documents/com~apple~CloudDocs/STAT_425/STAT_425_HW/final")
train <- read.csv("train.csv", header = TRUE)
test<- read.csv("test.csv", header = TRUE)

########### section 2 ##############
dim(train)
names(train)
stargazer(train, title = "Descriptive statistics")
unique(train$holiday)
unique(train$season)
unique(train$workingday)
unique(train$weather)

## correlation among predictors ##
cor(train[, c(6:12)])
stargazer(cor(train[, c(6:12)]))

round(cor(train[, c(6:12)])[7, ], 3)




library(scales)

# Add some new features, such as hour of the day, time, day
train$hour  = hour(train$datetime)
train$times = as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$jitter_times = train$times + minutes(round(runif(nrow(train),min=0,max=59)))
train$day = wday(train$datetime, label=TRUE)
# In lubridate, the week starts with Sunday. 
# Using the next command, we have the week to begin on Monday (coded as 1) and end 
# on Sunday (coded as 7)
train$day = ifelse(wday(train$datetime)==1, 7, wday(train$datetime)-1)
train$day = as.factor(train$day)
levels(train$day)=c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun")

## for test data ##
test$hour <- hour(test$datetime)
test$day <- wday(test$datetime, label = TRUE)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
    }

## plot: temp, x - hour, y - rental count on workdays ##
x_axis = "jitter_times"
y_axis = "count"
color  = "temp" 

ptemp_work <- ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Temp (°C)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals on Workdays") +
  theme(plot.title=element_text(size=18))

## plot: temp, x - hour, y - rental count on holidays/weekends ##
ptemp_nonwork <- ggplot(train[train$workingday==0,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Temp (°C)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals on Weekends/Holidays") +
  theme(plot.title=element_text(size=18))

# You can change "color" to other numerical variables and repeat the above plots

## windspeed, x- hour of the day, y - rental in weekends/holidays ##
color  = "windspeed" 
pwind_nonwork <- ggplot(train[train$workingday==0,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Windspeed", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals on Weekends/Holidays") +
  theme(plot.title=element_text(size=18))

## windspeed, x- hour of the day, y - rental in workingday ##
pwind_work <- ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Windspeed", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals on Workdays") +
  theme(plot.title=element_text(size=18))

multiplot(ptemp_work, ptemp_nonwork, pwind_work, pwind_nonwork, cols = 2)


## humidity, hour of the day in workingday ##
color  = "humidity" 
ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Humidity", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals on Workdays") +
  theme(plot.title=element_text(size=18))


# Calculate the average count for each day/time, store in a dataframe
hourly = group_by(train, day, hour)
day_hour_counts = summarise(hourly, count=mean(count))
day_hour_counts[1:5,]

# plot heat mat with ggplot, hour and day, average count ##
phour_day <- ggplot(day_hour_counts, aes(x = hour, y = day)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name="Average Counts", low="white", high="blue") + 
  theme(axis.title.y = element_blank())


## seasonly and day, average count, heat map ##
seasonly <- group_by(train, day, season)
day_season_counts <- summarise(seasonly, count = mean(count))
day_season_counts[1:5, ]

pseason_day <- ggplot(day_season_counts, aes(x = season, y = day)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name="Average Counts", low="white", high="blue") + 
  theme(axis.title.y = element_blank())


## month and day, average count, heat map ##
monthly <- group_by(train, day, month(train$datetime))
colnames(monthly)[17] <- "month"
day_month_counts <- summarise(monthly, count = mean(count))
colnames(day_month_counts)[2] <- "month"


pmonth_day <- ggplot(day_month_counts, aes(x = month , y = day)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name="Average Counts", low="white", high="blue") + 
  theme(axis.title.y = element_blank()) + scale_x_discrete(limits = seq(1, 12, 1))


multiplot(phour_day, pmonth_day, pseason_day, cols = 2)

## barplot, weather condition and average count ##
df_weather <- train[, c(5, 12)]
count_weather <- df_weather %>% group_by(weather) %>% summarise_each(funs(mean)) 
 

bar_weather <- ggplot(count_weather, aes(x = factor(weather), y = count)) + 
  geom_bar(stat = "identity", position="dodge", fill="lightblue") + 
  labs(x = "weather condition", y = "average bike rental count")

## barplot, season and average count ##
df_season <- train[, c(2, 12)]
count_season <- df_season %>% group_by(season) %>% summarise_each(funs(mean)) 

library(scales)
bar_season <- ggplot(count_season, aes(x = factor(season), y = count)) + 
  geom_bar(stat = "identity", position="dodge", fill="lightblue") + 
  labs(x = "season", y = "average bike rental count") + 
  scale_x_discrete("season", labels = c("Spring", "Summer", "Fall", "Winter"))

## barplot, hour and average count ##
df_hour <- train[, c(12, 13)]
count_hour <- df_hour %>% group_by(hour) %>% summarise_each(funs(mean)) 

bar_hour <- ggplot(count_hour, aes(x = factor(hour), y = count)) + 
  geom_bar(stat = "identity", position="dodge", fill="lightblue") + 
  labs(x = "hour", y = "average bike rental count")

## barplot, day and average count ##

df_day <- train[, c(12, 16)]
count_day <- df_day %>% group_by(day) %>% summarise_each(funs(mean)) 

bar_day <- ggplot(count_day, aes(x = factor(day), y = count)) + 
  geom_bar(stat = "identity", position="dodge", fill="lightblue") + 
  labs(x = "day", y = "average bike rental count")



multiplot(bar_day, bar_hour, bar_season, bar_weather, cols = 2)
## boxplot: season, weather, day, holiday/nonholiday, workingday/nonworking days ##

par(mfrow=c(2,3))
boxplot(train$count ~ train$season, main = "season", outline = FALSE)
#boxplot(count ~ factor(season), data = train, outline = FALSE)

boxplot(count ~ weather, data = train, main = "weather condition", outline = FALSE)
boxplot(count ~ day, data = hourly, main = "day", outline = FALSE)
#boxplot_day <- ggplot(data = day_hour_counts, aes(x = factor(day), y = count)) + geom_boxplot() + geom_jitter()
boxplot(count ~ holiday, data = train, main = "holiday", outline = FALSE)
boxplot(count ~ workingday, data = train, main = "workingday", outline = FALSE)

## registered and non-registered users ##
data_reg <- train[, c(10, 11, 16)]

#install.packages("reshape2")
library(reshape2)
#install.packages("plyr")
library(plyr)
melted <- melt(data_reg, id.vars="day")
means <- ddply(melted, c("day", "variable"), summarise,
               mean=mean(value))

means.barplot <- qplot(x=day, y= mean , fill=variable,
                       data=means, geom="bar", stat="identity",
                       position="dodge", ylab = "average bike rental count")

## barplot, registered/nonregistered users, x - day, y- average rental ##
means.barplot


## registered and nonregistered, x- hour, y-rental ##

p1<- ggplot(hourly, aes(x = hour, y = casual)) + geom_point()
p2<- ggplot(hourly, aes(x = hour, y = registered)) + geom_point()

## registered and nonregistered, x - weekdays y -rental ##

p3<- ggplot(hourly, aes(x = day, y = casual)) + geom_point()
p4<- ggplot(hourly, aes(x = day, y = registered)) + geom_point()

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#


multiplot(p1, p2, p3, p4, cols=2)

########################################
## registered and nonregistered, working and nonworkingday table ##
df_work <- train[, c(4, 10, 11)]
work_user <- df_work %>% group_by(workingday) %>% summarise_each(funs(mean)) 
work_user

install.packages("xtable")
library(xtable)
options(xtable.floating = FALSE)

xtable(work_user)
## registered and nonregistered, holiday and nonholiday table ##
df_holiday <- train[, c(3, 10, 11)]
holiday_user <- df_holiday %>% group_by(holiday) %>% summarise_each(funs(mean)) 
holiday_user

## total count over time ##
library(scales)
train$datetime <- as.Date(train$datetime)
ggplot(train, aes(x = datetime, y = count)) + geom_line() + 
  scale_x_date(labels = date_format("%m-%Y")) 




## creat new variables ##
DataFeature = function(data) {
  old.features = c("holiday",
                   "season",
                   "workingday",
                   "weather",
                   "temp",
                   "atemp",
                   "humidity",
                   "windspeed");
  newdata = data[, old.features];
  newdata$weather = as.factor(newdata$weather)
  newdata$season = as.factor(newdata$season)
  levels(newdata$season)=c("Spring", "Summer", "Fall", "Winter")
  
 
  newdata$hour= as.factor(hour(data$datetime))
  newdata$year = as.factor(year(data$datetime))
  newdata$month = as.factor(month(data$datetime))
  

  newdata$wday = ifelse(wday(data$datetime)==1, 7, wday(data$datetime)-1)
  newdata$wday = as.factor(newdata$wday)
  levels(newdata$wday)=c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun")
  return(newdata)
}


train = read.csv("train.csv")
test = read.csv("test.csv")

Newtrain = DataFeature(train)
Newtrain$lcount = log(train$count + 1)
Newtrain$lcasual <- log(train$casual + 1)
Newtrain$lregistered <- log(train$registered + 1)
Newtest = DataFeature(test)



## creat a new variable -- workingday1 ##
ww <- c("Mon", "Tue", "Wed", "Thurs", "Fri")
Newtest$workingday1 <- as.numeric(Newtest$wday %in% ww)
Newtrain$workingday1 <- as.numeric(Newtrain$wday %in% ww)



############ linear regression to examine variables #############


fit_holiday <- lm(lcount ~ factor(holiday), data = Newtrain)
summary(fit_holiday)

fit_workday <- lm(lcount ~factor(workingday), data = Newtrain)
summary(fit_workday)

fit_weather <- lm(lcount ~ factor(weather), data = Newtrain)
summary(fit_weather)



fit_season <- lm(lcount ~  factor(season), data = Newtrain)
summary(fit_season)

xtable(fit_season)
##### merge hour, become 2 levels categorical variable #####
ggplot(hourly, aes(x = hour, y = count)) + geom_point()

rushhour <- c(7,8,17,18)
Newtrain$rush <- as.numeric(Newtrain$hour %in% rushhour)


## rushhour and nonrush hour ##
fit_rush <- lm(lcount ~factor(rush), data = Newtrain)
summary(fit_rush)


stargazer(fit_holiday, fit_workday, fit_weather, fit_season, fit_rush, 
          title="Linear Regression Model",
          dep.var.labels="Log(count + 1)", 
          omit.stat = c("ser", "adj.rsq"), no.space=TRUE)



### interaction terms ###
## weather and temperature ##
fit_season_temp <- lm(lcount ~ windspeed + humidity + 
                         factor(season)*temp, data = Newtrain)
summary(fit_season_temp)

fit_season_wind <- lm(lcount ~ temp + humidity + 
                        factor(season)*windspeed, data = Newtrain)

summary(fit_season_wind)

xtable(summary(fit_season_temp))

xtable(fit_season_wind)

## non-linear trend of numerical variables ##
p12 <- ggplot(train, aes(x = temp, y = count)) + geom_point()

fit_temp2 <- lm(lcount ~ temp + I(temp^2) + windspeed + humidity, data = Newtrain)
summary(fit_temp2)

p22 <- ggplot(train, aes(x = windspeed, y = count)) + geom_point()

fit_wind2 <- lm(lcount ~ windspeed + I(windspeed^2) + 
                  temp + humidity, data = Newtrain)
summary(fit_wind2)


p32 <- ggplot(train, aes(x = humidity, y = count)) + geom_point()

fit_humid2 <- lm(lcount ~ humidity + I(humidity^2) + windspeed + temp, data = Newtrain)
summary(fit_humid2)

multiplot(p12, p22, p32, cols = 2)

stargazer(fit_temp2, fit_wind2, fit_humid2, 
          title="Nonlinear Regression Model",
          dep.var.labels="Log(count + 1)", 
          omit.stat = c("ser", "adj.rsq"), no.space=TRUE)

