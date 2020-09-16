############### section 3 ##################
###### prediction models #######


## A simple model prediction ##

submission0 = data.frame(datetime=test$datetime, 
                         hour=Newtest$hour, 
                         wday=Newtest$wday)


for (i_year in unique(Newtest$year)){
  for (i_month in unique(Newtest$month)) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    trainSubset = filter(Newtrain, year==i_year, month==i_month)
    by_wday_hour = group_by(trainSubset, wday, hour)
    wday_hour_lcounts = summarise(by_wday_hour, lcounts=mean(lcount))
    
    testLocs = Newtest$year ==i_year & Newtest$month == i_month
    tmp = submission0[testLocs, ]
    tmp = inner_join(tmp, wday_hour_lcounts)
    submission0[testLocs, "count"] = exp(tmp$lcounts)-1
  }
}
submission0=submission0[, c(1, 4)]
write.csv(submission0, file = "Submission0_simple_model.csv", row.names=FALSE)



## predict with linear model ##
## cut data into 2 segments(based on workingday1): workingday and nonworkingday;
## and fit casual and registered respectively ##


newtrain_workday <- filter(Newtrain, workingday1 == 1)
newtrain_nonwork <- filter(Newtrain, workingday1 == 0)

newtest_workday <- filter(Newtest, workingday1 == 1)
newtest_nonwork <- filter(Newtest, workingday1 == 0)


train$workingday1 <- Newtrain$workingday1
train_workday <- filter(train, workingday1 == 1)
train_nonwork <- filter(train, workingday1 == 0)


test$workingday1 <- Newtest$workingday1
test_workday <- filter(test, workingday1 == 1)
test_nonwork <- filter(test, workingday1 == 0)


df_work1 <- train[, c(10, 11, 13)]
work_user1 <- df_work1 %>% group_by(workingday1) %>% summarise_each(funs(mean)) 
work_user1

xtable(work_user1)

## in data of workingday = 1 ##
submission1 = data.frame(datetime=test_workday$datetime, 
                         hour=newtest_workday$hour, 
                         wday=newtest_workday$wday)

for (i_year in unique(year(ymd_hms(test_workday$datetime)))) {
  for (i_month in unique(month(ymd_hms(test_workday$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs = year(ymd_hms(test_workday$datetime))==i_year & month(ymd_hms(test_workday$datetime))==i_month
    trainLocs = ymd_hms(train_workday$datetime) <= min(ymd_hms(test_workday$datetime[testLocs]))
    
    myfit = lm(lcasual ~ temp + humidity + windspeed + holiday, data=newtrain_workday[trainLocs,])
    mypred.count = exp(predict(myfit, newtest_workday[testLocs,]))-1
    mypred.count[mypred.count < 0] = 0; 
    submission1[testLocs, "casual"] = mypred.count
  }
}

#write.csv(submission1, file = "casualwork_simple_LS_model.csv", row.names=FALSE)


###### registered in workingdays data ##
submission2 = data.frame(datetime=test_workday$datetime, 
                         hour=newtest_workday$hour, 
                         wday=newtest_workday$wday)

for (i_year in unique(year(ymd_hms(test_workday$datetime)))) {
  for (i_month in unique(month(ymd_hms(test_workday$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs = year(ymd_hms(test_workday$datetime))==i_year & month(ymd_hms(test_workday$datetime))==i_month
    trainLocs = ymd_hms(train_workday$datetime) <= min(ymd_hms(test_workday$datetime[testLocs]))
    
    myfit = lm(lregistered ~ temp  + humidity + windspeed + holiday, data=newtrain_workday[trainLocs,])
    mypred.count = exp(predict(myfit, newtest_workday[testLocs,]))-1
    mypred.count[mypred.count < 0] = 0; 
    submission2[testLocs, "registered"] = mypred.count
  }
}

#write.csv(submission2, file = "registeredwork_simple_LS_model.csv", row.names=FALSE)




##### in data of nonworking days ##
submission3 = data.frame(datetime=test_nonwork$datetime, 
                         hour=newtest_nonwork$hour, 
                         wday=newtest_nonwork$wday)

for (i_year in unique(year(ymd_hms(test_nonwork$datetime)))) {
  for (i_month in unique(month(ymd_hms(test_nonwork$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs = year(ymd_hms(test_nonwork$datetime))==i_year & month(ymd_hms(test_nonwork$datetime))==i_month
    trainLocs = ymd_hms(train_nonwork$datetime) <= min(ymd_hms(test_nonwork$datetime[testLocs]))
    
    myfit = lm(lcasual ~ temp + humidity + windspeed + holiday, data=newtrain_nonwork[trainLocs,])
    mypred.count = exp(predict(myfit, newtest_nonwork[testLocs,]))-1
    mypred.count[mypred.count < 0] = 0; 
    submission3[testLocs, "casual"] = mypred.count
  }
}

#write.csv(submission3, file = "casual_nonwork_simple_LS_model.csv", row.names=FALSE)
ggplot(submission3, aes(x = hour, y = casual)) + geom_point()


submission4 = data.frame(datetime=test_nonwork$datetime, 
                         hour=newtest_nonwork$hour, 
                         wday=newtest_nonwork$wday)

for (i_year in unique(year(ymd_hms(test_nonwork$datetime)))) {
  for (i_month in unique(month(ymd_hms(test_nonwork$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs = year(ymd_hms(test_nonwork$datetime))==i_year & month(ymd_hms(test_nonwork$datetime))==i_month
    trainLocs = ymd_hms(train_nonwork$datetime) <= min(ymd_hms(test_nonwork$datetime[testLocs]))
    
    myfit = lm(lregistered ~ temp  + humidity + windspeed + holiday, data=newtrain_nonwork[trainLocs,])
    mypred.count = exp(predict(myfit, newtest_nonwork[testLocs,]))-1
    mypred.count[mypred.count < 0] = 0; 
    submission4[testLocs, "registered"] = mypred.count
  }
}

#write.csv(submission4, file = "registered_nonwork_simple_LS_model.csv", row.names=FALSE)
ggplot(submission4, aes(x = hour, y = registered)) + geom_point()


## workingday rental count ##
count_workday <- submission1$casual + submission2$registered
count_nonwork <- submission3$casual + submission4$registered

#### 
submission1$registered <- submission2$registered
submission1$count <- count_workday

submission3$registered <- submission4$registered
submission3$count <- count_nonwork


total_sub <- rbind(submission1, submission3)
submission_final <- arrange(total_sub, datetime)



ggplot(submission_final, aes(x = wday , y = count)) + geom_point()
ggplot(submission_final, aes(x = hour , y = count)) + geom_point()

submission_final <- submission_final[, c(1, 6)]
write.csv(submission_final, file = "FinalSubmission_simple_LS_model.csv", row.names=FALSE)

### check with prof ##

submission = data.frame(datetime=test$datetime, 
                        hour=Newtest$hour, 
                        wday=Newtest$wday)

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs = year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    trainLocs = ymd_hms(train$datetime) <= min(ymd_hms(test$datetime[testLocs]))
    
    myfit = lm(lcount ~ temp + atemp + humidity + windspeed + workingday, data=Newtrain[trainLocs,])
    mypred.count = exp(predict(myfit, Newtest[testLocs,]))-1
    mypred.count[mypred.count < 0] = 0; 
    submission[testLocs, "count"] = mypred.count
  }
}


ggplot(submission, aes(x = wday, y = count)) + geom_point()
submission=submission[, c(1, 4)]
write.csv(submission, file = "Submission1_simple_LS_model.csv", row.names=FALSE)

## randomforest prediction model ##
install.packages("randomForest")
library(randomForest)

set.seed(3)

DataFeature2 = function(data) {
  old.features = c("holiday",
                   "workingday",
                   "temp",
                   "atemp",
                   "humidity",
                   "windspeed");
  newdata = data[, old.features];
  #newdata$season = as.factor(newdata$season)
  #levels(newdata$season)=c("Spring", "Summer", "Fall", "Winter")
  
  
  newdata$hour= as.factor(hour(data$datetime))
  
  
  #newdata$wday = ifelse(wday(data$datetime)==1, 7, wday(data$datetime)-1)
  #newdata$wday = as.factor(newdata$wday)
  #levels(newdata$wday)=c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun")
  return(newdata)
}


submission5 <- data.frame(datetime=test$datetime,count = NA)

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs   <- year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    testSubset <- test[testLocs,]
    trainLocs  <- ymd_hms(train$datetime) <= min(ymd_hms(testSubset$datetime))
    
    myrf <- randomForest(DataFeature2(train[trainLocs,]), train[trainLocs,"count"], ntree=500)
    mypred.count = predict(myrf, DataFeature2(testSubset))
    submission5[testLocs, "count"] = mypred.count
  }
}

write.csv(submission5, file = "Submission_RandomForest_model.csv", row.names=FALSE)
