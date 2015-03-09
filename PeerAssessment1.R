# lets read in the data
ActivityData<-read.csv("./activity.csv", stringsAsFactors=F)
# now load the ggplot2 library
require(ggplot2)
#create a count plot by day that becomes a data based histogram by summing each day's steps
qplot(date,steps, data=ActivityData, stat="summary", fun.y="sum", geom="bar",xlab="Date",ylab="Steps")
# now create a data fram by using aggregate to sum again by day
StepsSummed<-aggregate(steps~date,data=ActivityData,sum)
# lets determine the mean and median, then print them
StepsMean<-mean(StepsSummed$steps)
StepsMedian<-median(StepsSummed$steps)
StepsData<-cbind(StepsMean,StepsMedian)
print(StepsData)
StepsMax<-StepsSummed[which.max(StepsSummed$steps),2]
print(StepsMax)
# now lets aveage the steps by time interval by day (acepting defaults means NA is ignored)
qplot(interval,steps, data=ActivityData, stat="summary", fun.y="mean", geom="bar",xlab="Time of Day",ylab="Steps")
# not create the data frame of averaged values to report on and use later in imputing
StepsAveraged<-aggregate(steps~interval,data=ActivityData,mean)
# now find the time period that has the max by using which.max to find the right row in our data frame we created above
MaxStepsPeriod<-StepsAveraged[which.max(StepsAveraged[,2]),]
MaxStepsInterval<-MaxStepsPeriod[1,1]
print(MaxStepsInterval)
# now create a data frame equal to the original
ActivityDataImpute<-ActivityData
# now loop through the values and for every NA in steps replace it with the average for that time period from the data frame we created above (told you we would use it later)
for (i in 1:nrow(ActivityDataImpute)){
      if (is.na(ActivityDataImpute[i,1])==T){
            ActivityDataImpute[i,1]<-StepsAveraged[match(ActivityDataImpute[i,3],StepsAveraged[,1]),2]
      }
}
# now repeat the steps above that create the summary histogram and perform the calculations to determine and then print the mean and median values
require(ggplot2)
qplot(date,steps, data=ActivityDataImpute, stat="summary", fun.y="sum", geom="bar",xlab="Date",ylab="Steps")
StepsSummedImpute<-aggregate(steps~date,data=ActivityDataImpute,sum)
StepsMeanImpute<-mean(StepsSummedImpute$steps)
StepsMedianImpute<-median(StepsSummedImpute$steps)
StepsDataImpute<-cbind(StepsMeanImpute,StepsMedianImpute)
print(StepsDataImpute)
# now convert Interval to time series variable
ActivityDataImpute$intervalplot<-as.ts(ActivityDataImpute$interval)
#find the day of the week for each date
ActivityDataImpute$weekday<-weekdays(strptime(ActivityDataImpute$date,"%Y-%m-%d"))
# create an empty column
ActivityDataImpute$weekdayfactor<-NA
#populate the new column with the a 1 if the date is a weekend day or 0 if the date is a weekday
for (i in 1:nrow(ActivityDataImpute)){
      if (ActivityDataImpute[i,5]=="Sunday"|ActivityDataImpute[i,5]=="Saturday"){
            ActivityDataImpute[i,6]=1
      }
      else{
            ActivityDataImpute[i,6]=0
      }
}
#covert this to a factor variable
ActivityDataImpute$weekdayfactor<- factor(ActivityDataImpute$weekdayfactor, labels = c("Weekday", "Weekend"))
#create a plot using ggplot2 qplot function and use facets for weekday and weekend
qplot(intervalplot,steps, data=ActivityDataImpute, stat="summary", fun.y="mean", geom="line",facets=weekdayfactor~.,xlab="Time of Day",ylab="Steps")