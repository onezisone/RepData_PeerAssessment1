getwd()
setwd("D:/Users/Documents/Google Drive/Big Data/Notes/Course 5_Reproducible Research/Assignment1")

library(ggplot2)
library(knitr)
library(lubridate)
library(dplyr)
file<-"activity.csv"
#v1
MyData<-read.csv(file,header = TRUE, sep = ",",colClasses=c("numeric","Date","numeric"))
#v2
MyData<-read.csv(file,header = TRUE, sep = ",")

View(MyData)
str(MyData)

MyData<-group_by(MyData,date)
Sum_StepsDay <- summarise(MyData, steps = sum(steps))
Sum_StepsDay <-na.omit(Sum_StepsDay)

hist(Sum_StepsDay$steps, main = "Total number of steps taken per day", 
     xlab = "Steps", ylab = "Frequency"
     ,col="red")
