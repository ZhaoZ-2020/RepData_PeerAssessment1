library(dplyr)
library(ggplot2)

## Read in the Data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "activity.zip")
unzip("activity.zip")
data<-read.csv("activity.csv")

str(data)
head(data)

## Change the formate of the date variable
data<-mutate(data, date=as.Date(data$date, "%Y-%m-%d"))
str(data)


## Ignore rows with missing values of steps
data_nonmissing<-filter(data, !is.na(steps))
head(data_nonmissing)


###########################################
## Q1: total number of steps taken per day

steps_byD<-data_nonmissing %>% 
    group_by(date) %>%
    summarize(total.steps=sum(steps))

hist(steps_byD$total.steps, breaks=20, xlab="Number of steps", 
     main="Histogram of the total number of steps taken each day")
summary(steps_byD)



##############################################
## Q2: What is the average daily activity pattern?

steps_byIntv<-data_nonmissing %>% 
    group_by(interval) %>%
    summarize(avg.steps=mean(steps))
steps_byIntv

with(steps_byIntv, plot(interval, avg.steps , type="l", xlab="5-minute interval", ylab="Average number of steps"))
title(main="Time series plot of the average number of steps by 5-minute interval")

## interval 835 contains the max avg steps
filter(steps_byIntv,avg.steps==max(avg.steps))




#############################################
## Q3: Imputing missing values

## Checking the missing values
summary(data$steps)     #2304 missing rows
mean(is.na(data$steps)) #about 13%


## Impute the missing value using the average number of steps for that interval

n<-dim(data)[1]

for (i in 1:n) {
    if (is.na(data$steps[i])) {
        intv<-data$interval[i]
        data$steps[i]<-as.numeric(steps_byIntv[steps_byIntv$interval==intv,2])
    }
}

head(data)
summary(data$steps)
str(data)


steps_byD2<-data %>% 
    group_by(date) %>%
    summarize(total.steps=sum(steps))

hist(steps_byD2$total.steps, breaks=20, xlab="Number of steps", 
     main="Histogram of the total number of steps taken each day")
summary(steps_byD$total.steps)
summary(steps_byD2$total.steps)



## Put the two charts side by side
par(mfrow=c(2,1), mar=c(4,4,2,1))
hist(steps_byD$total.steps, breaks=30, ylim=c(0,15), xlab="Number of steps", 
     main="Ignore missing values")
hist(steps_byD2$total.steps, breaks=30, xlab="Number of steps", 
     main="After imputing missing values")

mtext("Ozone and Weather in New York City", outer=T)

#############################################
## Q4: Are there differences in activity patterns between weekdays and weekends?

weekday <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

data_bywkd<-data %>% 
    mutate(wkday=ifelse(weekdays(date) %in% weekday,1,2)) %>%
    mutate(wkday=factor(wkday,labels=c("Weekday","Weekend"))) %>%
    group_by_at(.vars=vars(one_of(c("interval","wkday")))) %>%
    summarize(avg.steps=mean(steps))

data_bywkd


g<-ggplot(data_bywkd, aes(interval, avg.steps))

g+geom_line(aes(color=wkday),show.legend = FALSE)+facet_grid(wkday~.)+
    labs(x="5-minute interval",y="Number of steps", 
         title="Average number of steps by 5-minute interval")
