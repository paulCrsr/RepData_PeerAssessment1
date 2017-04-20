# setwd("/Users/posborne/Coursera/ReproducibleResearch/RepData_PeerAssessment1")
# library(dplyr)
# 
# activity <- read.csv("activity.csv")
# 
# stepsByDay <- 
#     group_by(activity, date) %>%
#     summarise_each(funs(sum(., na.rm=TRUE)), -interval)
# 
# stepsByInterval <- 
#     group_by(activity, interval) %>%
#     summarise_each(funs(mean(., na.rm=TRUE)), -date)
# 
# tmp <- right_join(activity, stepsByInterval, by=c("interval"="interval"), suffix=c(".orig",".imputed"))
# tmp$imputedSteps <- ifelse(is.na(tmp$steps.orig), tmp$steps.imputed, tmp$steps.orig)
# # activity$imputedSteps <- ifelse(is.na(tmp$steps.orig), tmp$steps.imputed, tmp$steps.orig)
# 
# 
# activity2 <- activity
# tmp <- right_join(activity, stepsByInterval, by=c("interval"="interval"), suffix=c(".orig",".imputed"))
# activity2$imputedSteps <- ifelse(is.na(activity$steps), tmp$steps.imputed, tmp$steps.orig)
# 
# # activity2 <- 
# #     select(activity2, date, interval, imputedSteps) %>%
# #     mutate(steps = imputedSteps) %>%
# #     select(steps, date, interval)
# # 
# # stepsByDay2 <- 
# #     group_by(activity2, date) %>%
# #     summarise_each(funs(sum(., na.rm=TRUE)), -interval)

imputed <- right_join(activity, stepsByInterval, by=c("interval"="interval"), suffix=c(".orig",".imputed"))
imputed$steps <- ifelse(is.na(imputed$steps.orig), imputed$steps.imputed, imputed$steps.orig)
imputed <- select(imputed, date, interval, steps)

activity2 <- 
    right_join(activity, imputed, by=c("date"="date","interval"="interval","steps"="steps"), suffix=c(".orig",".imputed"))
    
    