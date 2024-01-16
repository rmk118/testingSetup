#Bremen Oyster Data Statistical Tests
#Ruby Krasnow
#Last modified 6/25/23

library(tidyverse)
library(lubridate)
library(ggpubr)
library(rstatix)

# Salinity ----------------------------------------------------------------

#read data
hobo<-read.csv("hobo.csv")
hobo$Date.time<-mdy_hms(hobo$Date.time)

noAir<- hobo[hobo$High.sal>5,]

#Find hours when both sensors logged with no issues
Inside<-noAir[noAir$Location=="Inside",]
Outside<-noAir[noAir$Location=="Outside",]
InsideDates<-Inside$Date.time
OutsideDates<-Outside$Date.time
commonDates<-base::intersect(InsideDates, OutsideDates)
common<-noAir[(noAir$Date.time %in% commonDates),]

InsideCommon<-common[common$Location=='Inside',]
OutsideCommon<-common[common$Location=='Outside',]

common2<- left_join(InsideCommon, OutsideCommon, by="Date.time")

common2 <- common2 %>% 
  mutate(salIn = High.sal.x,
         salOut = High.sal.y,
         tempIn = Temp.x,
         tempOut = Temp.y,
         .keep = "unused") %>% 
  select(-c("Location.x", "Location.y")) 

common2<- common2 %>% 
  mutate(salDiffs = salIn - salOut)

ggplot(common2, aes(x=Date.time, y=salDiffs))+geom_line()
mean(common2$salDiffs)
mean(common2$salIn)

bxp <- ggboxplot(
  common, x = "Location", y = "High.sal", 
  ylab = "Salinity", xlab = "Locations", add = "jitter"
)
bxp

common %>%
  group_by(Location) %>%
  identify_outliers(High.sal)


common %>%
  group_by(Location) %>%
  shapiro_test(High.sal)

commonSalInside2 <- InsideCommon %>%
  select(Location, Date.time, High.sal) %>%
  mutate(daily_sal_avg= rollmean(High.sal, k = 24, fill = NA))

commonSalOutside2 <- OutsideCommon %>%
  select(Location, Date.time, High.sal) %>%
  mutate(out_sal_daily_avg= rollmean(High.sal, k = 24, fill = NA))

both_sal_rolling<-c(commonSalInside2$daily_sal_avg, commonSalOutside2$out_sal_daily_avg)
common$sal_rolling<-both_sal_rolling

avgIn <- commonSalInside2$daily_sal_avg
avgOut <- commonSalOutside2$out_sal_daily_avg
date <- commonSalInside2$Date.time
  
sal<- tibble(date, avgIn, avgOut)
sal<- sal %>% 
  mutate(avgSalDiff = avgIn - avgOut)

summer <- sal %>% 
  filter(date < "2022-09-01 01:00:00")

ggplot(sal, aes(x=date, y=avgSalDiff))+geom_line()
ggplot(summer, aes(x=date, y=avgSalDiff))+geom_line()

ggplot(common, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()
ggplot(commonSummer, aes(x=Date.time, y=sal_rolling, group=Location, color=Location))+geom_line()

commonSummer <- common %>% 
  filter(Date.time < "2022-09-01 01:00:00")

stat.test <- commonSummer  %>%
  sign_test(sal_rolling ~ Location, detailed = TRUE) %>%
  add_significance()
stat.test

stat.test2 <- common  %>%
  sign_test(sal_rolling ~ Location, detailed = TRUE) %>%
  add_significance()
stat.test2

commonSummer %>% 
  group_by(Location) %>% 
get_summary_stats(sal_rolling)

test<- summer %>%  filter (avgSalDiff > 0)
length(test$date)/1937
1442/1937

sign_test(common2, salDiffs ~ 1, mu=0)
test<- common2 %>%  filter (salDiffs > 0)
length(test$salDiffs)/2696

rm(list=ls())
