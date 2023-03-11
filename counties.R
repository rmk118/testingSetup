#County Analysis
#RK 3/10/23

library(tidyverse)
library(rstatix)
library(MASS)
library(Rfit)
library(patchwork)
library(ggpubr)
library(ggpmisc)
library(mgcv)

#Input data
data<-read.csv("difficulty.csv")

data<- data %>%
  rename(
    numConditions = X..conditions.on.lease,
    numWitFor = Witnesses.testifying.on.behalf.of.applicant,
    witForScore = Witnesses.for.applicant.score,
    numWitOpp = X..of.unique.individuals.testifying.in.opposition,
    witOppScore = Witnesses.opposed.score,
    commentsOpp = Emails..letters..petitions..etc..received.in.opposition.,
    diffScore = Total.points,
    pound = Pound.,
    appType = App.type..most.recent.included.in.lease.decision.PDF.,
    leaseType = Lease.Type,
    areaMod = Alteration.of.lease.area,
    pageLen = App..length..pages.
  )

mean(data$Acreage) #mean acreage = 7.05

data<- data %>% 
  mutate(
    leaseType = as.factor(leaseType),
    appType = as.factor(appType),
    pound = as.factor(pound),
    Waterbody = as.factor(Waterbody))




# County analysis - not significant ---------------------------------------------------------

data$Town<-as.factor(data$Town)

York<-c("Eliot", "Wells")
Hancock<-c("Bar Harbor", "Blue Hill", "Brooksville", "Brooksville and Sedgwick", "Cranberry Isles", "Deer Isle", "Franklin", "Gouldsboro", "Hancock", "Lamoine", "Sorrento", "Swan's Island", "Trenton" )
Washington<-c("Beals", "Cutler", "Steuben")
Lincoln<-c("Boothbay", "Bremen", "Bristol and Damariscotta", "Bristol and S. Bristol", "South Bristol", "Damariscotta", "Damariscotta/Newcastle", "Edgecomb", "Newcastle", "Newcastle/Damariscotta", "Walpole")
Cumberland<-c("Brunswick", "Chebeague I. and Long I.", "Chebeague Island","Cumberland","Falmouth", "Freeport", "South Freeport", "Harpswell", "Scarborough", "Yarmouth")
Knox<-c("Cushing", "Friendship", "North Haven", "South Thomaston")
Sagadahoc<-c("Georgetown", "Phippsburg", "West Bath")

dataCounties<- data %>% 
  mutate(county=NA)

York<- dataCounties %>% 
  group_by(Town) %>% 
  filter(grepl("Eliot|Wells",Town)) %>% 
  mutate(county="York")

Hancock<- dataCounties %>% 
  group_by(Town) %>% 
  filter(grepl("Bar Harbor|Blue Hill|Brooksville|Brooksville and Sedgwick|Cranberry Isles|Deer Isle|Franklin|Gouldsboro|Hancock|Lamoine|Sorrento|Swan's Island|Trenton",Town)) %>% 
  mutate(county="Hancock")

Washington<- dataCounties %>% 
  group_by(Town) %>% 
  filter(grepl("Beals|Cutler|Steuben",Town)) %>% 
  mutate(county="Washington")

Lincoln<- dataCounties %>% 
  group_by(Town) %>% 
  filter(grepl("Boothbay|Bremen|Bristol and Damariscotta|Bristol and S. Bristol|South Bristol|Damariscotta|Damariscotta/Newcastle|Edgecomb|Newcastle|Newcastle/Damariscotta|Walpole", Town)) %>% 
  mutate(county="Lincoln")

Cumberland<- dataCounties %>% 
  group_by(Town) %>% 
  filter(grepl("Brunswick|Chebeague I. and Long I.|Chebeague Island|Cumberland|Falmouth|Freeport|South Freeport|Harpswell|Scarborough|Yarmouth",Town)) %>% 
  mutate(county="Cumberland")

Knox<- dataCounties %>% 
  group_by(Town) %>% 
  filter(grepl("Cushing|Friendship|North Haven|South Thomaston", Town)) %>% 
  mutate(county="Knox")

Sagadahoc<- dataCounties %>% 
  group_by(Town) %>% 
  filter(grepl("Georgetown|Phippsburg|West Bath", Town)) %>% 
  mutate(county="Sagadahoc")

dataCounties<-bind_rows(York, Hancock, Washington, Lincoln, Cumberland, Knox, Sagadahoc)

noOutlier<- dataCounties %>% filter(Acreage < 60)

noCV<- noOutlier %>% filter(pageLen < 80)

noCV2<- noOutlier %>% 
  mutate(pageLen=replace(pageLen, Site.ID=="EAST TB", 57))

scatterCounty<-ggplot(dataCounties, aes(x=Acreage, y=diffScore, color=county, fill=county))+ geom_point()+theme_classic()

ggplot(data = dataCounties) +
  geom_point(aes(x=Acreage, y=diffScore, color=county)) +
  theme_bw() +
  geom_smooth(aes(x=Acreage, y=diffScore, color=county), se=FALSE,
              method = "lm")

mod_lmCounty = gam(diffScore ~ s(Acreage) + s(pageLen) + county, data = dataCounties)
summary(mod_lmCounty)


mod_lmCounty3 = gam(diffScore ~ s(Acreage) + s(pageLen) + county, data = noOutlier)
summary(mod_lmCounty3)

mod_lmCounty3 = gam(diffScore ~ s(Acreage,pageLen) + county, data = noCV)
summary(mod_lmCounty3)

mod_lmCounty2 = gam(diffScore ~ s(Acreage) + s(pageLen) + county, data = dataCounties, method="REML")
summary(mod_lmCounty2)



mod_lmCounty4 = gam(diffScore ~ s(Acreage) + s(pageLen) + county, data = noOutlier, method="REML")
summary(mod_lmCounty4)


mod_lmCounty5 = gam(diffScore ~ s(Acreage) + s(pageLen) + county, data = noCV2)
summary(mod_lmCounty5)

gam.check(mod_lmCounty3)
concurvity(mod_lmCounty3)

plot(mod_lmCounty5, all.terms = TRUE, pages=1)
