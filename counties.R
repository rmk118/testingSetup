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




# County setup ---------------------------------------------------------

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

noCV<- noCV %>% 
  mutate(
    county = as.factor(county))
# 
# noCV2<- noCV %>% 
#   mutate(pageLen=replace(pageLen, Site.ID=="EAST TB", 57))

scatterCounty<-ggplot(dataCounties, aes(x=Acreage, y=diffScore, color=county, fill=county))+ geom_point()+theme_classic()

ggplot(data = dataCounties) +
  geom_point(aes(x=Acreage, y=diffScore, color=county)) +
  theme_bw() +
  geom_smooth(aes(x=Acreage, y=diffScore, color=county), se=FALSE,
              method = "lm")


# All data - acreage only ----------------------------------------------------------------

mod_lmCounty = gam(diffScore ~ s(Acreage) + county, data = noOutlier)
summary(mod_lmCounty)

mod_lmCounty2 = gam(diffScore ~ s(Acreage), data = noOutlier)
summary(mod_lmCounty2)
# 
# model.r = rfit(diffScore ~ Acreage, data = noOutlier)
# modelSum<-summary(model.r)

lm = lm(diffScore ~ Acreage, data = noOutlier)
model_lmSum<-summary(lm)

AIC(mod_lmCounty, mod_lmCounty2, lm)

ggplot(noOutlier, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+
geom_smooth(method = "lm")+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))



# No changes - plus pageLen -----------------------------------------------

noChange <- noCV %>% 
  filter(appType == "Standard aquaculture lease application" | appType == "Experimental aquaculture lease application")
  
mod_Change1 = gam(diffScore ~ s(Acreage) + s(pageLen) + county, data = noChange)
summary(mod_Change1)

mod_Change2 = gam(diffScore ~ s(Acreage) + s(pageLen), data = noChange)
summary(mod_Change2)

mod_Change3.1 = gam(diffScore ~ s(Acreage,pageLen) + county, data = noChange)
summary(mod_Change3.1)

mod_Change3 = gam(diffScore ~ s(Acreage,pageLen), data = noChange)
summary(mod_Change3)

mod_Change4 = gam(diffScore ~ s(pageLen), data = noChange)
summary(mod_Change4)

AIC(mod_Change1, mod_Change2, mod_Change3, mod_Change3.1, mod_Change4)

# noChange2 <- noCV %>% 
#   filter(appType == "Standard aquaculture lease application" | appType == "Experimental aquaculture lease application"| appType == "Aquaculture lease renewal application")


gam.check(mod_Change2)
concurvity(mod_Change2)

plot(mod_Change2, all.terms = TRUE, pages=1)


