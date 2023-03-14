#County Analysis
#RK 3/13/23

library(tidyverse)
library(rstatix)
library(MASS)
library(Rfit)
library(patchwork)
library(ggpubr)
library(ggpmisc)
library(mgcv)
library(visreg)

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
  mutate(county = as.factor(county))

# noCV2<- noCV %>% 
#   mutate(pageLen=replace(pageLen, Site.ID=="EAST TB", 57))

#Scatterplot color-coded by county
scatterCounty<-ggplot(dataCounties, aes(x=Acreage, y=diffScore, color=county, fill=county))+ geom_point()+theme_classic()

#Scatterplot color-coded by county with trendlines
ggplot(data = dataCounties) +
  geom_point(aes(x=Acreage, y=diffScore, color=county)) +
  theme_bw() +
  geom_smooth(aes(x=Acreage, y=diffScore, color=county), se=FALSE, method = "lm")


# All data GAMs - Acreage only ----------------------------------------------------------------

hist(noOutlier$diffScore, breaks = 15)


sizeCounty1 = gam(diffScore ~ s(Acreage) + county, data = noOutlier)
summary(sizeCounty1)
gam.check(sizeCounty1)

size1 = gam(diffScore ~ s(Acreage), data = noOutlier, family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)")
summary(size1)
gam.check(size1)
plot(size1)
visreg(size1, scale="response")

MR_Temp = visreg(size1, "Acreage",
                 scale = "response",
                 partial = FALSE,
                 rug = FALSE,
                 line = list(col = "black"),
                 fill = list(fill ="lightblue"), gg=TRUE)+
  #scale_y_continuous(limits = c(-5.5, -0.5))+
  #scale_x_continuous(limits = c(0,18), breaks = c(0, 4, 8, 12, 16))+
  labs(x = "Acreage", y = "Effect on difficulty score")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        text = element_text(size=10, color = "black"),
        axis.text = element_text(size = 10, color = "black"))

lmSize = lm(diffScore ~ Acreage, data = noOutlier)
model_lmSize<-summary(lmSize)

AIC(sizeCounty1, size1, lmSize) #size1 and lmSize have the same AIC, lower than when county included

# GAMs that incorporate application length -----------------------------------------------

# # noChange excludes lease transfers, renewals, expansions, and changes in gear and/or species authorization. This is a subset of 48 leases from the whole sample of 101 - explanation in updated methods/results doc
# noChange <- noCV %>% 
#   filter(appType == "Standard aquaculture lease application" | appType == "Experimental aquaculture lease application")
# #top row in table 5 - full model
# len1 = gam(diffScore ~ s(Acreage) + s(pageLen) + county, data = noChange)
# summary(len1)
# gam.check(len1)
# concurvity(len1)
# plot(len1, all.terms = TRUE, pages=1)
# 
# #second row in table 5 - interaction plus county
# len2 = gam(diffScore ~ s(Acreage,pageLen) + county, data = noChange)
# summary(len2)
# 
# #third row in table 5 - interaction, no county
# len3 = gam(diffScore ~ s(Acreage,pageLen), data = noChange)
# summary(len3)
# 
# #fourth row in table 5 - additive only, no county (best fit)
# len4 = gam(diffScore ~ s(Acreage) + s(pageLen), data = noChange)
# summary(len4)
# gam.check(len4)
# concurvity(len4)
# plot(len4, all.terms = TRUE, pages=1)
# 
# #last row in table 5 - app length only
# len5 = gam(diffScore ~ s(pageLen), data = noChange)
# summary(len5)
# 
# AIC(len1, len2, len3, len4, len5) #len4 has lowest AIC

# noChange2 <- noCV %>% 
#   filter(appType == "Standard aquaculture lease application" | appType == "Experimental aquaculture lease application"| appType == "Aquaculture lease renewal application")

# # Including waterbody -----------------------------------------------------
# even if these technically work, I think too many waterbodies to make this good stats

# mod_1 = gam(diffScore ~ s(Acreage, pageLen) + Waterbody, data = noOutlier)
# summary(mod_1) #R-sq.(adj) =  0.658   Deviance explained = 79.9%
# concurvity(mod_1)
# gam.check(mod_1)
# 
# mod_2 = gam(diffScore ~ s(Acreage) + s(pageLen) + Waterbody, data = noOutlier)
# summary(mod_2)
# gam.check(mod_2)
# gamtabs(mod_2, type="HTML")
# plot(mod_2, pages=1)
# 
# mod_3 = gam(diffScore ~ s(Acreage) + Waterbody, data = noOutlier)
# summary(mod_3)
# plot(mod_3, all.terms = TRUE, pages=1)
# gam.check(mod_3)
# 
# AIC(mod_1, mod_2, mod_3)

# Scatterplots -----------------------------------------------------

#Figure 1 in new results section
ggplot(noOutlier, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+
  geom_smooth(method = "lm")+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))

ggplot(noCV, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_smooth(method="gam")

ggplot(noCV, aes(x=pageLen, y=diffScore))+ geom_point()+theme_classic()+labs(x="App length (pages)", y="Difficulty score")+geom_smooth(method="gam")
