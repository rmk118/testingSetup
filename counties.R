#County Analysis
#RK 4/3/23

library(tidyverse)
library(rstatix)
library(MASS)
library(Rfit)
library(patchwork)
library(ggpubr)
library(ggpmisc)
library(mgcv)
library(visreg)
library(viridis)

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
    Town = as.factor(Town),
    Waterbody = as.factor(Waterbody))

# County setup ---------------------------------------------------------
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

AIC(sizeCounty1, size1, lmSize)


onlyOysters2<- data %>% 
  mutate(oysters=TRUE) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Moosabec Mussels, Inc.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Cooke Aquaculture USA, Inc.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Wild Ocean Aquaculture, LLC.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Damariscove Seafood LLC.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "West, James and Springtide Seaweed, LLC", FALSE))
  
onlyOysters<- onlyOysters2 %>% 
  filter(oysters==TRUE)

hist(onlyOysters$diffScore)

twGam = gam(diffScore ~ s(Acreage), data = onlyOysters, family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)")
summary(twGam)
gam.check(twGam)

visreg(twGam, scale="response")

gaussianGam = gam(diffScore ~ s(Acreage), data = onlyOysters)
summary(gaussianGam)
gam.check(gaussianGam)
visreg(gaussianGam, scale="response")

poissonGam = gam(diffScore ~ s(Acreage), data = onlyOysters, family = "poisson")
summary(poissonGam)
gam.check(poissonGam)

lmSize2 = lm(diffScore ~ Acreage, data = onlyOysters)
summary(lmSize2)
# plot(lmSize2)
hist(lmSize2$residuals)

tweedieGraph<-ggplot(onlyOysters, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_smooth(method="gam", method.args=list((family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)")))+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))

ggplot(onlyOysters, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_smooth(method="gam")

ggplot(onlyOysters, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_smooth(method="lm")

poissonGraph<-ggplot(onlyOysters, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_smooth(method="glm", method.args=list((family="poisson")))

tweedieGraph + poissonGraph

mean(onlyOysters$diffScore)
var(onlyOysters$diffScore)


glm1<-glm(diffScore ~ Acreage, data = onlyOysters, family = "poisson")
summary(glm1)
glm1$fitted.values
rstandard(glm1)
plot(glm1)


#worse than poisson, quasipoisson was also worse
# glm2<-glm.nb(diffScore ~ Acreage, data = onlyOysters)
# summary(glm2)

AER::dispersiontest(glm1)

mu<- predict(glm1, type="response")
z<-predict(glm1)+ (onlyOysters$diffScore-mu)/mu
plot(z ~ log(diffScore), onlyOysters, ylab="linearized response")

AIC(lmSize2, glm1, poissonGam, gaussianGam, twGam)

par(mfrow = c(2,2))
plot(glm1)

plot(residuals(glm1) ~ predict(glm1, type="response"), xlab=expression(hat(mu)), ylab="deviance residuals")
plot(residuals(glm1) ~ predict(glm1, type="link"), xlab=expression(hat(eta)), ylab="deviance residuals")
plot(residuals(glm1, type="response") ~ predict(glm1, type="link"), xlab=expression(hat(eta)), ylab="response residuals")

1 - pchisq(deviance(glm1), df.residual(glm1))

with(glm1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


ggplot(dataCounties, aes(x=county, y=diffScore)) + 
  geom_boxplot()

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
# ggplot(noOutlier, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+
#   geom_smooth(method = "lm")+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))
# 
# ggplot(noCV, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_smooth(method="gam")
# 
# ggplot(noCV, aes(x=pageLen, y=diffScore))+ geom_point()+theme_classic()+labs(x="App length (pages)", y="Difficulty score")+geom_smooth(method="gam")

# glm1$model$fitted <- predict(glm1, type = "response")

# ggplot2 Plot:
# ggplot(glm1$model) + 
#   geom_point(aes(Acreage, diffScore)) +
#   geom_line(aes(Acreage, fitted)) + 
#   labs(x = "\n acreage", y = "diffscore \n") +
#   theme(plot.title = element_text(hjust = 0.5),
#         axis.title.x = element_text(face="bold", colour="blue", size = 12),
#         axis.title.y = element_text(face="bold", colour="blue", size = 12))

onlyOysters3 <- onlyOysters2 %>% 
  filter(Acreage < 70)


# Fig1b<-
Fig1b<-ggplot(onlyOysters2, aes(x=Acreage, y=diffScore, group=oysters)) + 
  geom_point(aes(color=oysters)) +
  theme_classic()+
  labs(x="Acreage", y="Difficulty score", color="Cultivated species") +
  scale_color_manual(values=c('Red', "Black"), name = "Cultivated species", labels = c("Other", "American oyster")) +
  guides(color = guide_legend(reverse=TRUE)) +
  annotate("text", x = 38, y = 11, label = "A")+
  annotate("text", x = 7, y = 8, label = "B") +
  #annotate("text", x = 19, y = 8, label = "C")+
  theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14), legend.text = element_text(size=10), legend.title = element_text(size=12))

Fig1a<-
  tweedieGraph +
  annotate("text", x = 36, y = 11, label = "A")+
  annotate("text", x = 4, y = 8, label = "B")

Fig1a + Fig1b +plot_layout(ncol=2)+ plot_annotation(tag_levels = 'A') & theme(legend.position = "right", text = element_text(size=14))
  
 

