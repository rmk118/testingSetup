#Difficulty Scoring Analysis
#RK 3/24/23

library(tidyverse)
library(rstatix)
library(MASS)
library(Rfit)
library(patchwork)
library(ggpubr)
library(ggpmisc)
library(itsadug)
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

#Subsets of data
Pounds <- data %>% filter(pound == "Yes")
noPound <- data %>% filter(pound == "No")
noMussels<- data %>% filter(Acreage < 60)

onlyOysters<- data %>% 
  mutate(oysters=TRUE) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Moosabec Mussels, Inc.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Cooke Aquaculture USA, Inc.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Wild Ocean Aquaculture, LLC.", FALSE)) %>% #they have 3 leases
  mutate(oysters = replace(oysters, Leaseholder == "Damariscove Seafood LLC.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "West, James and Springtide Seaweed, LLC", FALSE)) 

onlyOysters<- onlyOysters %>% 
  filter(oysters==TRUE)

# noCV<- noMussels %>% filter(pageLen < 80)
# noCV2<- noMussels %>% 
#   mutate(pageLen=replace(pageLen, Site.ID=="EAST TB", 57))

Pounds2 <- onlyOysters %>% filter(pound == "Yes")
noPound2 <- onlyOysters %>% filter(pound == "No")

mean(noPound2$diffScore) #2.02
sd(noPound2$diffScore) #1.84

opposedUL <- noPound %>% filter(numWitOpp > 0)
length(opposedUL$numWitOpp)

opposedUL2 <- noPound2 %>% filter(numWitOpp > 0)
length(opposedUL2$numWitOpp)

for2 <- noPound2 %>% filter(numWitFor > 0)
length(for2$numWitFor)

comments2 <- noPound2 %>% filter(commentsOpp > 0)
length(comments2$commentsOpp)

opposed <- data %>% filter(numWitOpp > 0)
length(opposed$numWitOpp)

area <- noPound %>% filter(areaMod > 0)
length(area$areaMod)

area2 <- noPound2 %>% filter(areaMod > 0)
length(area2$areaMod)

#Summary statistics
data %>% group_by(pound) %>%
  get_summary_stats(diffScore, type = "common")

data %>% group_by(pound) %>%
  get_summary_stats(numConditions, type="common")

onlyOysters %>% group_by(pound) %>%
  get_summary_stats(numConditions, type="common")

data %>% group_by(pound) %>%
  get_summary_stats(Acreage, type="common")

onlyOysters %>% group_by(pound) %>%
  get_summary_stats(Acreage, type="common")

data %>% group_by(pound) %>%
  get_summary_stats(numWitFor, type="common")

onlyOysters %>% group_by(pound) %>%
  get_summary_stats(numWitFor, type="common")

data %>% group_by(pound) %>%
  get_summary_stats(numWitOpp, type="common")

onlyOysters %>% group_by(pound) %>%
  get_summary_stats(numWitOpp, type="common")

noPound %>% group_by(leaseType) %>%
  get_summary_stats(diffScore, type="common")

stat.test <- data %>% 
  wilcox_test(diffScore ~ pound, alternative = "greater") %>%
  add_significance()
stat.test

stat.test2 <- onlyOysters %>% 
  wilcox_test(diffScore ~ pound, alternative = "greater") %>%
  add_significance()
stat.test2

wilcox.test(Pounds$diffScore, noPound$diffScore, alternative = "less")
wilcox.test(data=data, diffScore ~ pound, alternative = "greater")


dataSmall <- data %>% filter(Acreage <=17.3)
wilcox.test(data=dataSmall, diffScore ~ pound, alternative = "greater")

dataSmall2 <- onlyOysters %>% filter(Acreage <=17.3)
wilcox.test(data=dataSmall2, diffScore ~ pound, alternative = "greater")

dataSmaller <- data %>% filter(Acreage <=4.15)
wilcox.test(data=dataSmaller, diffScore ~ pound, alternative = "greater")

dataSmaller2 <- onlyOysters %>% filter(Acreage <=4.15)
wilcox.test(data=dataSmaller2, diffScore ~ pound, alternative = "greater")

wilcox.test(data=data, diffScore ~ leaseType)
wilcox.test(data=onlyOysters, diffScore ~ leaseType)

data %>% group_by(leaseType) %>%
  get_summary_stats(numConditions, type="common")

data %>% group_by(appType) %>%
  get_summary_stats(numConditions, type="common")
noExp <- data %>% filter(appType != "Aquaculture lease expansion")
kruskal.test(data=noExp, diffScore ~ appType)

noExp2 <- onlyOysters %>% filter(appType != "Aquaculture lease expansion")
kruskal.test(data=noExp2, diffScore ~ appType)


# Dotplots and histograms ------------------------------------------------------------------

ggplot(data, aes(x=diffScore))+geom_histogram(binwidth = 1)+theme_classic()
#basic acreage histogram
ggplot(data, aes(x=Acreage, fill=pound, color=pound))+geom_histogram()

#Difficulty Score dotplot
ggplot(data, aes(x=diffScore, fill=pound, color=pound))+geom_dotplot(binwidth = 0.25, stackratio = 1.3, dotsize = 1.25)+theme_classic()+theme(axis.title.y = element_blank())+labs(x="Difficulty Score", y="")+scale_y_continuous(breaks = NULL)+scale_fill_grey(start=0.7, end=0.1)+scale_color_grey(start=0.7, end=0.1)+ guides(fill=guide_legend("Pound?"), color="none")

#Acreage dotplot
ggplot(data, aes(x=Acreage, fill=pound, color=pound))+geom_dotplot(dotsize = 0.8, stackratio = 1.3)+theme_classic()+theme(axis.title.y = element_blank())+labs(x="Acreage", y="")+scale_y_continuous(breaks = NULL)+scale_fill_grey(start=0.7, end=0.1)+scale_color_grey(start=0.7, end=0.1)+ guides(fill=guide_legend("Pound?"), color="none")

#diff score histogram dodged
ggplot(data, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 0.5, position = "dodge")+theme_classic()+labs(x="Difficulty score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,12,1))+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))

#diff score histogram dodged- only leases <17.3
ggplot(dataSmall, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 0.5, position = "dodge")+theme_classic()+labs(x="Difficulty score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,12,1))+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))

#diff score histogram all
allPlot<-ggplot(data, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))+scale_x_continuous(breaks=seq(0,12,1))

allPlot2<-ggplot(onlyOysters, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))+scale_x_continuous(breaks=seq(0,12,1))

#diff score histogram - only leases <17.3
smallPlot<-ggplot(dataSmall, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty Score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))+scale_x_continuous(breaks=seq(0,12,1), limits = c(-1,12))
smallPlot

#diff score histogram - only leases <17.3 only oysters
smallPlot2<-ggplot(dataSmall2, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty Score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))+scale_x_continuous(breaks=seq(0,12,1), limits = c(-1,12))
smallPlot2

smallPlot+smallPlot2

#diff score histogram - only leases <4.16
smallerPlot<-ggplot(dataSmaller, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty Score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+ylim(0,30)+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))+scale_x_continuous(breaks=c(0:12), limits = c(-1,12))
smallerPlot

#diff score histogram - only leases <4.16 only oysters
smallerPlot2<-ggplot(dataSmaller2, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty Score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+ylim(0,30)+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))+scale_x_continuous(breaks=c(0:12), limits = c(-1,12))
smallerPlot2

smallerPlot+smallerPlot2

#Acreage histogram no outlier
noMusselsAcreage<-ggplot(noMussels, aes(x=Acreage, fill=pound))+geom_histogram(binwidth = 1, alpha = 0.5, position = "identity")+theme_classic()+labs(x="Acreage", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,90,10))+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))

allPlot + smallPlot + smallerPlot +plot_layout(ncol=3,guides = 'collect')+ plot_annotation(tag_levels = 'A') & theme(legend.position = "right", text = element_text(size=14))

#Histogram figure in manuscript (original results section Fig. 2)
allPlot2 + smallPlot2 + smallerPlot2 +plot_layout(ncol=3,guides = 'collect')+ plot_annotation(tag_levels = 'A') & theme(legend.position = "right", text = element_text(size=14))


# Robust linear regression ------------------------------------------------
model.r = rfit(diffScore ~ Acreage, data = data)
modelSum<-summary(model.r)

model.r2 = rfit(diffScore ~ Acreage, data = noCV)
summary(model.r2)
modelSum2<-summary(model.r2)

model.r3 = rfit(diffScore ~ Acreage, data = noMussels)
summary(model.r3)
modelSum3<-summary(model.r3)


#Only leases <17.3
model.r3 = rfit(diffScore ~ Acreage, data = dataSmall)
summary(model.r3)
modelSum2<-summary(model.r3)

#Only leases <4.16
model.r4 = rfit(diffScore ~ Acreage, data = dataSmaller)
summary(model.r4)
modelSum2<-summary(model.r4)

model.r5 = rfit(diffScore ~ Acreage*pageLen, data = noCV)
summary(model.r5)

model.r6 = rfit(diffScore ~ Acreage + pageLen, data = noCV)
summary(model.r6)

# Scatterplots ------------------------------------------------
eqn <- sprintf(
  "italic(y) == %.3g + %.2g * italic(x)  * ',' ~~ italic(p) ~ '=' ~ %.3g",
  coef(model.r)[1],
  coef(model.r)[2],
  modelSum$coefficients[2,4]
)

eqn2 <- sprintf(
  "italic(y) == %.3g + %.2g * italic(x) * ',' ~~ italic(p) ~ '=' ~ %.3g",
  coef(model.r2)[1],
  coef(model.r2)[2],
  modelSum2$coefficients[2,4]
)


scatterAll<-ggplot(data, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r2$coefficients[1], slope=model.r2$coefficients[2])+
  annotate(
    "text",
    x = Inf, y = -Inf,
    label = eqn, parse = TRUE,
    hjust = 1.1, vjust = -1.1
  )
scatterAll

ggplot(data, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")

scatternoMussels<-ggplot(noMussels, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r2$coefficients[1], slope=model.r2$coefficients[2])+
  annotate(
    "text",
    x = Inf, y = -Inf,
    label = eqn2, parse = TRUE,
    hjust = 1.1, vjust = -1.1
  )
scatternoMussels

#FIGURE 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scatternoMussels<-ggplot(noMussels, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r2$coefficients[1], slope=model.r2$coefficients[2])+
  annotate(
    "text",
    x =15, y = 10,
    label = eqn2, parse = TRUE
  )
scatternoMussels+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Comparing with and without the 89 acre lease
scatterAll + scatternoMussels + plot_annotation(tag_levels = 'A') & theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))


# + stat_regline_equation(label.y = 10, aes(label = ..eq.label..))+
#                          stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")))

scatterSmall<-ggplot(dataSmall, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r3$coefficients[1], slope=model.r3$coefficients[2])

scatternoMusselsColored<-ggplot(noMussels, aes(x=Acreage, y=diffScore, color=leaseType))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r2$coefficients[1], slope=model.r2$coefficients[2])+
  annotate(
    "text",
    x =15, y = 10,
    label = eqn2, parse = TRUE
  )
scatternoMusselsColored+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))



