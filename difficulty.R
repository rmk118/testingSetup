#Difficulty Scoring Analysis
#RK 3/10/23

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
noOutlier<- data %>% filter(Acreage < 60)


noCV<- noOutlier %>% filter(pageLen < 80)
noCV2<- noOutlier %>% 
  mutate(pageLen=replace(pageLen, Site.ID=="EAST TB", 57))

mean(Pounds$diffScore) #0.4
sd(Pounds$diffScore) #0.55
mean(noPound$diffScore) #1.98
sd(noPound$diffScore) #1.84

opposedUL <- noPound %>% filter(numWitOpp > 0)
length(opposedUL$numWitOpp)

opposed <- data %>% filter(numWitOpp > 0)
length(opposed$numWitOpp)

area <- noPound %>% filter(areaMod > 0)
length(area$areaMod)

#Summary statistics
data %>% group_by(pound) %>%
  get_summary_stats(diffScore, type = "common")

data %>% group_by(pound) %>%
  get_summary_stats(numConditions, type="common")

data %>% group_by(pound) %>%
  get_summary_stats(Acreage, type="common")

data %>% group_by(pound) %>%
  get_summary_stats(numWitFor, type="common")

data %>% group_by(pound) %>%
  get_summary_stats(numWitOpp, type="common")

noPound %>% group_by(leaseType) %>%
  get_summary_stats(diffScore, type="common")

stat.test <- data %>% 
  wilcox_test(diffScore ~ pound, alternative = "greater") %>%
  add_significance()
stat.test

wilcox.test(Pounds$diffScore, noPound$diffScore, alternative = "less")
wilcox.test(data=data, diffScore ~ pound, alternative = "greater")


dataSmall <- data %>% filter(Acreage <=17.3)
wilcox.test(data=dataSmall, diffScore ~ pound, alternative = "greater")

dataSmaller <- data %>% filter(Acreage <=4.15)
wilcox.test(data=dataSmaller, diffScore ~ pound, alternative = "greater")


# Graphs ------------------------------------------------------------------

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

#diff score histogram - only leases <17.3
smallPlot<-ggplot(dataSmall, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty Score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))+scale_x_continuous(breaks=seq(0,12,1), limits = c(-1,12))
smallPlot

#diff score histogram - only leases <4.16
smallerPlot<-ggplot(dataSmaller, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty Score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+ylim(0,30)+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))+scale_x_continuous(breaks=c(0:12), limits = c(-1,12))
smallerPlot

#Acreage histogram no Outlier
noOutlierAcreage<-ggplot(noOutlier, aes(x=Acreage, fill=pound))+geom_histogram(binwidth = 1, alpha = 0.5, position = "identity")+theme_classic()+labs(x="Acreage", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,90,10))+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))

allPlot + smallPlot + smallerPlot +plot_layout(ncol=3,guides = 'collect')+ plot_annotation(tag_levels = 'A') & theme(legend.position = "right", text = element_text(size=14))




# Scatterplots/regressions ------------------------------------------------
model.r = rfit(diffScore ~ Acreage, data = data)
modelSum<-summary(model.r)

eqn <- sprintf(
  "italic(y) == %.3g + %.2g * italic(x)  * ',' ~~ italic(p) ~ '=' ~ %.3g",
  coef(model.r)[1],
  coef(model.r)[2],
 # summary(model.r)$R2,
  modelSum$coefficients[2,4]
)
#* ',' ~~ italic(r)^2 ~ '=' ~ %.2g

model.r2 = rfit(diffScore ~ Acreage, data = noCV)
summary(model.r2)
modelSum2<-summary(model.r2)

eqn2 <- sprintf(
  "italic(y) == %.3g + %.2g * italic(x) * ',' ~~ italic(p) ~ '=' ~ %.3g",
  coef(model.r2)[1],
  coef(model.r2)[2],
  #summary(model.r2)$R2,
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

scatterNoOutlier<-ggplot(noOutlier, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r2$coefficients[1], slope=model.r2$coefficients[2])+
  annotate(
    "text",
    x = Inf, y = -Inf,
    label = eqn2, parse = TRUE,
    hjust = 1.1, vjust = -1.1
  )
scatterNoOutlier

#FIGURE 1
scatterNoOutlier<-ggplot(noOutlier, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r2$coefficients[1], slope=model.r2$coefficients[2])+
  annotate(
    "text",
    x =15, y = 10,
    label = eqn2, parse = TRUE
  )
scatterNoOutlier+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))


scatterAll + scatterNoOutlier + plot_annotation(tag_levels = 'A') & theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))


# + stat_regline_equation(label.y = 10, aes(label = ..eq.label..))+
#                          stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")))

model.r3 = rfit(diffScore ~ Acreage, data = dataSmall)
summary(model.r3)
modelSum2<-summary(model.r3)

scatterSmall<-ggplot(dataSmall, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r3$coefficients[1], slope=model.r3$coefficients[2])

model.r4 = rfit(diffScore ~ Acreage, data = dataSmaller)
summary(model.r4)
modelSum2<-summary(model.r4)

scatterSmaller<-ggplot(dataSmaller, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r4$coefficients[1], slope=model.r4$coefficients[2])







mod_lm = gam(diffScore ~ s(Acreage), data = data)
summary(mod_lm)
plot(mod_lm)

mod_lm2 = gam(diffScore ~ s(Acreage), data = noCV)
summary(mod_lm2)
plot(mod_lm2)


mod_lm3 = gam(diffScore ~ s(Acreage), data = noCV)
summary(mod_lm3)
plot(mod_lm3)

mod_lm4 = gam(diffScore ~ s(Acreage, pageLen), data = noCV)
summary(mod_lm4)

mod_lm5 = gam(diffScore ~ s(Acreage) + s(pageLen) + Waterbody, data = noCV)
summary(mod_lm5)


mod_lm5 = gam(diffScore ~ s(Acreage) + s(pageLen), data = noCV2)
summary(mod_lm5)


ggplot(noCV, aes(x=Acreage, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_smooth(method="gam")

ggplot(noCV, aes(x=pageLen, y=diffScore))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_smooth(method="gam")

wilcox.test(data=data, diffScore ~ leaseType)

data %>% group_by(leaseType) %>%
  get_summary_stats(numConditions, type="common")

data %>% group_by(appType) %>%
  get_summary_stats(numConditions, type="common")
noExp <- data %>% filter(appType != "Aquaculture lease expansion")
kruskal.test(data=noExp, diffScore ~ appType)



scatterNoOutlierColored<-ggplot(noOutlier, aes(x=Acreage, y=diffScore, color=leaseType))+ geom_point()+theme_classic()+labs(x="Acreage", y="Difficulty score")+geom_abline(intercept=model.r2$coefficients[1], slope=model.r2$coefficients[2])+
  annotate(
    "text",
    x =15, y = 10,
    label = eqn2, parse = TRUE
  )
scatterNoOutlierColored+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))





# Including waterbody -----------------------------------------------------


mod_1 = gam(diffScore ~ s(Acreage, pageLen) + Waterbody, data = noOutlier)
summary(mod_lm6) #R-sq.(adj) =  0.658   Deviance explained = 79.9%
concurvity(mod_1)
gam.check(mod_1)

mod_2 = gam(diffScore ~ s(Acreage) + s(pageLen) + Waterbody, data = noCV)
summary(mod_2)
gam.check(mod_2)
gamtabs(mod_2, type="HTML")
plot(mod_2, pages=1)

mod_3 = gam(diffScore ~ s(Acreage) + Waterbody, data = noOutlier)
summary(mod_3)
plot(mod_3, all.terms = TRUE, pages=1)
gam.check(mod_3)