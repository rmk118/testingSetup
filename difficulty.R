#Difficulty Scoring Analysis
#RK 1/15/23

library(tidyverse)
library(rstatix)
library(MASS)
library(Rfit)
library(patchwork)
library(ggpubr)

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
    areaMod = Alteration.of.lease.area
  )

mean(data$Acreage) #mean acreage = 7.05

data<- data %>% 
  mutate(
    leaseType = as.factor(leaseType),
    appType = as.factor(appType),
    pound = as.factor(pound))

#Subsets of data
Pounds <- data %>% filter(pound == "Yes")
noPound <- data %>% filter(pound == "No")
noOutlier<- data %>% filter(Acreage < 60)

mean(Pounds$diffScore) #0.4
sd(Pounds$diffScore) #0.55
mean(noPound$diffScore) #1.98
sd(noPound$diffScore) #1.84

opposed <- noPound %>% filter(numWitOpp > 0)
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

noPound %>% group_by(leaseType) %>%
  get_summary_stats(diffScore, type="common")

stat.test <- data %>% 
  wilcox_test(diffScore ~ pound, alternative = "greater") %>%
  add_significance()
stat.test

wilcox.test(Pounds$diffScore, noPound$diffScore, alternative = "less")
wilcox.test(data=data, diffScore ~ pound, alternative = "greater")


test<-lm(formula = diffScore ~ Acreage * App..length..pages., data = noOutlier)     
summary(test)

scatterAll<-ggplot(data, aes(x=Acreage, y=diffScore))+ geom_point()+geom_smooth(method="rlm")+theme_classic()+labs(x="Acreage", y="Difficulty score")+
  
library(ggpmisc)
ggplot(data, aes(x=Acreage, y=diffScore))+ geom_point()+geom_smooth(method="rlm")+theme_classic()+labs(x="Acreage", y="Difficulty score")+
  

scatterNoOutlier<-ggplot(noOutlier, aes(x=Acreage, y=diffScore))+ geom_point()+geom_smooth(method="rlm")+theme_classic()+labs(x="Acreage", y="Difficulty score")+
  stat_regline_equation(label.y = 10, aes(label = ..eq.label..))+
                         stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")))


scatterSmall<-ggplot(dataSmaller, aes(x=Acreage, y=diffScore))+ geom_point()+geom_smooth(method="rlm")+theme_classic()+labs(x="Acreage", y="Difficulty score")+
  stat_regline_equation(label.y = 10, aes(label = ..eq.label..))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "*`,`~")))


scatterAll + scatterNoOutlier + plot_annotation(tag_levels = 'A')

test2<-lm(formula = diffScore ~ Acreage, data = noOutlier)     
summary(test2)

dataSmall <- data %>% filter(Acreage <17.3)
wilcox.test(data=dataSmall, diffScore ~ pound, alternative = "greater")

dataSmaller <- data %>% filter(Acreage <4.16)
wilcox.test(data=dataSmaller, diffScore ~ pound, alternative = "greater")



# Graphs ------------------------------------------------------------------

ggplot(data, aes(x=diffScore))+geom_histogram(binwidth = 1)+theme_classic()
ggplot(data, aes(x=Acreage, fill=pound, color=pound))+geom_histogram()

#Difficulty Score dotplot
ggplot(data, aes(x=diffScore, fill=pound, color=pound))+geom_dotplot(binwidth = 0.25, stackratio = 1.3, dotsize = 1.25)+theme_classic()+theme(axis.title.y = element_blank())+labs(x="Difficulty Score", y="")+scale_y_continuous(breaks = NULL)+scale_fill_grey(start=0.7, end=0.1)+scale_color_grey(start=0.7, end=0.1)+ guides(fill=guide_legend("Pound?"), color="none")

#Acreage dotplot
ggplot(data, aes(x=Acreage, fill=pound, color=pound))+geom_dotplot(dotsize = 0.8, stackratio = 1.3)+theme_classic()+theme(axis.title.y = element_blank())+labs(x="Acreage", y="")+scale_y_continuous(breaks = NULL)+scale_fill_grey(start=0.7, end=0.1)+scale_color_grey(start=0.7, end=0.1)+ guides(fill=guide_legend("Pound?"), color="none")


#diff score histogram dodged
ggplot(data, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 0.5, position = "dodge")+theme_classic()+labs(x="Difficulty score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,12,1))+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))

#diff score histogram dodged- only small pounds
ggplot(dataSmall, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 0.5, position = "dodge")+theme_classic()+labs(x="Difficulty score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,12,1))+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))


#diff score histogram all
allPlot<-ggplot(data, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,12,1))+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))


#diff score histogram only smaller leases
smallPlot<-ggplot(dataSmall, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty Score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,12,1))+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))

#diff score histogram only smaller leases
smallerPlot<-ggplot(dataSmaller, aes(x=diffScore, fill=pound))+geom_histogram(binwidth = 1)+theme_classic()+labs(x="Difficulty Score", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,12,1))+scale_fill_grey(start=0.7, end=0.1)+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))

#Acreage histogram no outlier
noOutlierAcreae<-ggplot(noOutlier, aes(x=Acreage, fill=pound))+geom_histogram(binwidth = 1, alpha = 0.5, position = "identity")+theme_classic()+labs(x="Acreage", y="Number of leases")+guides(fill=guide_legend("Pound?"), color="none")+scale_x_continuous(breaks=seq(0,90,10))+theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=12))

allPlot + smallPlot + smallerPlot +plot_layout(ncol=3,guides = 'collect') & theme(legend.position = "right")

#fit ordinary least squares regression model
ols <- lm(diffScore~Acreage, data=data)

par(mfrow = c(2, 2))
plot(ols)
summary(ols)


#fit robust regression model
robust <- rlm(diffScore~Acreage, data=data, psi = psi.bisquare)
robust
summary(robust)


model.r = rfit(diffScore ~ Acreage, data = data)
summary(model.r)


model.r2 = rfit(diffScore ~ Acreage, data = noOutlier)
summary(model.r2, overall.test = "Wald")

