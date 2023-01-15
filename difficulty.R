#Difficulty Scoring Analysis
#RK 1/15/23

library(tidyverse)
library(rstatix)
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
    appType = App.type..most.recent.included.in.lease.decision.PDF.
  )

mean(data$Acreage) #mean acreage = 7.05

data<- data %>% 
  mutate(
    Lease.Type = as.factor(Lease.Type),
    appType = as.factor(appType),
    pound = as.factor(pound))


ggplot(data, aes(x=diffScore))+geom_histogram(binwidth = 1)+theme_classic()

ggplot(data, aes(x=diffScore, fill=pound, color=pound))+geom_dotplot(binwidth = 0.25, stackratio = 1.3, dotsize = 1.25)+theme_classic()+theme(axis.title.y = element_blank())+labs(x="Difficulty Score", y="")+scale_y_continuous(breaks = NULL)+scale_fill_grey(start=0.7, end=0.1)+scale_color_grey(start=0.7, end=0.1)+ guides(fill=guide_legend("Pound?"), color=FALSE)

Pounds <- data %>% 
  filter(pound == "Yes")

noPound <- data %>% 
  filter(pound == "No")

mean(Pounds$diffScore) #0.4
sd(Pounds$diffScore) #0.55
mean(noPound$diffScore) #1.98
sd(noPound$diffScore) #1.84

#Summary statistics
data %>%
  group_by(pound) %>%
  get_summary_stats(diffScore, type = "common")

stat.test <- data %>% 
  wilcox_test(diffScore ~ pound, alternative = "greater") %>%
  add_significance()
stat.test


wilcox.test(Pounds$diffScore, noPound$diffScore, alternative = "less")
