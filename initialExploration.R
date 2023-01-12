# Initial Exploration
library(tidyverse)
library(lubridate)
library(car)
library(ARTool)

#Input data and convert to factors
data<-read.csv("through10_17.csv") %>% 
  mutate(
    Cage = as.factor(Cage),
    Bag = as.factor(Bag),
    Location = as.factor(Location),
    Gear = as.factor(Gear),
    Treatment = as.factor(Treatment),
    Tag.num = as.factor(Tag.num)
  )
#Fix date format
data$Date<-mdy(data$Date)
str(data)

#Select only October measurements
final<- filter(data, Date == "2022-10-17")

#Final heights
ggplot(data=final, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,95))+ylab("Shell height (mm)")

#Final cup ratio
ggplot(data=final, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio")+scale_y_continuous(limits=c(0,0.4))

#Final shell shape
ggplot(data=final, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape")


# summary -----------------------------------------------------------------



dataSum<- data %>% 
  group_by(Location, Date, Gear, Treatment) %>% 
            summarise(sh.sd = sd(Height, na.rm = TRUE),
            sh = mean(Height, na.rm = TRUE))

dataSum
ggplot(dataSum, aes(x=Date, y=sh, color=Location, linetype=Gear)) +geom_line()


dataSum2<- data %>% 
  group_by(Location, Date) %>% 
  summarise(sh2.sd = sd(Height, na.rm = TRUE),
            sh2 = mean(Height, na.rm = TRUE))

dataSum2
ggplot(dataSum2, aes(x=Date, y=sh2, color=Location)) +geom_line()



dataSum3<- data %>% 
  group_by(Gear, Date) %>% 
  summarise(sh3.sd = sd(Height, na.rm = TRUE),
            sh3 = mean(Height, na.rm = TRUE))

dataSum3

ggplot(dataSum3, aes(x=Date, y=sh3, color=Gear)) +geom_line()


dataSum4<- data %>% 
  group_by(Location, Date, Gear, Treatment) %>% 
  summarise(cr.sd = sd(Cup.ratio, na.rm = TRUE),
            cr = mean(Cup.ratio, na.rm = TRUE))

dataSum4
ggplot(dataSum4, aes(x=Date, y=cr, color=Location, linetype=Gear)) +geom_line()

dataSum5<- data %>% 
  group_by(Location, Date) %>% 
  summarise(cr2.sd = sd(Cup.ratio, na.rm = TRUE),
            cr2 = mean(Cup.ratio, na.rm = TRUE))

dataSum5
ggplot(dataSum5, aes(x=Date, y=cr2, color=Location)) +geom_line()



dataSum6<- data %>% 
  group_by(Gear, Date) %>% 
  summarise(cr3.sd = sd(Cup.ratio, na.rm = TRUE),
            cr3 = mean(Cup.ratio, na.rm = TRUE))

ggplot(dataSum6, aes(x=Date, y=cr3, color=Gear)) +geom_line()
