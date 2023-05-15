#Bremen Oyster Data Exploration
#5/15/23

library(tidyverse)
library(lubridate)
library(car)
library(ARTool)
library(tidyquant)
library(plotrix)
library(patchwork)



# Oyster Data -------------------------------------------------------------

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


# Summary graphs -----------------------------------------------------------------

#Height - all 6 treatments
dataSum<- data %>% 
  group_by(Location, Date, Gear, Treatment) %>% 
            summarise(sh.sd = sd(Height, na.rm = TRUE),
            sh = mean(Height, na.rm = TRUE))
ggplot(dataSum, aes(x=Date, y=sh, color=Location, linetype=Gear)) +geom_line()

#Height -in vs out
dataSum2<- data %>% 
  group_by(Location, Date) %>% 
  summarise(sh2.sd = sd(Height, na.rm = TRUE),
            sh2 = mean(Height, na.rm = TRUE))
ggplot(dataSum2, aes(x=Date, y=sh2, color=Location)) +geom_line()

#Height - by gear type
dataSum3<- data %>% 
  group_by(Gear, Date) %>% 
  summarise(sh3.sd = sd(Height, na.rm = TRUE),
            sh3 = mean(Height, na.rm = TRUE))
ggplot(dataSum3, aes(x=Date, y=sh3, color=Gear)) +geom_line()

#Cup ratio - all 6 treatments
dataSum4<- data %>% 
  group_by(Location, Date, Gear, Treatment) %>% 
  summarise(cr.sd = sd(Cup.ratio, na.rm = TRUE),
            cr = mean(Cup.ratio, na.rm = TRUE))
ggplot(dataSum4, aes(x=Date, y=cr, color=Location, linetype=Gear)) +geom_line()

#Cup ratio - in vs out
dataSum5<- data %>% 
  group_by(Location, Date) %>% 
  summarise(cr2.sd = sd(Cup.ratio, na.rm = TRUE),
            cr2 = mean(Cup.ratio, na.rm = TRUE))
ggplot(dataSum5, aes(x=Date, y=cr2, color=Location)) +geom_line()

#Cup ratio - by gear type
dataSum6<- data %>% 
  group_by(Gear, Date) %>% 
  summarise(cr3.sd = sd(Cup.ratio, na.rm = TRUE),
            cr3 = mean(Cup.ratio, na.rm = TRUE))

ggplot(dataSum6, aes(x=Date, y=cr3, color=Gear)) +geom_line()


# Environmental data ---------------------------------------------------------------
hobo<-read.csv("hobo.csv")
hobo$Date.time<-mdy_hms(hobo$Date.time)

#all temps
ggplot(hobo, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+ylab("Temperature (°C)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))

tempRolling<-ggplot(hobo, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line(alpha=0)+geom_ma(n=24, linetype="solid")+ylab("Temperature (°C)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+scale_y_continuous(limits=c(12.5, 22.5))
tempRolling

noAir<- hobo[hobo$High.sal>5,]
ggplot(noAir, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+ylab("Temperature (°C)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))

#all sal - super weird
salGraph<-ggplot(hobo, aes(x=Date.time, y=High.sal, group=Location, color=Location))+ geom_line()+ylab("Salinity")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))

# Turbidity ---------------------------------------------------------------
turbidity<-read.csv("turbidity.csv")
turbidity$Date<-mdy(turbidity$Date)
turbidity$Location<-as.factor(turbidity$Location)

turbiditySummary<- turbidity %>% 
  group_by(Location, Date) %>% 
  summarise(se = std.error(Turbidity, na.rm = TRUE),
            meanTurbidity = mean(Turbidity, na.rm = TRUE))
turbiditySummary

turbidityGraph<-ggplot(turbiditySummary, aes(x=Date, y=meanTurbidity, group=Location, color=Location))+ geom_line()+ylab("Turbidity")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=meanTurbidity+se, ymax=meanTurbidity-se), width=.2,position=position_dodge(0.05))

# ChlA --------------------------------------------------------------------
ChlaDatasheet<-read.csv("chlA.csv")

ChlaDatasheet<- ChlaDatasheet %>% 
  filter(Major_issue == FALSE) %>% 
  mutate(Date = mdy(Trial_Date)) %>% 
  select(Location, Acetone_vol, Tube_Num, Vol_Filtered, Fo, Fa, Fo.Fa, Date) %>% 
  mutate(Location = as.factor(Location))

ChlFs = 0.000482
FoFa_max = 1.7718

ChlaDatasheet = ChlaDatasheet %>%
  mutate(Ave_Chl1 = (ChlFs*(FoFa_max/(FoFa_max-1))* 
                       (ChlaDatasheet$Fo-ChlaDatasheet$Fa)*
                       (((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))

ChlaDatasheet = ChlaDatasheet %>%
  mutate(ChlaDatasheet, Ave_Phaeo1 = ((ChlFs*(FoFa_max/(FoFa_max-1)))*
                                    ((FoFa_max-1)*(ChlaDatasheet$Fo-ChlaDatasheet$Fa))*(((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))

ChlaDatasheet <- ChlaDatasheet %>% 
  group_by(Date, Location) %>% 
  summarise(mean = mean(Ave_Chl1),
            se = std.error(Ave_Chl1))

chlaGraph<-ggplot(ChlaDatasheet, aes(x=Date, y=mean, group=Location, color=Location)) + geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+ theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=mean+se, ymax=mean-se), width=.2,position=position_dodge(0.05))


# Accel -------------------------------------------------------------------
#Inner Bag 9/12 to 10/06
FBiData<- read.csv("Inner_FB_Accel_9.12.22to10.6.22.csv",header=TRUE)
FBiData<- FBiData %>% 
  mutate(Datetime=ISO.8601.Time, accelX=Ax..g., accelY=Ay..g., accelZ=Az..g.) %>% 
  select(Datetime, accelX, accelY, accelZ)

#Inner Cage 9/12 to 10/06
FCiData<- read.csv("Inner_Cage_Accel_9.12.22to10.6.22.csv",header=TRUE)
FCiData<- FCiData %>% 
  mutate(Datetime=ISO.8601.Time, accelX=Ax..g., accelY=Ay..g., accelZ=Az..g.) %>% 
  select(Datetime, accelX, accelY, accelZ)

#Outer Cage 9/12 to 10/06
FCoData<- read.csv("Outer_Cage_Accel_9.12.22to10.6.22.csv",header=TRUE)
FCoData<- FCoData %>% 
  mutate(Datetime=ISO.8601.Time, accelX=Ax..g., accelY=Ay..g., accelZ=Az..g.) %>% 
  select(Datetime, accelX, accelY, accelZ)

#format time as a POSICXct 
FBiData$Datetime<-ymd_hms(FBiData$Datetime)
FCiData$Datetime<-ymd_hms(FCiData$Datetime)
FCoData$Datetime<-ymd_hms(FCoData$Datetime)

FBiData = FBiData %>%
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(FBiData)

FCiData = FCiData %>%
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(FCiData)

FCoData = FCoData %>%
  mutate(
    diffX = accelX - lag(accelX),
    diffY = accelY - lag(accelY),
    diffZ = accelZ - lag(accelZ),
    motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
str(FCoData)

# acceleration plot FB all
FBiPlotAll<-ggplot(data=FBiData, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "", y = "Motion")
FBiPlotAll

# acceleration plot FCi all
FCiPlotAll<-ggplot(data=FCiData, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "", y = "Motion")
FCiPlotAll

# acceleration plot FCo all
FCoPlotAll<-ggplot(data=FCoData, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "", y = "Motion")
FCoPlotAll

combinedAccel<-(FBiPlotAll+ggtitle("Inside bags")) + 
               (FCiPlotAll+ggtitle("Inside cages")) + 
               (FCoPlotAll+ggtitle("Outside cages")) + 
                plot_layout(ncol=1, guides = "collect")

# all environmental graphs ------------------------------------------------
allGraphs<-(tempRolling+ guides(color="none")+ (turbidityGraph+ guides(color="none"))) / (salGraph + (chlaGraph + guides(color="none"))& theme(legend.position = "bottom"))| combinedAccel
