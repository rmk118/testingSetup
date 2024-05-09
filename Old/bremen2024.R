#Updated analysis of data from Bremen oyster experiment
#Last modified: May 8, 2024
#Ruby Krasnow

library(tidyverse)


# Temp and Salinity -------------------------------------------------------

hobo <- read.csv("hobo.csv") %>% 
  mutate(dt=mdy_hms(Date.time), temp=Temp, sal=High.sal,loc=Location,.keep="unused")

noAir <- hobo %>% filter(sal>5)

#Find hours when both sensors logged with no issues
Inside<- noAir %>% filter(loc=="Inside") %>% rename(sal_in = sal, temp_in=temp)
Outside<-noAir %>% filter(loc=="Outside")%>% rename(sal_out = sal, temp_out=temp)

common <- inner_join(Inside, Outside, by="dt") %>% 
  select(-c("loc.x", "loc.y")) %>% 
  mutate(sal_diff = sal_in - sal_out, temp_diff=temp_in-temp_out)

ggplot(common, aes(x=dt, y=sal_diff))+geom_line()
ggplot(noAir, aes(x=dt, y=sal, color=loc))+geom_line()
ggplot(hobo, aes(x=dt, y=sal, color=loc))+geom_line()+theme_bw()
ggboxplot(hobo, x = "loc", y = "sal", ylab = "Salinity", xlab = "Locations", add = "jitter")

ggplot(common, aes(x=dt, y=temp_diff))+geom_line()
ggplot(noAir, aes(x=dt, y=temp, color=loc))+geom_line()+theme_bw()
ggplot(hobo, aes(x=dt, y=temp, color=loc))+geom_line()+theme_bw()
ggboxplot(hobo, x = "loc", y = "temp", ylab = "Temp", xlab = "Locations", add = "jitter")

common_roll <- common %>%
  mutate(across(where(is.double), \(x) rollmean(x, k = 24, fill = NA))) %>% na.omit()

noAir_roll <- noAir %>% group_by(loc) %>% 
  mutate(across(where(is.double), \(x) rollmean(x, k = 24, fill = NA))) %>% na.omit()

ggplot(common_roll, aes(x=dt, y=sal_diff))+geom_line()
ggplot(noAir_roll, aes(x=dt, y=sal, color=loc))+geom_line()

ggplot(common_roll, aes(x=dt, y=temp_diff))+geom_line()
ggplot(noAir_roll, aes(x=dt, y=temp, color=loc))+geom_line()+theme_bw()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  labs(x=NULL,y="Temperature (°C)", color="Location")



# Turbidity -------------------------------------------------------

t <- read.csv("turbidity.csv") %>% 
  mutate(date=mdy(Date), loc=as.factor(Location),turbidity=Turbidity,.keep="unused")

t <- t %>% group_by(date, loc) %>% summarise(mean=mean(turbidity), sd=sd(turbidity))

ggplot(data=t)+
  geom_errorbar(aes(x=date, ymin=mean-sd, ymax=mean+sd, color=loc), width=4)+
  geom_line(aes(x=date, y=mean, color=loc))+
  geom_point(aes(x=date, y=mean, color=loc))+
  labs(x=NULL, y="Turbidity", color="Location")+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])

# Chl A -------------------------------------------------------

chlA <- read.csv("chlA.csv") %>% 
    mutate(date = mdy(Trial_Date), loc=as.factor(Location)) %>%
    select(loc, Acetone_vol, Tube_Num, Vol_Filtered, Fo, Fa, Fo.Fa, date)

ChlFs <- 0.000482
FoFa_max <- 1.7718

chl <- chlA %>%
  mutate(chl = ChlFs * (FoFa_max/(FoFa_max-1)) * (Fo-Fa)* ((Acetone_vol)/Vol_Filtered))

chl <- chl %>% 
  group_by(date, loc) %>%
  summarise(mean = mean(chl),sd = sd(chl))

ggplot(data=chl)+
  geom_errorbar(aes(x=date, ymin=mean-sd, ymax=mean+sd, color=loc), width=2)+
  geom_line(aes(x=date, y=mean, color=loc))+
  geom_point(aes(x=date, y=mean, color=loc))+
  labs(x=NULL, y="Chl A", color="Location")+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])

# SPM -------------------------------------------------------