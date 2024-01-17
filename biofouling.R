#Analysis of biofouling data from expt conducted at Community Shellfish in Bremen, ME, in Summer 2022
#Ruby Krasnow
#Last modified: January 17, 2024

#Load packages
library(tidyverse)
library(tidytext)
library(patchwork)
library(plotrix)

# Type/abundance of hard biofouling ---------------------------------------------------
#Import data
types <- read_csv("./data/bremen_biofouling_type.csv", show_col_types = FALSE)

types <- types %>% 
  mutate(across(where(is.double), ~replace_na(.x,0))) %>% #replace NA values with 0
  mutate(date = mdy(date)) %>% #convert column to date format
  mutate(gear=as.factor(gear), trt=as.factor(trt), location=as.factor(location), slippers=slipper, .keep="unused") #convert to factors and rename column for consistency

##Turn missing rows into explicit 0s
grid <- expand_grid(date=unique(types$date), gear=levels(types$gear), location=levels(types$location), oyster_num=1:20)

types_filled <- right_join(types %>% group_by(date, trt, gear, location) %>% mutate(oyster_num = row_number()), grid) %>% arrange(date, gear, location, oyster_num) %>% mutate(across(where(is.numeric), ~replace_na(.x,0)))

types_long <- types_filled %>% select(-c(comments, trt)) %>% pivot_longer(cols=c(slippers, mussels, barnacles, tunicates), values_to = "num", names_to = "organism")

types_sum <- types_long %>% group_by(date, gear, location, organism) %>% summarise(total=sum(num))

#Preliminary visualization - num organisms/20 oysters
ggplot(data=types_sum)+
  geom_bar(aes(x=gear,y=total,fill=location), stat="identity",position="dodge")+
  facet_wrap(~str_to_title(organism))+
  theme_classic()+
  labs(x="Gear type", y="Num per 20 oysters", fill="Location")+
  theme(axis.title = element_text(size=15), axis.text = element_text(size=12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), strip.text = element_text(
          size = 13))+
  scale_fill_discrete(labels = c("Inside", "Outside"))

ggplot(data=types_sum)+
  geom_bar(aes(x=reorder(gear, total),y=total,color=organism, fill=organism), stat="identity", position="dodge")+
  facet_wrap(~location)

types_means <- types_long %>% 
  select(-trt) %>% 
  group_by(gear, location, organism) %>% 
  summarise(mean = mean(num), sd=sd(num), se=std.error(num))

ggplot(data=types_means)+
  geom_bar(aes(x=gear,y=mean,color=str_to_title(organism), fill=str_to_title(organism)), stat="identity", position="dodge", alpha=0.5)+
  geom_errorbar(aes(x=gear,ymin=mean-se,ymax=mean+se,color=str_to_title(organism)), position=position_dodge(0.9), width=0.25, show.legend = FALSE)+
  facet_wrap(~location, labeller = labeller(location=c("in"="Inside", "out"="Outside")))+
 # scale_color_viridis_d()+
 # scale_fill_viridis_d()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=13),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(x="Gear type", y="Mean num per oyster", fill=NULL, color=NULL)


# Fouling ratios ----------------------------------------------------------



                                                                                            