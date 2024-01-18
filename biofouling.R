#Analysis of biofouling data from expt conducted at Community Shellfish in Bremen, ME, in Summer 2022
#Ruby Krasnow
#Last modified: January 17, 2024

#Load packages
library(tidyverse)
library(tidytext)
library(patchwork)
library(plotrix)
library(rstatix)

# Type/abundance of hard biofouling ---------------------------------------------------
#Import data
types <- read_csv("./data/bremen_biofouling_type.csv", show_col_types = FALSE)

types <- types %>% 
  mutate(across(where(is.double), ~replace_na(.x,0))) %>% #replace NA values with 0
  mutate(date = mdy(date)) %>% #convert column to date format
  mutate(gear=as.factor(gear), trt=as.factor(trt), location=as.factor(location), slippers=slipper, .keep="unused") #convert to factors and rename column for consistency

##Turn missing rows into explicit 0s
grid <- expand_grid(date=unique(types$date), gear=levels(types$gear), location=levels(types$location), oyster_num=1:20)

types_filled <- right_join(types %>% group_by(date, trt, gear, location) %>% mutate(oyster_num = row_number()), grid) %>% 
  arrange(date, gear, location, oyster_num) %>% 
  mutate(across(where(is.numeric), ~replace_na(.x,0)))

types_long <- types_filled %>% select(-c(comments, trt)) %>% 
  pivot_longer(cols=c(slippers, mussels, barnacles, tunicates), values_to = "num", names_to = "organism")

types_sum <- types_long %>% group_by(date, gear, location, organism) %>% summarise(total=sum(num))

types_means <- types_long %>% 
  select(-trt) %>% 
  group_by(gear, location, organism) %>% 
  summarise(mean = mean(num), sd=sd(num), se=std.error(num))

#Preliminary visualizations
#Total number of each organism per 20 oysters
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

#Mean (±SE) number of each organism per oyster
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

#Import data
ratios_raw <- read_csv("./data/bremen_biofouling_ratios.csv", show_col_types = FALSE)

ratios <- ratios_raw %>% mutate(fouling_wt = dirty_wt_boat-clean_wt_boat,
                                whole_wet_wt = clean_wt_boat-weigh_boat,
                                fouling_ratio=fouling_wt*100/whole_wet_wt) %>% 
  filter(fouling_ratio>0) %>% 
  mutate(log_fouling=log(fouling_ratio),
         prop_fouling=fouling_wt/whole_wet_wt)

ratios2 <- ratios %>% filter(fouling_ratio<50)

ggplot(data=ratios %>% filter(fouling_ratio<50))+
  geom_boxplot(aes(x=gear, y=fouling_ratio, color=location))+
  facet_wrap(~date)

ggplot(data=ratios)+
  geom_boxplot(aes(x=gear, y=log_fouling, color=location))+
  facet_wrap(~date)

ggplot(data=ratios)+
  geom_boxplot(aes(x=gear, y=whole_wet_wt, color=location))+
  facet_wrap(~date)

library(lme4)
library(lmerTest)



# ratios_lm1 <- ratios %>% 
#   ungroup() %>% 
#   mutate(date=as.factor(date)) %>% 
#   lm(whole_wet_wt~gear*location, data=.)
# 
# 
# lme1<- ratios %>% 
#   ungroup() %>% 
#   mutate(date=as.factor(date), gear=as.factor(gear), location=as.factor(location)) %>% 
#   lme(fouling_ratio~gear*location, random=~1|date, data=.)
# summary(lme1)
# hist(residuals(lme1))

# glm_fit <-glm(data=ratios, fouling_ratio~gear*location, family=Gamma)
# glmer_fit <-glmer(data=ratios, fouling_ratio~gear*location+(1|date), family=Gamma)
# 
# summary(glmer_fit)
# summary(glm_fit)
# 
# plot(lmer_fit, which=2)
# plot(glmer_fit, which=2)
# shapiro.test(residuals(glmer_fit))
# shapiro.test(residuals(glm_fit))
# hist(residuals(glmer_fit))
# hist(residuals(glm_fit))

library(betareg)
beta_fit <- betareg(data=ratios %>% filter(fouling_ratio<50), prop_fouling~gear*location)
summary(beta_fit)
hist(residuals(beta_fit))
shapiro.test(residuals(beta_fit))

beta_fit2 <- betareg(data=ratios %>% filter(fouling_ratio<50), prop_fouling~gear*location|date)
summary(beta_fit2)
hist(residuals(beta_fit2))
shapiro.test(residuals(beta_fit2))

beta_fit3 <- betareg(data=ratios2, prop_fouling~gear*location*date|date)
summary(beta_fit3)
hist(residuals(beta_fit3))
shapiro.test(residuals(beta_fit3))
plot(beta_fit3, which = 1)

beta_formula <- list(
  no_date=prop_fouling~gear*location,
  all_combos=prop_fouling~gear*location*date,
  var_gear=prop_fouling~gear*location*date|gear,
  add_only=prop_fouling~gear+location+date|date,
  no_loc=prop_fouling~gear*date+gear:location+gear:location:date|gear,
  var_loc=prop_fouling~gear*location*date|location,
  var_date=prop_fouling~gear*location*date|date,
  var_both=prop_fouling~gear*location*date|location+gear)

#beta_formulas <- tibble(id=c("no_date", "all_combos", "var_gear", "var_loc", "var_both"), formula=beta_formula)

beta_models <- tibble(beta_formula, models = map(beta_formula, ~betareg(data=ratios2, formula=.x ))) %>% add_column(id=c("no_date", "all_combos", "var_gear","add_only", "no_loc","var_loc", "var_date","var_both"), .before=1)

beta_models <- beta_models %>% 
  mutate(tidy_model = map(models, tidy),
          AIC=map(models, AIC),
          resids = map(models, residuals)) %>% unnest(cols=c(AIC))

test <- beta_models %>% select(id, resids) %>% 
  mutate(resids = map(resids, as.numeric()),
         normal = map(resids, shapiro.test))

test %>% mutate(normal_p = map(normal, ~.x$p.value)) %>% unnest(cols=c(normal_p))

library(gglm)
gglm(data=beta_fit3)
ggplot(data=beta_fit3)+
  #stat_fitted_resid()
stat_resid_hist()

beta_models %>% mutate(r=map(models, ~.x$pseudo.r.squared)) %>% unnest(cols=c(r)) %>% arrange(r)
library(car)
