#Analysis of biofouling data from expt conducted at Community Shellfish in Bremen, ME, in Summer 2022
#Ruby Krasnow
#Last modified: January 19, 2024

#Load packages
library(tidyverse)
library(tidytext)
library(patchwork)
library(plotrix)
library(rstatix)
library(betareg)
library(lmtest)
library(marginaleffects)

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

types_long <- types_filled %>%
  select(-c(comments, trt)) %>% 
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
                                prop_fouling=fouling_wt/whole_wet_wt,
                                fouling_ratio=prop_fouling*100) %>% 
  filter(fouling_ratio>0) %>% 
  mutate(log_fouling=log(fouling_ratio))

ratios <- ratios %>% filter(fouling_ratio<50)

ggplot(data=ratios)+
  geom_boxplot(aes(x=gear, y=fouling_ratio, color=location))+
 facet_wrap(~date)

ggplot(data=ratios)+
  geom_boxplot(aes(x=location, y=fouling_ratio, color=gear))
 # facet_wrap(~date)

ggplot(data=ratios)+
  geom_boxplot(aes(x=gear, y=whole_wet_wt, color=location))+
  facet_wrap(~date)

ggplot(data=ratios, aes(x=whole_wet_wt, y=fouling_ratio, color=location))+
  geom_point()+
  geom_smooth(method="lm", aes(linetype=gear), se=FALSE)+
  facet_wrap(~date)

ratios %>% 
  group_by(gear, date, location) %>% 
  identify_outliers(prop_fouling) %>% 
  select(date, gear, oyster_num, location, prop_fouling, is.extreme) #identify extreme outliers


ratios <- ratios %>% filter(!(oyster_num==106 & date=="10/17/22")) # if you include this point when running the model, the diagnostic plots also show that it has a very high Cook's distance

beta_formula <- list(
  #gear most obvious factor controlling variance, when including that, see which explanatory variables are needed
  add_only=prop_fouling~gear+location+date|gear,
  no_date=prop_fouling~gear*location|gear,
  no_loc1=prop_fouling~gear*date+gear:location|gear,
  no_loc2=prop_fouling~gear*date+gear:location+gear:location:date|gear,
  no_loc3=prop_fouling~gear*date+gear:location+gear:date|gear,
  
  #with the same main explanatory variables, having all three also contribute to the precision variance increases model performance
  full=prop_fouling~gear*location*date|gear+location+date,
  var_loc=prop_fouling~gear*location*date|location,
  var_gear_plus_loc=prop_fouling~gear*location*date|location+gear,
  var_date=prop_fouling~gear*location*date|date,
  var_gear=prop_fouling~gear*location*date|gear,
  no_var=prop_fouling~gear*location*date)

beta_models <- tibble(beta_formula, models = map(beta_formula, ~betareg(data=ratios, formula=.x ))) %>% 
  add_column(id=names(beta_formula), .before=1)

beta_models <- beta_models %>% 
  mutate(tidy_model = map(models, tidy),
          AIC=map(models, AIC),
          resids = map(models, residuals)) %>% unnest(cols=c(AIC)) %>%
  mutate(resids = map(resids, as.numeric()),
         normal_p = map(resids, ~shapiro.test(.x)$p.value)) %>%
  unnest(cols=c(normal_p)) %>%
  mutate(logLik=map(models, ~(logLik(.x)[1]))) %>% 
  unnest(cols=c(logLik)) %>% 
  mutate(r=map(models, ~.x$pseudo.r.squared)) %>% 
  unnest(cols=c(r)) %>% arrange(AIC)

#Turns out the full model does not contain any unnecessary information
beta_full<-betareg(data=ratios, prop_fouling~gear*location*date|gear+location+date)
summary(beta_full)


map(c("logit", "probit", "cloglog", "cauchit", "loglog"),
    ~(logLik(betareg(data=ratios, prop_fouling~gear*location*date|gear+location+date, link = .x))[1]))

map(c("logit", "probit", "cloglog", "cauchit", "loglog"),
      ~summary((betareg(data=ratios, prop_fouling~gear*location*date|gear+location+date, link = .x)))$pseudo.r.squared)

beta_log<-betareg(data=ratios, prop_fouling~gear*location*date|gear+location+date, link="loglog")
summary(beta_log)

par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(beta_log)
plot(beta_log, which = 5)
plot(beta_log, which = 6)
hist(residuals(beta_log))

plot(resid(beta_log) ~ fitted(beta_log))
lines(lowess(resid(beta_log) ~ fitted(beta_log)))

shapiro.test(residuals(beta_log))

coeftest(beta_log)
lrtest(beta_log, . ~ . | 1) 

### For Methods
# We analyzed fouling ratio using beta regression models from the package betareg (version 3.1-4), which are appropriate for response variables (rates, proportions) bound between 0 and 1. Two influential outliers with unusually high fouling ratios were removed before model fitting. Maximum likelihood was used to estimate coefficients for the mean equation relating the explanatory variables to the response variable as well as parameters for the precision model (i.e., regressors for the precision parameter, phi). The best link function for the mean model was the log-log link; the precision link was left as the default log link. Models were compared on the basis of their Akaike Information Criterion (AIC) values. A threshold of alpha=0.05 was used for all significance testing.

avg_slopes(beta_log, variable="gear") #gear, across all locations and dates
avg_slopes(beta_log, variable="location") #location, across all gears and dates
avg_slopes(beta_log, variable="date") #date, across all gears and locations

comparisons(beta_log, variables=list("gear"="pairwise"), by="date") #gear contrasts by date, across locations
comparisons(beta_log, variables=list("location"="pairwise"), by="date") #location contrasts by date, across gears
comparisons(beta_log, variables=list("gear"="pairwise"), by=c("date", "location")) #gear contrasts for each date and location
comparisons(beta_log, variables=c("location"), by=c("date", "gear")) #location contrasts for each date and gear

