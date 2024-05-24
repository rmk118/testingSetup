#Bremen Oyster Data Exploration
#5/15/23
# 
# library(tidyverse)
# library(lubridate)
# library(car)
# #library(ARTool)
# library(tidyquant)
# library(plotrix)
# library(patchwork)
# 
# # Oyster Data -------------------------------------------------------------
# 
# #Input data and convert to factors
# data<-read.csv("through10_17.csv") %>% 
#   mutate(
#     Cage = as.factor(Cage),
#     Bag = as.factor(Bag),
#     Location = as.factor(Location),
#     Gear = as.factor(Gear),
#     Treatment = as.factor(Treatment),
#     Tag.num = as.factor(Tag.num)
#   )
# #Fix date format
# data$Date<-mdy(data$Date)
# str(data)
# 
# #Select only October measurements
# final<- filter(data, Date == "2022-10-17")
# 
# #Final heights
# ggplot(data=final, aes(x = Gear, y = Height, fill=Location))+geom_boxplot()+scale_y_continuous(limits=c(0,95))+ylab("Shell height (mm)")
# 
# #Final cup ratio
# ggplot(data=final, aes(x = Gear, y = Cup.ratio, fill=Location))+geom_boxplot()+ylab("Cup ratio")+scale_y_continuous(limits=c(0,0.4))
# 
# #Final shell shape
# ggplot(data=final, aes(x = Gear, y = Shell.shape, fill=Location))+geom_boxplot()+ylab("Shell shape")
# 
# 
# # Summary graphs -----------------------------------------------------------------
# 
# #Height - all 6 treatments
# dataSum<- data %>% 
#   group_by(Location, Date, Gear, Treatment) %>% 
#             summarise(sh.sd = sd(Height, na.rm = TRUE),
#             sh = mean(Height, na.rm = TRUE))
# ggplot(dataSum, aes(x=Date, y=sh, color=Location, linetype=Gear)) +geom_line()
# 
# #Height -in vs out
# dataSum2<- data %>% 
#   group_by(Location, Date) %>% 
#   summarise(sh2.sd = sd(Height, na.rm = TRUE),
#             sh2 = mean(Height, na.rm = TRUE))
# ggplot(dataSum2, aes(x=Date, y=sh2, color=Location)) +geom_line()
# 
# #Height - by gear type
# dataSum3<- data %>% 
#   group_by(Gear, Date) %>% 
#   summarise(sh3.sd = sd(Height, na.rm = TRUE),
#             sh3 = mean(Height, na.rm = TRUE))
# ggplot(dataSum3, aes(x=Date, y=sh3, color=Gear)) +geom_line()
# 
# #Cup ratio - all 6 treatments
# dataSum4<- data %>% 
#   group_by(Location, Date, Gear, Treatment) %>% 
#   summarise(cr.sd = sd(Cup.ratio, na.rm = TRUE),
#             cr.stder = std.error(Cup.ratio),
#             cr = mean(Cup.ratio, na.rm = TRUE))
# ggplot(dataSum4, aes(x=Date, y=cr, color=Location, linetype=Gear)) +geom_line()+
#   geom_errorbar(aes(ymin=cr-cr.stder, ymax=cr+cr.stder,  color=Location,  linetype=Gear))+
#   facet_wrap(~Gear)
# 
# #Cup ratio - in vs out
# dataSum5<- data %>% 
#   group_by(Location, Date) %>% 
#   summarise(cr2.sd = sd(Cup.ratio, na.rm = TRUE),
#             cr.stder2 = std.error(Cup.ratio),
#             cr2 = mean(Cup.ratio, na.rm = TRUE))
# ggplot(dataSum5, aes(x=Date, y=cr2, color=Location)) +geom_line()+
#   geom_errorbar(aes(ymin=cr2-cr.stder2, ymax=cr2+cr.stder2,  color=Location))
# 
# #Cup ratio - by gear type
# dataSum6<- data %>% 
#   group_by(Gear, Date) %>% 
#   summarise(cr3.sd = sd(Cup.ratio, na.rm = TRUE),
#             cr.stder3 = std.error(Cup.ratio),
#             cr3 = mean(Cup.ratio, na.rm = TRUE))
# ggplot(dataSum6, aes(x=Date, y=cr3, color=Gear)) +geom_line()+
#   geom_errorbar(aes(ymin=cr3-cr.stder3, ymax=cr3+cr.stder3,  color=Gear))
# 
# 
# # Environmental data ---------------------------------------------------------------
# hobo<-read.csv("hobo.csv")
# hobo$Date.time<-mdy_hms(hobo$Date.time)
# 
# #all temps
# ggplot(hobo, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+ylab("Temperature (°C)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))
# 
# tempRolling<-ggplot(hobo, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line(alpha=0)+geom_ma(n=24, linetype="solid")+ylab("Temperature (°C)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+scale_y_continuous(limits=c(12.5, 22.5))
# tempRolling
# 
# noAir<- hobo[hobo$High.sal>5,]
# ggplot(noAir, aes(x=Date.time, y=Temp, group=Location, color=Location))+ geom_line()+ylab("Temperature (°C)")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))
# 
# #all sal - super weird
# salGraph<-ggplot(hobo, aes(x=Date.time, y=High.sal, group=Location, color=Location))+ geom_line()+ylab("Salinity")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))
# 
# # Turbidity ---------------------------------------------------------------
# turbidity<-read.csv("turbidity.csv")
# turbidity$Date<-mdy(turbidity$Date)
# turbidity$Location<-as.factor(turbidity$Location)
# 
# turbiditySummary<- turbidity %>% 
#   group_by(Location, Date) %>% 
#   summarise(se = std.error(Turbidity, na.rm = TRUE),
#             meanTurbidity = mean(Turbidity, na.rm = TRUE))
# turbiditySummary
# 
# turbidityGraph<-ggplot(turbiditySummary, aes(x=Date, y=meanTurbidity, group=Location, color=Location))+ geom_line()+ylab("Turbidity")+xlab("")+theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=meanTurbidity+se, ymax=meanTurbidity-se), width=.2,position=position_dodge(0.05))
# turbidityGraph
# 
# # ChlA --------------------------------------------------------------------
# ChlaDatasheet<-read.csv("chlA.csv")
# 
# ChlaDatasheet<- ChlaDatasheet %>% 
#   filter(Major_issue == FALSE) %>% 
#   mutate(Date = mdy(Trial_Date)) %>% 
#   select(Location, Acetone_vol, Tube_Num, Vol_Filtered, Fo, Fa, Fo.Fa, Date) %>% 
#   mutate(Location = as.factor(Location))
# 
# ChlFs = 0.000482
# FoFa_max = 1.7718
# 
# ChlaDatasheet = ChlaDatasheet %>%
#   mutate(Ave_Chl1 = (ChlFs*(FoFa_max/(FoFa_max-1))* 
#                        (ChlaDatasheet$Fo-ChlaDatasheet$Fa)*
#                        (((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))
# 
# ChlaDatasheet = ChlaDatasheet %>%
#   mutate(ChlaDatasheet, Ave_Phaeo1 = ((ChlFs*(FoFa_max/(FoFa_max-1)))*
#                                     ((FoFa_max-1)*(ChlaDatasheet$Fo-ChlaDatasheet$Fa))*(((ChlaDatasheet$Acetone_vol)/ChlaDatasheet$Vol_Filtered))))
# 
# ChlaDatasheet <- ChlaDatasheet %>% 
#   group_by(Date, Location) %>% 
#   summarise(mean = mean(Ave_Chl1),
#             se = std.error(Ave_Chl1))
# 
# chlaGraph<-ggplot(ChlaDatasheet, aes(x=Date, y=mean, group=Location, color=Location)) + geom_line()+ylab("Chlorophyll A (μg/L)")+xlab("")+ theme(axis.title.y = element_text(margin = margin(r = 10)))+geom_errorbar(aes(ymin=mean+se, ymax=mean-se), width=.2,position=position_dodge(0.05))
# 
# 
# # Accel -------------------------------------------------------------------
# #Inner Bag 9/12 to 10/06
# FBiData<- read.csv("Inner_FB_Accel_9.12.22to10.6.22.csv",header=TRUE)
# FBiData<- FBiData %>% 
#   mutate(Datetime=ISO.8601.Time, accelX=Ax..g., accelY=Ay..g., accelZ=Az..g.) %>% 
#   select(Datetime, accelX, accelY, accelZ)
# 
# #Inner Cage 9/12 to 10/06
# FCiData<- read.csv("Inner_Cage_Accel_9.12.22to10.6.22.csv",header=TRUE)
# FCiData<- FCiData %>% 
#   mutate(Datetime=ISO.8601.Time, accelX=Ax..g., accelY=Ay..g., accelZ=Az..g.) %>% 
#   select(Datetime, accelX, accelY, accelZ)
# 
# #Outer Cage 9/12 to 10/06
# FCoData<- read.csv("Outer_Cage_Accel_9.12.22to10.6.22.csv",header=TRUE)
# FCoData<- FCoData %>% 
#   mutate(Datetime=ISO.8601.Time, accelX=Ax..g., accelY=Ay..g., accelZ=Az..g.) %>% 
#   select(Datetime, accelX, accelY, accelZ)
# 
# #format time as a POSICXct 
# FBiData$Datetime<-ymd_hms(FBiData$Datetime)
# FCiData$Datetime<-ymd_hms(FCiData$Datetime)
# FCoData$Datetime<-ymd_hms(FCoData$Datetime)
# 
# FBiData = FBiData %>%
#   mutate(
#     diffX = accelX - lag(accelX),
#     diffY = accelY - lag(accelY),
#     diffZ = accelZ - lag(accelZ),
#     motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
# str(FBiData)
# 
# FCiData = FCiData %>%
#   mutate(
#     diffX = accelX - lag(accelX),
#     diffY = accelY - lag(accelY),
#     diffZ = accelZ - lag(accelZ),
#     motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
# str(FCiData)
# 
# FCoData = FCoData %>%
#   mutate(
#     diffX = accelX - lag(accelX),
#     diffY = accelY - lag(accelY),
#     diffZ = accelZ - lag(accelZ),
#     motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2)
# str(FCoData)
# 
# # acceleration plot FB all
# FBiPlotAll<-ggplot(data=FBiData, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "", y = "Motion")
# FBiPlotAll
# 
# # acceleration plot FCi all
# FCiPlotAll<-ggplot(data=FCiData, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "", y = "Motion")
# FCiPlotAll
# 
# # acceleration plot FCo all
# FCoPlotAll<-ggplot(data=FCoData, aes(x = Datetime, y = motionIndex))+ geom_line()+ theme_bw()+ labs(x = "", y = "Motion")
# FCoPlotAll
# 
# combinedAccel<-(FBiPlotAll+ggtitle("Inside bags")) + 
#                (FCiPlotAll+ggtitle("Inside cages")) + 
#                (FCoPlotAll+ggtitle("Outside cages")) + 
#                 plot_layout(ncol=1, guides = "collect")
# 
# # all environmental graphs ------------------------------------------------
# allGraphs<-(tempRolling+ guides(color="none")+ (turbidityGraph+ guides(color="none"))) / (salGraph + (chlaGraph + guides(color="none"))& theme(legend.position = "bottom"))| combinedAccel


# biofouling --------------------------------------------------------------

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


# biofouling beta ---------------------------------------------------------

beta_fit <- betareg(data=ratios, prop_fouling~gear*location)
beta_fit2 <- betareg(data=ratios, prop_fouling~gear*location|date)

beta_fit_var_gear <- betareg(data=ratios2, prop_fouling~gear*location*date|gear)
summary(beta_fit_var_gear)

plot(beta_fit_var_gear, which = 6)

beta_no_outliers <- betareg(data=ratios3, prop_fouling~gear*location*date|gear)
#lrtest(beta_fit_var_gear, beta_no_outliers)

summary(beta_no_outliers)
hist(residuals(beta_no_outliers))
shapiro.test(residuals(beta_no_outliers))


plot((test %>% filter(id=="var_gear") %>% pull(models))[[1]])
plot((test2 %>% filter(id=="var_gear") %>% pull(models))[[1]])

lrtest((test %>% filter(id=="var_gear") %>% pull(models))[[1]],
       (test %>% filter(id=="plus_wt") %>% pull(models))[[1]])

waldtest((test %>% filter(id=="plus_wt") %>% pull(models))[[1]], -whole_wet_wt)
waldtest((test2 %>% filter(id=="plus_wt") %>% pull(models))[[1]], 4)


#We performed post-hoc comparisons testing the differences due to gear type, location, and gear*location with estimated marginal means using the emmeans package with Tukey adjusted p-values.
library(emmeans)
joint_tests(beta_log)
emm_gear<- emmeans(beta_log, ~ gear)

emmeans(beta_log, "gear")
with(ratios, tapply(prop_fouling, gear, mean))

emm_location<- emmeans(beta_log, ~ location)
emm_gear_by_loc<-emmeans(beta_log, ~ gear|location)
emm_loc_by_gear<-emmeans(beta_log, ~ location|gear)

emmeans::contrast(emm_gear, method = "tukey")
emmeans::contrast(emm_gear_by_loc, method = "tukey")
emmeans::contrast(emm_location, method = "tukey")
emmeans::contrast(emm_loc_by_gear, method = "tukey")

plot(emm_gear)
plot(emm_gear_by_loc)
marginal = emmeans(beta_log, ~ gear)
marginal2 = emmeans(beta_log, ~ gear*location)

pairs(marginal, adjust="tukey")
pairs(marginal2)

#library(multcomp)
# Sum = cld(marginal,
#           alpha   = 0.05,
#           Letters = letters,         ###  Use lowercase letters for .group
#           adjust  = "sidak")
# 
# Sum

slopes(beta_log) %>% group_by(term) %>% summarize(avg_slope = mean(estimate))
summary(slopes(beta_log))

avg_slopes(beta_log, newdata = datagrid(gear = c("BP", "FB", "FC")))

avg_slopes(beta_log, variable="gear")

slopes(beta_log, variable="gear", newdata = datagrid())

slopes(beta_log, variable=c("gear"), by="location")
slopes(beta_log, variable=c("gear"), by="location", newdata = datagrid(location=unique))

hypotheses(beta_log)

glht(beta_log)

# ggplot(data=ratios, aes(x=whole_wet_wt, y=fouling_ratio, color=location))+
#   geom_point()+
#   geom_smooth(method="lm", aes(linetype=gear), se=FALSE)+
#   facet_wrap(~date)

#avg_slopes(beta_log, variable=c("gear"), by="date") #same as above without FC-FB contrasts

#avg_comparisons(beta_log, variables=list("gear"="pairwise"), by=c("date", "location")) equiv to below
avg_comparisons(beta_log, variables = list(gear= "pairwise"))
avg_comparisons(beta_log, variables = list(location= "pairwise"))
avg_comparisons(beta_log, variables = list(date= "pairwise"))

comparisons(beta_log,newdata = "marginalmeans", variables = list(gear= "pairwise"), by="location")

plot_comparisons(beta_log,newdata = "marginalmeans", variables = list(gear= "pairwise"), by="location")
plot_comparisons(beta_log, condition = c("gear", "location"))

plot_slopes(beta_log,variables = list(gear= "pairwise"), by="location" )
plot_slopes(beta_log,variables = list(gear= "pairwise"))

plot_predictions(beta_log, condition = c("gear", "location"))

predictions(beta_log, by=c("location", "gear"), hypothesis="pairwise")

predictions(beta_log, newdata = datagrid(location = c("in", "out"), gear=c("FB", "FC", "BP")), hypothesis = c("FB=FC", "BP=FC", "BP=FB"))

comparisons(beta_log, variables = "location", by="gear", hypothesis="pairwise")

predictions(beta_log, by = "gear",
  newdata = datagrid(gear = unique, location = unique), hypothesis="pairwise")

# Equivalent but not contrasts of interest
#avg_slopes(beta_log, variable="location", by="date")
#avg_comparisons(beta_log, variables=list("location"="pairwise"), by="date")
predictions(beta_log, by=c("gear", "location", "date"))

avg_slopes(beta_log, newdata = "mean") #what is calculated by package emmeans


#predictions(beta_log, by=c("location", "gear"), hypothesis = "pairwise")

library(emmeans)
joint_test(beta_log)

plot_ame_fancy <- beta_log |> 
  avg_slopes()

plot_mem_fancy <- beta_log |> 
  avg_slopes(newdata = "mean")

# Combine the two tidy data frames for plotting
plot_effects <- bind_rows("AME" = plot_ame_fancy, "MEM" = plot_mem_fancy, .id = "type") |> 
  filter(term == "gear") |> 
  mutate(nice_slope = round(estimate * 100, 3))

library(MetBrewer)
# Use the Johnson color palette
clrs <- met.brewer("Johnson")

ggplot(plot_effects %>% filter(contrast=="FC - BP"), aes(x = estimate * 100, y = fct_rev(type), color = type)) +
  geom_vline(xintercept = 0, linewidth = 0.5, linetype = "24", color = clrs[1]) +
  geom_pointrange(aes(xmin = conf.low * 100, xmax = conf.high * 100)) +
  geom_label(aes(label = nice_slope), nudge_y = 0.3) +
  labs(x = "Marginal effect (percentage points)", y = NULL) +
  # scale_color_manual(values = c(clrs[2], clrs[5]), guide = "none") +
  theme_bw()

plot_predictions(beta_log, condition = c("gear", "location"))


nd <- datagrid(
  model = beta_log,
  gear = ratios$gear,
  location = ratios$location,
  date=ratios$date)
nrow(nd)

cmp <- comparisons(beta_log,
                   variables = list("gear" = "pairwise"),
                   newdata = nd,
                   type = "response")
nrow(cmp)

avg_comparisons(beta_log,
                by = "location",
                variables = list("gear" = "pairwise"),
                newdata = nd,
                type = "response")

emm_gear_by_loc<-emmeans(beta_log, specs="gear",by="location", regrid = "response")
emmeans::contrast(emm_gear_by_loc, method = "revpairwise")


predictions(beta_log, by = "gear", hypothesis="pairwise") #gear, across all locations and dates
predictions(beta_log, by = "location", hypothesis="pairwise") #location, across all gears and dates



pairs(emmeans(beta_log, "gear", by = "location", at = list(date=unique(ratios$date))),
      infer = TRUE, adjust = "none", reverse = TRUE)



library(emmeans)
emm_gear <- emmeans(beta_log, ~gear, regrid = "response")
emm_location<- emmeans(beta_log, ~ location, regrid = "response")
emm_gear_by_loc<-emmeans(beta_log, ~ gear|location, regrid = "response")
emm_gear_by_loc<-emmeans(beta_log, spec="gear", by="location", regrid = "response")
emm_loc_by_gear<-emmeans(beta_log, ~ location|gear, regrid = "response")

emmeans::contrast(emm_gear, method = "tukey")
emmeans::contrast(emm_gear_by_loc, method = "tukey")
emmeans::contrast(emm_location, method = "tukey")
emmeans::contrast(emm_loc_by_gear, method = "tukey")

avg_comparisons(beta_log, variables=c("location"), by=c("date", "gear")) #location contrasts for each date and gear
avg_comparisons(beta_log, variables=list("location"="pairwise"), by="date") #location contrasts by date, across gears

em


# May 2024  ---------------------------------------------------------------

# g <- expand_grid(location=c("in", "out"), gear=c("FB", "FC", "BP")) %>% 
#   mutate(date=mdy("6/3/22"), height_mean=47.64, height_sd=11.72) %>% bind_rows(g)

# emm.src <- emmeans(mod2, specs=~"gear*loc*date", type="response")
# 
# str(regrid(emm.src))
# 
# summary(emm.src, infer = TRUE)
# summary(emm.src, infer = TRUE, type = "response")
# summary(regrid(emm.src), infer = TRUE)
# 
# plot(emm.src)


#pairs(emmeans(mod2, "gear", by="loc", type="response"), reverse = "true", adjust="bonferroni")

create_supp_gt_avg_comps <- function(mod, variable, by=TRUE) {
  avg_comparisons(mod, vcov=vcov(mod2), variables=variable, by=by, p_adjust = "bonferroni") %>% 
    select(any_of(c("term", "contrast", "date", "gear", "loc", "estimate", "std.error", 
                    "statistic","p.value", "conf.low", "conf.high"))) %>% 
    dplyr::rename(any_of(names)) %>% 
    gt() %>% 
    fmt_number(decimals =4) %>% 
    sub_small_vals(threshold = 0.001) %>% 
    tab_style(locations = cells_column_labels(columns=any_of(c("term", "contrast", "date", "gear", "loc", "estimate", "statistic", "conf.low", "conf.high"))),
              style = cell_text(transform = "capitalize")) %>% 
    tab_style(locations = cells_body(columns="term"),
              style = cell_text(transform = "capitalize"))
}


#Supplementary tables
s1 <- create_supp_gt(mod=mod2, variable=list(gear="pairwise")) #gear, across all locations and dates
s2 <- create_supp_gt(mod=mod2,variable=list(loc="pairwise")) #location, across all gears and dates
s3 <- create_supp_gt(mod=mod2,variable=list("gear"="pairwise"), by="loc") #gear contrasts by location, across dates
s4 <- create_supp_gt(mod=mod2,variable=list("loc"="pairwise"), by="gear") #location contrasts by gear, across dates
s5 <- create_supp_gt(mod=mod2,variable=list("gear"="pairwise"), by=c("date", "loc")) #gear contrasts for each date and location


supp_gts <- gt_group(s1, s2, s3, s4, s5)
supp_gts

# create_supp_tab1 <- function(mod, variable, by=NULL) {
#   
#   pairs(emmeans(mod, specs=variable, by=by, type="response"), reverse = "true", adjust="bonferroni") %>% 
#     tidy() %>% 
#     mutate(across(any_of(c("adj.p.value","p.value")), ~if_else(.x==0, 1e-10, .x))) %>% 
#     select(any_of(c("term", "contrast", "date", "gear", "loc", "odds.ratio", "estimate", "std.error", "statistic", "conf.low", "conf.high", "z.ratio", "p.value", "adj.p.value"))) %>% 
#     dplyr::rename(any_of(names))
# }
# 
# 
# s1c <- create_supp_tab1(mod=mod2,"gear") #gear, across all locations and dates
# s2c <- create_supp_tab1(mod=mod2,"loc") #location, across all gears and dates
# 
# s3c <- create_supp_tab1(mod=mod2,"gear", by="date") #gear contrasts by date, across locations
# s4c <- create_supp_tab1(mod=mod2,"gear", by="loc") #gear contrasts by location, across dates
# s5c <- create_supp_tab1(mod=mod2,"loc", by="gear") #location contrasts by gear, across dates
# s6c <- create_supp_tab1(mod=mod2,"gear", by=c("date", "loc")) #gear contrasts for each date and location
# 
# 
# supp_tabs1 <- bind_rows(s1c, s2c, s3c, s4c, s5c, s6c, .id="id")
# supp_tabs1  %>% 
#   gt() %>% 
#   fmt_number(decimals =3) %>% 
#   sub_small_vals(threshold = 0.001) %>% 
#   tab_style(locations = cells_column_labels(columns=any_of(c("term", "contrast", "date", "gear", "estimate", "statistic", "conf.low", "conf.high"))),
#             style = cell_text(transform = "capitalize")) %>% 
#   tab_style(locations = cells_body(columns="term"),
#             style = cell_text(transform = "capitalize")) %>% 
#   cols_move_to_end(contains("value"))


par(mfrow=c(2,3))
lapply(M3, \(x) function(x) {
  ggcorr(x,label=TRUE, label_round=2)})

corrplot(all_monthly %>% ungroup() %>% 
           select(-date) %>% 
           select(-ends_with("mg")),label=TRUE, label_round=2)+facet_grid(loc~gear)

par(mfrow=c(2,3))
monthly_nested <- all_monthly %>% 
  ungroup() %>% 
  select(-date) %>% 
  select(-ends_with("mg")) %>% group_by(loc, gear) %>% nest()


m<-monthly_nested %>% rowwise() %>% mutate(corrR = list(rcorr(data %>% as.matrix())$r),
                                           corrP = list(rcorr(data %>% as.matrix())$P))

par(mfrow=c(2,3))
pwalk(list(m$corrR, m$corrP,m$loc, m$gear), \(x, y, z, a) corrplot(x, type="lower", diag = FALSE, p.mat = y, insig = "blank", title = paste(z, a)))

# Env correlations --------------------------------------------------------

hobo_weekly <- noAir %>%
  mutate(date = floor_date(dt, unit="week")) %>% 
  group_by(date, loc) %>% 
  summarise(temp = mean(temp), sal = mean(sal))

t_weekly <- t_data %>%
  mutate(date = floor_date(date, unit="week")) %>% 
  group_by(date, loc) %>% 
  summarise(turbidity = mean(turbidity))

g_weekly <- g_data %>% 
  mutate(date = floor_date(date, unit="week"), loc=location) %>% 
  group_by(date, loc, gear) %>% 
  summarise(across(all_of(g_cols), mean)) %>% 
  mutate(loc = if_else(loc=="in", "Inside", "Outside"))

chl_weekly <- chlA %>% 
  mutate(date = floor_date(date, unit="week")) %>% 
  group_by(date, loc) %>%
  summarise(chl = mean(chl))

SPM_weekly <- SPM_final_data %>%
  mutate(date = floor_date(Date, unit="week"), loc = Type, .keep="unused") %>% 
  group_by(date, loc) %>%
  summarise(across(where(is.double),mean))


ggplot(hobo_weekly, aes(x=date, y=temp, color=loc))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  labs(x=NULL,y="Temperature (°C)", color="Location")

ggplot(t_weekly, aes(x=date, y=turbidity, color=loc))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  labs(x=NULL,y="Turbidity", color="Location")


all_weekly <- hobo_weekly %>% 
  full_join(chl_weekly, by=c("date", "loc")) %>% 
  full_join(SPM_weekly, by=c("date", "loc")) %>% 
  full_join(t_weekly, by=c("date", "loc"))
#full_join(g_weekly, by=c("date", "loc"))


hobo_monthly <- noAir %>%
  mutate(date = floor_date(dt, unit="month")) %>% 
  group_by(date, loc) %>% 
  summarise(temp = mean(temp), sal = mean(sal))

t_monthly <- t_data %>%
  mutate(date = floor_date(date, unit="month")) %>% 
  group_by(date, loc) %>% 
  summarise(turbidity = mean(turbidity))

g_monthly <- g_data %>% 
  mutate(date = floor_date(date, unit="month"), loc=location) %>% 
  group_by(date, loc, gear) %>% 
  summarise(across(all_of(g_cols), mean)) %>% 
  mutate(loc = if_else(loc=="in", "Inside", "Outside"))

chl_monthly <- chlA %>% 
  mutate(date = floor_date(date, unit="month")) %>% 
  group_by(date, loc) %>%
  summarise(chl = mean(chl))

SPM_monthly <- SPM_final_data %>%
  mutate(date = floor_date(Date, unit="month"), loc = Type, .keep="unused") %>% 
  group_by(date, loc) %>%
  summarise(across(where(is.double),mean))


ggplot(hobo_monthly, aes(x=date, y=temp, color=loc))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  labs(x=NULL,y="Temperature (°C)", color="Location")

ggplot(t_monthly, aes(x=date, y=turbidity, color=loc))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  labs(x=NULL,y="Turbidity", color="Location")


all_monthly <- hobo_monthly %>% 
  full_join(chl_monthly, by=c("date", "loc")) %>% 
  full_join(SPM_monthly, by=c("date", "loc")) %>% 
  full_join(t_monthly, by=c("date", "loc"))

all_monthly <- all_monthly %>% right_join(g_monthly)

M3 <- split(all_monthly, ~gear+loc)
M3 <- map(M3, \(x) x %>% 
            ungroup() %>% 
            select(where(is.double)) %>% 
            select(-date) %>% 
            select(-ends_with("mg")) %>% 
            as.matrix())

M3 <- map(M3, \(x) rcorr(x))

# Plot all picture

corrplot(M3$BP.Inside$r, diag = FALSE, type="lower", p.mat = M3$BP.Inside$P, insig = "blank")


sample_dates <- g_data %>% select(date) %>% distinct()

disp_formula_short <- list(
  full = ~gear*loc,
  add_only=~gear+loc,
  gear=~gear,
  loc=~loc)

disp_models_m <- disp_models_m %>% 
  mutate(tidy_model = map(models, broom.mixed::tidy),
         r2=map(models, r2_efron),
         resids = map(models, residuals)) %>% unnest(cols=c(r2)) %>%
  mutate(resids = map(resids, as.numeric()),
         normal_p = map(resids, ~shapiro.test(.x)$p.value)) %>%
  unnest(cols=c(normal_p)) %>%
  mutate(disp=map(models, ~check_overdispersion(m_mod)$chisq_statistic)) %>% 
  unnest(cols=c(disp)) %>% arrange(r2)

disp_models_m <- tibble(disp_formula_short,
                        models = map(disp_formula_short,
                                     ~glmmTMB(data=m_data, 
                                              formula= num~gear,
                                              dispformula=.x, 
                                              family = poisson))) %>% 
  add_column(id=names(disp_formula_short), .before=1)

compare_performance(ci1, ci2, ci3,ci4,metrics = c("AIC", "AICc", "BIC", "RMSE"))


# FAN RATIO MODELS --------------------------------------------------------

disp_models_fan <- tibble(disp_formula, 
                          models = future_map(disp_formula,
                                              ~glmmTMB(data=g, 
                                                       formula= fan_ratio ~ chl+temp+PIM+date*gear,
                                                       dispformula=.x, 
                                                       family = tweedie))) 


disp_models_fan <- disp_models_fan %>% add_column(id=names(disp_formula), .before=1) %>% compare_mods()

#glmmTMB(fan_ratio ~ chl+temp+PIM+turbidity+date+gear+loc, data = g, family=lognormal) #fan_mod lower AIC than this model with tweedie, Gamma, and lognormal families
#glmmTMB(fan_ratio ~ chl+temp+PIM+date+gear, data = g, family=lognormal) #fan_mod lower AIC than this model with tweedie, Gamma, and lognormal families
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear*loc, data = g, family="tweedie") #rank-deficient
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*loc+gear, data = g, family="tweedie") #rank-deficient
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear+loc, data = g, family="lognormal") #fan_mod lower AIC than this model with tweedie, Gamma, and lognormal families
#glmmTMB(fan_ratio ~ chl+temp+PIM+date+gear*loc, data = g, family=tweedie) #fan_mod lower AIC than this model with tweedie, Gamma, and lognormal families
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear+loc+date, data = g, family=Gamma) #fan_mod lower AIC than this model with tweedie, Gamma, and lognormal families


#glm(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family=Gamma) #fan_mod lower AIC
#glmer(fan_ratio~date*gear*loc+(1|date), data=g, family=Gamma) #singular

#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family=Gamma) #fan_mod lower AIC than this model with tweedie, Gamma, and lognormal families
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="lognormal", dispformula=~gear*loc) #AIC=-6496
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="lognormal", dispformula=~loc+date) #AIC=-6535
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="lognormal", dispformula=~gear+date) #AIC=-6537
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="lognormal", dispformula=~date) #AIC=-6537
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="lognormal", dispformula=~loc) #fan_mod lower AIC
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="lognormal", dispformula=~gear)  #fan_mod lower AIC

#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="tweedie", dispformula=~gear*loc) #fan_mod lower AIC
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="tweedie", dispformula=~gear+loc) #fan_mod lower AIC


#glmmTMB(fan_ratio ~ date*gear*loc, data = g, dispformula=~loc+date) #non-uniform residuals

####### Convergence issues (iteration limit reached without convergence)
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="tweedie", dispformula=~gear+date)
#glmmTMB(fan_ratio ~ chl+temp+PIM+date*gear, data = g, family="tweedie", dispformula=~date+loc)
#glmmTMB(fan_ratio ~ date*gear*loc, data = g, family="tweedie", dispformula=~gear*loc)
#glmmTMB(fan_ratio ~ date*gear*loc, data = g, family="tweedie", dispformula=~gear+loc)
#glmmTMB(fan_ratio ~ date*gear*loc, data = g, family="tweedie", dispformula=~gear*date)
#glmmTMB(fan_ratio ~ date*gear*loc, data = g, family="tweedie", dispformula=~gear+date)
#glmmTMB(fan_ratio ~ date*gear*loc, data = g, family="tweedie", dispformula=~loc+date)
#glmmTMB(fan_ratio ~ date*gear*loc, data = g, family="lognormal")
#glmmTMB(fan_ratio ~ date*gear*loc, data = g, family="lognormal", dispformula=~loc+date)


#glmmTMB(fan_ratio~ chl+temp+PIM+date*gear+date*loc, data=g, family=lognormal) #AIC -6528 but rank-deficient
#glm(fan_ratio~date*gear*loc, data=g, family=Gamma) fan_mod lower AIC

anova(glm1, fan_mod)
compare_performance(glm1, fan_mod, metrics = c("AIC", "AICc", "BIC", "RMSE"))


# CUP RATIO MODELS --------------------------------------------------------

# glm1<-glmmTMB(cup_ratio ~ chl+temp+PIM+turbidity+date+gear*location, data = df, family="beta_family")
# glm1<-glmmTMB(cup_ratio ~ chl+temp+PIM+turbidity+date+gear*location, data = df, family="beta_family")
# glm1<-glmmTMB(cup_ratio ~ chl+PIM+turbidity+date+gear*location, data = df, family="beta_family")
# glm1<-glmmTMB(cup_ratio ~ chl+PIM+date+gear*location+(1|tag_num), data = df, family="beta_family")
# glmm1<-glmmTMB(cup_ratio ~ chl+PIM+date+gear*location, data = df, family="beta_family")
# glmm2<-glmmTMB(cup_ratio ~ chl+PIM+date+gear*location, data = df,dispformula = ~gear, family="beta_family")
# glmm3<-glmmTMB(cup_ratio ~ chl+PIM+date+gear*location, data = df,dispformula = ~gear*location, family="beta_family")
# 
#glmmTMB(cup_ratio ~ date*gear*loc, data = g, dispformula = ~loc*date, family="beta_family") #lower AIC than cup_mod

#glmmTMB(cup_ratio ~ date*gear*loc, data = g, dispformula = ~date*loc, family="lognormal") #convergence issues
# Lower AIC but gives convergence warning and has same efron's pseudo-R^2
# glmm5<-glmmTMB(cup_ratio ~ date*gear*location, data = df,dispformula = ~date*gear*location, family="beta_family")
# glmm6<-glmmTMB(cup_ratio ~ date*gear*location+chl, data = df,dispformula = ~date*gear*location, family="beta_family")

# Shell shape -------------------------------------------------------------

glmmTMB(chi ~ chl+temp+PIM+turbidity+date+gear, data =g, family="tweedie")
chi1<-glmmTMB(chi ~ date*gear*location, data =g, family=tweedie, dispformula = ~date+gear+loc)
chi3<-glmmTMB(chi ~ temp+turbidity+date+loc*gear, data =g, family="tweedie")
#chi4<-glmmTMB(chi ~ date*gear*location+chl+turbidity+PIM+temp, data =g, family="tweedie") #rank-deficient
chi5<-glmmTMB(chi ~ date*gear*location+(1|tag_num), data =g, family="tweedie")

anova(chi_mod, chi1)
plot(check_homogeneity(chi5))


# Height ------------------------------------------------------------------

h1<-lm(height ~ chl+temp+turbidity+date+gear, data = g)
h2<-lm(height ~ chl+temp+date+gear, data = g)
h3<-lm(height ~ chl+temp+date*gear, data = g)
h3.5<-lm(height ~ temp+date*gear, data = g)
h4<-glmmTMB(height ~ chl+temp+date*gear+(1|location), data = g)
h5<-glmmTMB(height ~ chl+temp+date*gear+(1|tag_num), data = g)
h5<-lmer(height ~ chl+temp+date*gear+(1|tag_num), data = g)
h7<-lmer(height ~ temp+date+date:gear+(1|tag_num), data = g)
check_model(h5)

compare_performance(h1, h2, h3, h3.5,h4,h5,h6,h7,metrics = c("AIC", "AICc", "BIC", "RMSE"))
compare_performance(h5, h6,h7)
anova(h1, h2, h3, h3.5, h4, h5, h6)
anova(h5, h6,h7)
