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