#Updated analysis of data from Bremen oyster experiment
#Last modified: May 15, 2024
#Ruby Krasnow

library(tidyverse)
library(patchwork)
library(corrplot)
library(GGally)
library(Hmisc)
library(ggstatsplot)
library(easystats)

source("Linear modelling workflow_support functions.R") 

# Temp and Salinity -------------------------------------------------------

hobo <- read.csv("./data/hobo.csv") %>% 
  mutate(date=mdy_hms(Date.time), temp=Temp, sal=High.sal,loc=Location,.keep="unused")

noAir <- hobo %>% filter(sal>5)

#Find hours when both sensors logged with no issues
Inside<- noAir %>% filter(loc=="Inside") %>% dplyr::rename(sal_in = sal, temp_in=temp)
Outside<-noAir %>% filter(loc=="Outside")%>% dplyr::rename(sal_out = sal, temp_out=temp)

common <- inner_join(Inside, Outside, by="date") %>% 
  select(-c("loc.x", "loc.y")) %>% 
  mutate(sal_diff = sal_in - sal_out, temp_diff=temp_in-temp_out)

# ggplot(common, aes(x=date, y=sal_diff))+geom_line()
# ggplot(noAir, aes(x=date, y=sal, color=loc))+geom_line()
 ggplot(hobo, aes(x=date, y=sal, color=loc))+geom_line()+
   theme_classic()+ labs(x = NULL, y = "Salinity", color="Location") +
   scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])
# ggboxplot(hobo, x = "loc", y = "sal", ylab = "Salinity", xlab = "Locations", add = "jitter")
# 
# ggplot(common, aes(x=date, y=temp_diff))+geom_line()
# ggplot(noAir, aes(x=date, y=temp, color=loc))+geom_line()+theme_bw()
# ggplot(hobo, aes(x=date, y=temp, color=loc))+geom_line()+theme_bw()
# ggboxplot(hobo, x = "loc", y = "temp", ylab = "Temp", xlab = "Locations", add = "jitter")

common_roll <- common %>%
  mutate(across(where(is.double), \(x) rollmean(x, k = 24, fill = NA))) %>% na.omit()

noAir_roll <- noAir %>% group_by(loc) %>% 
  mutate(across(where(is.double), \(x) rollmean(x, k = 24, fill = NA))) %>% na.omit()

ggplot(common_roll, aes(x=date, y=sal_diff))+geom_line()

sal_plot <- ggplot(noAir_roll, aes(x=date, y=sal, color=loc))+
  geom_line()+
  theme_classic()+ labs(x = NULL, y = "Salinity", color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  theme(legend.position="none")



temp_plot <-ggplot(noAir_roll, aes(x=date, y=temp, color=loc))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  labs(x=NULL,y="Temperature (°C)", color="Location")



# Turbidity -------------------------------------------------------

t_data <- read.csv("./data/turbidity.csv") %>% 
  mutate(date=mdy(date), loc=as.factor(location),.keep="unused")

t <- t_data %>% group_by(date, loc) %>% summarise(mean=mean(turbidity), sd=sd(turbidity))

turbidity_plot <- ggplot(data=t)+
  geom_errorbar(aes(x=date, ymin=mean-sd, ymax=mean+sd, color=loc), width=0.2)+
  geom_line(aes(x=date, y=mean, color=loc))+
  geom_point(aes(x=date, y=mean, color=loc))+
  labs(x=NULL, y="Turbidity", color="Location")+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])

# Chl A -------------------------------------------------------

chlA <- read.csv("./data/chlA.csv") %>% 
    mutate(date = mdy(Trial_Date), loc=as.factor(Location)) %>%
    select(loc, Acetone_vol, Tube_Num, Vol_Filtered, Fo, Fa, Fo.Fa, date)

ChlFs <- 0.000482
FoFa_max <- 1.7718

chlA <- chlA %>%
  mutate(chl = ChlFs * (FoFa_max/(FoFa_max-1)) * (Fo-Fa)* ((Acetone_vol)/Vol_Filtered))

chl <- chlA %>% 
  group_by(date, loc) %>%
  summarise(mean = mean(chl),sd = sd(chl))

chl_plot <-ggplot(data=chl)+
  geom_errorbar(aes(x=date, ymin=mean-sd, ymax=mean+sd, color=loc), width=0.2)+
  geom_line(aes(x=date, y=mean, color=loc))+
  geom_point(aes(x=date, y=mean, color=loc))+
  labs(x=NULL, y="Chlorophyll A (μg/L)", color="Location")+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])

# SPM -------------------------------------------------------

SPM_data <- read.csv("./data/SPM_data.csv")

#Finding the TPM and POM from dry and ashed weights
SPM <- SPM_data %>% mutate(TPM_mg = Dry_wt-Filter_wt, POM_mg = Dry_wt-Ashed_wt, date=mdy(Date))

# Separate and average for formate Blank TPM & POM

blank_df <- SPM %>%
  filter(Type %in% c("Formate Blank", "True Blank"))

true_blank_df <- SPM %>%
  filter(Type == 'True Blank') %>%
  group_by(date) %>%
  dplyr::summarize(TB_TPM = mean(TPM_mg, na.rm = TRUE),
            TB_POM =mean(POM_mg, na.rm = TRUE))

formate_blank_df <- SPM %>%
  filter(Type == 'Formate Blank') %>%
  group_by(date) %>%
  dplyr::summarize(FB_TPM = mean(TPM_mg, na.rm = TRUE),
            FB_POM =mean(POM_mg, na.rm = TRUE))

SPM <- SPM %>%
  left_join(formate_blank_df, by='date') %>%
  left_join(true_blank_df, by='date')

# Solve for blank corrected values including PIM
SPM <- SPM %>%
  mutate(TPM_Blank_Corrected_mg = TPM_mg - (FB_TPM - TB_TPM), 
         POM_Blank_Corrected_mg = POM_mg - (FB_POM - TB_POM),
         PIM_Blank_Corrected_mg = TPM_Blank_Corrected_mg - POM_Blank_Corrected_mg)

# Convert Volume sampled & Filtered into a conversion of total volume
SPM <- SPM %>%
  mutate(Total_Vol_Convers = Vol_Filtered/Vol_Sampled)

# Solve for TOTAL particulate weights
SPM <- SPM %>%
  mutate(TPM_TOTAL_mg = TPM_Blank_Corrected_mg * Total_Vol_Convers,
         POM_TOTAL_mg = POM_Blank_Corrected_mg * Total_Vol_Convers,
         PIM_TOTAL_mg = PIM_Blank_Corrected_mg * Total_Vol_Convers) %>% 
  mutate(SPM, across(ends_with("_TOTAL_mg"), ~replace_na(.x,0))) #remove NAs

# Calculating particulate weight in mg/ml
SPM <- SPM %>%
  mutate(TPM_mg_ml = TPM_Blank_Corrected_mg/Vol_Filtered,
         POM_mg_ml = POM_Blank_Corrected_mg/Vol_Filtered,
         PIM_mg_ml = PIM_Blank_Corrected_mg/Vol_Filtered) %>% 
  mutate(SPM, across(ends_with("_mg_ml"), ~replace_na(.x,0)))  #remove NAs

SPM_final_data <- SPM %>% 
  filter(Type %in% c("Inside", "Outside")) %>% 
  select(date, Type, TPM_TOTAL_mg, POM_TOTAL_mg, PIM_TOTAL_mg, TPM_mg_ml, POM_mg_ml, PIM_mg_ml)

SPM_final <- SPM_final_data %>%
  group_by(date, Type) %>%
  summarise(#Ave_TOT_TPM_mg = mean(TPM_TOTAL_mg), 
            #TOT_TPM_SE = std.error(TPM_TOTAL_mg),
           # TOT_TPM_SD = sd(TPM_TOTAL_mg),
           # Ave_TOT_POM_mg = mean(POM_TOTAL_mg), 
           # TOT_POM_SD = sd(POM_TOTAL_mg),
          #  Ave_TOT_PIM_mg = mean(PIM_TOTAL_mg), 
           # TOT_PIM_SD = sd(PIM_TOTAL_mg),
            TPM_mg_L = mean(TPM_mg_ml*1000), 
            TPM_mg_L_SD = sd(TPM_mg_ml*1000),
            POM_mg_L = mean(POM_mg_ml*1000), 
            POM_mg_L_SD = sd(POM_mg_ml*1000),
            PIM_mg_L = mean(PIM_mg_ml*1000),
            PIM_mg_L_SD = sd(PIM_mg_ml*1000)) %>% 
  dplyr::rename(loc=Type)

# Theme for creating extra classy gg plots
mytheme <- theme_classic()+theme(legend.position = "bottom",
                 axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

TPM_mg_L <- ggplot(SPM_final) +
  geom_point(aes(x = date, y = TPM_mg_L, colour = loc)) +
  geom_errorbar(aes(x = date, ymin = TPM_mg_L-TPM_mg_L_SD, ymax = TPM_mg_L+TPM_mg_L_SD, color = loc),
                width = 0.2) +
  geom_line(aes(x = date, y = TPM_mg_L, colour = loc, group = loc)) +
  ylab("TPM (mg/L)") + labs(x=NULL, color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+ 
  mytheme

POM_mg_L <- ggplot(SPM_final) +
  geom_point(aes(x = date, y = POM_mg_L, colour = loc)) +
  geom_errorbar(aes(x = date, ymin = POM_mg_L-POM_mg_L_SD, ymax = POM_mg_L+POM_mg_L_SD, color = loc),
                width = 0.2) +
  geom_line(aes(x = date, y = POM_mg_L, colour = loc, group = loc)) +
  ylab("POM (mg/L)") + labs(x=NULL, color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  mytheme

PIM_mg_L <- ggplot(SPM_final) +
  geom_point(aes(x = date, y = PIM_mg_L, colour = loc)) +
  geom_errorbar(aes(x = date, ymin = PIM_mg_L-PIM_mg_L_SD, ymax = PIM_mg_L+PIM_mg_L_SD, color = loc),
                width = 0.2) +
  geom_line(aes(x = date, y = PIM_mg_L, colour = loc, group = loc)) +
  ylab("PIM (mg/L)") + labs(x=NULL, color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  mytheme 

spm_plot <-(TPM_mg_L / POM_mg_L / PIM_mg_L) +
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(face = "bold"), legend.position = 'none')

# Accelerometer -------------------------------------------------------

ic_fall <- read.csv("./data/accel/Inner_Cage_Accel_9.12.22to10.6.22.csv") %>% mutate(trt="ic")
ib_fall <- read.csv("./data/accel/Inner_FB_Accel_9.12.22to10.6.22.csv") %>% mutate(trt="ib")
oc_fall <- read.csv("./data/accel/Outer_Cage_Accel_9.12.22to10.6.22.csv") %>% mutate(trt="oc")

accel_dfs <- list(ic_fall, ib_fall, oc_fall)

clean_accel_df <- function(df) {
  df %>% mutate(date=ymd_hms(ISO.8601.Time), accelX=Ax..g., accelY=Ay..g., accelZ=Az..g.) %>% 
    select(date, accelX, accelY, accelZ, trt)
}
     
cleaned_accel <- map(accel_dfs, clean_accel_df) %>% 
  bind_rows() %>% 
  group_by(trt) %>%  mutate(
        diffX = accelX - lag(accelX),
        diffY = accelY - lag(accelY),
        diffZ = accelZ - lag(accelZ),
        motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2) %>% na.omit()
cleaned_accel <- cleaned_accel %>% mutate(loc = if_else(trt=="oc", "Outside", "Inside"))

ggplot(data=cleaned_accel, aes(x = date, y = motionIndex, color=loc))+ 
  geom_line()+ 
  facet_wrap(~trt, ncol=1, labeller = labeller(trt=c("ib" = "Inside Bags", "ic"= "Inside Cages", "oc"="Outside Cages")))+
  theme_classic()+ labs(x = "", y = "Motion", color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  theme(legend.position="none")

ib_summer <- read.csv("./data/accel/InnerFB_Accel_6.09.22to9.09.22.csv") %>% mutate(trt="ib")  %>% clean_accel_df() %>% mutate(
  diffX = accelX - lag(accelX),
  diffY = accelY - lag(accelY),
  diffZ = accelZ - lag(accelZ), loc="Inside",
  motionIndex = sqrt((diffX)^2+(diffY)^2)+(diffZ)^2) %>% na.omit()

ggplot(data=ib_summer, aes(x = date, y = motionIndex))+ 
  geom_line()+
  theme_classic()+ labs(x = "", y = "Motion")

accel_plot <- ggplot(data=cleaned_accel, aes(x = date, y = motionIndex, color=loc))+ 
  geom_line()+ 
  geom_line(data=ib_summer, aes(x = date, y = motionIndex))+
  facet_wrap(~trt, ncol=1, labeller = labeller(trt=c("ib" = "Inside Bags", "ic"= "Inside Cages", "oc"="Outside Cages")))+
  theme_classic()+ labs(x = "", y = "Motion", color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  theme(legend.position="none")

# Accel Temp -------------------------------------------------------

ic_temp_june <- read.csv("./data/accel_temp/Inner_Cage_Temp_6.9.22.csv") %>% mutate(trt="ic", period="june")
ic_temp_oct <- read.csv("./data/accel_temp/Inner_Cage_Temp_10.6.22.csv") %>% mutate(trt="ic", period="oct")
ib_temp_oct <- read.csv("./data/accel_temp/Inner_FB_Temp_10.6.22.csv") %>% mutate(trt="ib", period="oct")
ib_temp_summer <- read.csv("./data/accel_temp/Inner_FB_Temp_AllSummer.csv") %>% mutate(trt="ib", period="summer")
oc_temp_oct <- read.csv("./data/accel_temp/Outer_Cage_Temp_10.6.22.csv") %>% mutate(trt="oc", period="oct")

accel_temp_dfs <- list(ic_temp_june, ic_temp_oct, ib_temp_oct, ib_temp_summer, oc_temp_oct)

clean_accel_temp <- function(df) {
  df %>% mutate(date=ymd_hms(ISO.8601.Time), temp=Temperature..C.) %>% 
    select(date, temp, trt, period) %>% mutate(loc = if_else(trt=="oc", "Outside", "Inside"))
}

cleaned_accel_temp <- map(accel_temp_dfs, clean_accel_temp) %>% 
  bind_rows() %>% na.omit()

ggplot(data=cleaned_accel_temp, aes(x = date, y = temp, color=loc))+ 
  geom_line()+ 
  facet_grid(period~trt)+
  theme_classic()+ labs(x = "", y = "Temperature (°C)", color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])

ib_temp <- cleaned_accel_temp %>% filter(trt=="ib")
ggplot()+
  geom_line(data=ib_temp, aes(x=date, y=temp), color="turquoise")+
  geom_line(data=noAir %>% filter(loc=="Inside"), aes(x=date, y=temp))+theme_classic()


oc_hobo <- hobo %>% filter(loc=="Outside", date > ymd_hms("2022-09-12 14:00:00")) %>% 
  mutate(date=round_date(date, unit = "second"), trt="hobo") %>% select(-c(date, sal)) 

oc_seahorse <- cleaned_accel_temp %>% filter(loc=="Outside") %>% 
  mutate(date=round_date(date, unit = "hour")) %>% group_by(date) %>% 
  summarise(temp=mean(temp)) %>% mutate(trt="seahorse")

oc_validation <- bind_rows(oc_hobo, oc_seahorse) %>% mutate(loc="Outside")

ggplot()+
  geom_line(data=oc_validation, aes(x=date, y=temp, color=trt))

ic_hobo <- hobo %>% filter(loc=="Inside", date > ymd_hms("2022-09-12 14:00:00")) %>% 
  mutate(date=round_date(date, unit = "second"), trt="hobo") %>% select(-c(date, sal)) 

ic_seahorse <- cleaned_accel_temp %>% 
  filter(trt=="ic", period=="oct") %>%
  mutate(date=round_date(date, unit = "hour")) %>% group_by(date) %>% 
  summarise(temp=mean(temp)) %>% 
  mutate(trt="seahorse")

ic_validation <- bind_rows(ic_hobo, ic_seahorse) %>% mutate(loc="Inside")

ggplot()+
  geom_line(data=ic_validation, aes(x=date, y=temp, color=trt))

seahorse_temp <- bind_rows(oc_validation, ic_validation) %>% 
  filter(trt!="hobo") %>% na.omit()
 # mutate(across(where(is.double), \(x) rollmean(x, k = 24, fill = NA))) %>% na.omit()

ggplot()+
   geom_line(data=seahorse_temp, aes(x=date, y=temp, color=loc))

seahorse_diff <- seahorse_temp %>% ungroup() %>% pivot_wider(names_from = loc, values_from = temp) %>% mutate(diff=Inside-Outside)

summary(seahorse_diff$diff %>% na.omit())

# All Enviro Data -------------------------------------------------------

chl_plot+
  (temp_plot+theme(legend.position="none"))+ sal_plot+
  turbidity_plot+
  spm_plot+ accel_plot+
  plot_layout(guides = 'collect')


# Mortality ---------------------------------------------------------------

m_data <- data.frame(read.csv("./data/bremen_mortality.csv")) %>% 
  dplyr::rename(num=num_remaining, loc=location) %>% 
  mutate(cage_id = if_else(trt %in% c("FBi", "FBo"), "FB", cage_id))

ggboxplot(m_data, x = "loc", y = "num", color="gear", add = "jitter")
ggboxplot(m_data, x = "gear", y = "num", color="loc", add = "jitter")


m_formula <- list(
  a = num ~ gear*loc+cage_id,
  b = num ~ gear+gear:cage_id,
  c = num~gear+cage_id,
  d = num~gear*cage_id,
  e= num~gear,
  g= num~loc,
  h= num~gear+loc)

m_models <- tibble(m_formula,
            models = map(m_formula, ~glmmTMB(data=m_data, formula=.x , family = poisson))) %>%
  add_column(id=names(m_formula), .before=1)

m_models <- m_models %>%
  mutate(tidy_model = map(models, tidy),
         AIC=map(models, AIC),
         resids = map(models, residuals)) %>% unnest(cols=c(AIC)) %>%
  mutate(resids = map(resids, as.numeric()),
         normal_p = map(resids, ~shapiro.test(.x)$p.value)) %>%
  unnest(cols=c(normal_p)) %>%
  mutate(logLik=map(models, ~(logLik(.x)[1]))) %>%
  mutate(r=map(models, ~r2(.x)[[1]])) %>% 
  unnest(cols=c(logLik)) %>% arrange(AIC)

m_mod <- glm(
  num ~ gear,
  family = poisson,
  data = m_data)

parameters(m_mod)
glance(m_mod)
summary(m_mod)
Anova(m_mod)
emmeans(m_mod, pairwise~gear, type="response")

check_residuals(m_mod)
check_outliers(m_mod)
check_overdispersion(m_mod)
testUniformity(m_mod)
r2(m_mod)
r2_efron(m_mod)

ggboxplot(m_data, x = "gear", y = "num", add = "jitter")+mytheme+
  labs(x="Gear", y="Survived")
  # + ylim(0,175) + stat_compare_means(method = "wilcox", label.x = 1.5, label.y = 20)

# Weight ---------------------------------------------------------------

w_data <- read.csv("./data/bremen_biofouling_ratios.csv")

w <- w_data %>% mutate(w = clean_wt_boat-weigh_boat,date=as.factor(date), 
                       loc=as.factor(location), gear=as.factor(gear)) %>% mutate(log_w=log(w))

ggboxplot(w, x = "location", y = "w", color="gear", add = "jitter")+facet_wrap(~date)
ggboxplot(w, x = "location", y = "w", add = "jitter")+facet_wrap(~date)
ggboxplot(w, x = "gear", y = "w", add = "jitter")+facet_wrap(~date)
ggboxplot(w, x = "gear", y = "w", color="location", add = "jitter")+facet_wrap(~date)

var_fac = c("date", "loc", "gear")
coll_fac(data = w,predictors=var_fac)
relat_single(data = w,response = "w", predictors = var_fac) 
distr_response(data = w, response="w") 

w1 <- glmmTMB(w ~ gear*loc*date, data = w)
w2 <- glmmTMB(w ~ gear*date+loc, data = w)
w3 <- glmmTMB(w ~ gear*date+loc, data = w, family="tweedie")
w4 <- glmmTMB(w ~ gear*date+loc, data = w, family="lognormal")
w5 <- lm(log_w ~gear*date+loc, data = w)

compare_performance(w1, w2, w3,w5,metrics = c("AIC", "AICc", "BIC", "RMSE"))
anova(w1, w2, w3)
anova(w3, w4, w5)

testUniformity(w5)
testResiduals(w5)
plot(simulateResiduals(w3))
check_residuals(w5) # or testResiduals(w2)
check_model(w5)
r2(w5)
r2_efron(w5)
emmip(w5, gear~loc)
emmip(w5, loc~gear)
Anova(w5)

tidy(w5)
joint_tests(w5)
tidy_avg_comparisons(w5)
create_supp_gt(w5)

# Condition Index ---------------------------------------------------------------

ci_data <- read.csv("./data/bremen_condition_index.csv")
ci <- ci_data %>% mutate(tissue = dry_tissue_in_T-t_boat,
                         wet_wt = clean_wt_in_S-s_boat,
                         shell = dry_shell_in_S-s_boat,
                         ci=tissue/(wet_wt-shell)*100,
                         gear=as.factor(gear), date=as.factor(date), loc=as.factor(loc),
                         log_ci = log(ci)) %>% 
  filter(gear!="initial")

ggboxplot(ci, x = "loc", y = "ci", color="gear", add = "jitter")+facet_wrap(~date)
ggboxplot(ci, x = "loc", y = "ci", add = "jitter")+facet_wrap(~date)
ggboxplot(ci, x = "gear", y = "ci", add = "jitter")+facet_wrap(~date)
ggboxplot(ci, x = "gear", y = "ci", color="loc", add = "jitter")+facet_wrap(~date)

coll_fac(data = ci, predictors=var_fac)
relat_single(data = ci,response = "ci", predictors = var_fac) 
distr_response(data = ci, response="ci") 

ci1 <- lm(ci ~gear*date*loc, data = ci)
ci2 <- lm(ci ~gear+date+loc, data = ci)
ci3 <- glmmTMB(ci ~gear*date*loc, data = ci, family = tweedie)
ci4 <- glmmTMB(ci ~gear+loc+date, data = ci, family = tweedie)
compare_performance(ci1, ci2, ci3,ci4,metrics = c("AIC", "AICc", "BIC", "RMSE"))

summary(ci3)
check_model(ci3)
check_residuals(ci3)
testUniformity(ci3)
testResiduals(ci3)
plot(simulateResiduals(ci3))
joint_tests(ci3)
r2_efron(ci3)

emmip(ci3, gear~loc)
emmip(ci3, loc~gear)

emm <- emmeans(ci3, specs="gear", type="response")
pairs(emm, reverse=TRUE) %>% tidy()

#Supplementary tables
create_supp_gt(ci3)

# Growth ---------------------------------------------------------------

g_data <- read.csv("./data/bremen_growth.csv") %>% 
  mutate(cup_ratio=width/height,
         fan_ratio=length/height,
         sum_dims = length+width+height,
         chi=((sum_dims*0.5-height)^2/(sum_dims*0.5))+
           ((sum_dims*(1/3)-length)^2/(sum_dims/3))+
           ((sum_dims*(1/6)-width)^2/(sum_dims/6)),
         date = mdy(date))

g <- g_data %>% 
  group_by(date, location, gear) %>% 
  dplyr::summarize(across(c(height, length, width, cup_ratio, fan_ratio, chi), 
                   list(mean=mean, sd=sd))) %>% ungroup()



ggplot()+
  geom_line(data=g, aes(x=date, y=height_mean, color=location, linetype = gear))

ggplot()+
  geom_line(data=g, aes(x=date, y=cup_ratio_mean, color=location, linetype = gear))
ggplot()+
  geom_boxplot(data=g, aes(x=gear, y=cup_ratio_mean, color=location))

ggplot()+
  geom_line(data=g, aes(x=date, y=fan_ratio_mean, color=location, linetype = gear))

ggplot()+
  geom_line(data=g, aes(x=date, y=chi_mean, color=location, linetype = gear))

M1 = cor(g %>% select(ends_with("mean")), method = "s")
colnames(M1) <- c("Height", "Length", "Width", "Cup ratio", "Fan ratio", "Shell shape")
rownames(M1) <-  c("Height", "Length", "Width", "Cup ratio", "Fan ratio", "Shell shape")

testM1 = cor.mtest(g %>% select(ends_with("mean")), conf.level = 0.95, method = "s")

colnames(testM1$p) <- c("Height", "Length", "Width", "Cup ratio", "Fan ratio", "Shell shape")
rownames(testM1$p) <-  c("Height", "Length", "Width", "Cup ratio", "Fan ratio", "Shell shape")

corrplot(M1, 
         #type="lower",
         #method = 'circle', 
         #method = "number",
         method = "color",
         #order = 'AOE',
        order = "FPC",
        # order="hclust",
         diag = FALSE,
         p.mat = testM1$p, 
         #insig = 'label_sig', 
         insig = "blank", 
         #sig.level = c(0.001, 0.01, 0.05),
         addCoef.col = "black",
         pch.cex = 2,
         tl.srt=45,
         tl.col = 'black',
         tl.offset = 1)

ggpairs(g %>% select(ends_with("mean")))

g_cols <- c("height", "length", "width", "cup_ratio", "fan_ratio", "chi")

M2 = cor(g_data %>% select(all_of(g_cols)), method = "s")
colnames(M2) <- c("Height", "Length", "Width", "Cup ratio", "Fan ratio", "Shell shape")
rownames(M2) <-  c("Height", "Length", "Width", "Cup ratio", "Fan ratio", "Shell shape")

testM2 = cor.mtest(g_data %>% select(all_of(g_cols)), conf.level = 0.95, method = "s", exact=FALSE)

colnames(testM2$p) <- c("Height", "Length", "Width", "Cup ratio", "Fan ratio", "Shell shape")
rownames(testM2$p) <-  c("Height", "Length", "Width", "Cup ratio", "Fan ratio", "Shell shape")

correlation(g_data %>% select(all_of(g_cols)), method="spearman") %>% summary(redundant=TRUE) %>% plot()

g_data %>% 
  group_by(gear, location) %>% 
  select(all_of(c(g_cols, "gear", "location"))) %>% 
  correlation(method="spearman") #%>% arrange(rho)

g_data %>% 
  group_by(gear) %>% 
  select(all_of(c(g_cols, "gear"))) %>% 
  correlation() #%>% arrange(rho)

g_data %>% 
  group_by(location) %>% 
  select(all_of(c(g_cols, "location"))) %>% 
  correlation()

corrplot(M2, 
         #type="lower",
       # method = 'circle', 
         #method = "number",
         method = "color",
         #order = 'AOE',
         order = "FPC",
         # order="hclust",
         diag = FALSE,
         p.mat = testM2$p, 
         #insig = 'label_sig', 
         insig = "blank", 
         sig.level = c(0.001, 0.01, 0.05),
         addCoef.col = "black",
         pch.cex = 2,
         tl.srt=45,
         tl.col = 'black',
         tl.offset = 1)



# Env correlations --------------------------------------------------------

add_sample <- function(df) {
  df %>% mutate(sample = case_when(
    date %in% as_date(c("2022-06-14"))~1,
    date %in% as_date(c("2022-06-22", "2022-06-29", "2022_07-05","2022-07-06"))~2,
    date %in% as_date(c("2022-07-12","2022-07-19","2022-07-26","2022-07-27"))~3,
    date %in% as_date(c("2022-08-02","2022-08-10","2022-08-15","2022-08-16"))~4,
    date %in% as_date(c("2022-09-13","2022-09-15"))~5,
    date %in% as_date(c("2022-09-28", "2022-10-17"))~6
  ))
}

t_match <- t_data %>%
  add_sample %>% 
  group_by(sample, loc) %>% 
  summarise(turbidity = mean(turbidity)) %>% ungroup()

chl_match <- chlA %>% 
  add_sample %>% 
  group_by(sample, loc) %>%
  summarise(chl = mean(chl)) %>% ungroup()

SPM_match <- SPM_final_data %>%
  select(!contains("TOTAL")) %>%
  dplyr::rename(loc=Type) %>% 
  add_sample %>% 
  group_by(date, loc) %>%
  summarise(across(where(is.double),mean)) %>% ungroup()



t %>% 
  dplyr::rename(turb=mean) %>% 
  select(-sd) %>% 
  full_join(chl %>% select(-sd) %>% dplyr::rename(chl=mean), by=c("date", "loc")) %>% 
  full_join(SPM_final %>% select(!contains("SD"))) %>% 
  ungroup() %>% select(-date) %>% 
  correlation(method="spearman")


temp_match <- noAir %>% mutate(
  sample = case_when(
    date >= as_datetime("2022-06-08 19:00:00") & date <= as_datetime("2022-06-14 23:00:00")~1,
    date >= as_datetime("2022-06-15 0:00:00") & date <= as_datetime("2022-07-06 23:00:00")~2,
    date >= as_datetime("2022-07-07 0:00:00") & date <= as_datetime("2022-07-27 23:00:00")~3,
    date >= as_datetime("2022-07-28 0:00:00") & date <= as_datetime("2022-08-16 23:00:00")~4,
    date >= as_datetime("2022-08-17 0:00:00") & date <= as_datetime("2022-09-15 23:00:00")~5,
    date >= as_datetime("2022-09-16 0:00:00") & date <= as_datetime("2022-10-17 23:00:00")~6,
  )) %>% group_by(sample, loc) %>% summarise(temp=mean(temp))


all_env <- t_match %>% 
  full_join(chl_match) %>% 
  full_join(SPM_match) %>% 
  full_join(temp_match) %>% 
  select(turbidity, chl, TPM_mg_ml, POM_mg_ml, PIM_mg_ml,temp, loc, sample) %>% 
  mutate(location=if_else(loc=="Inside", "in", "out"))

all_env %>% select(-sample) %>% 
 correlation(method="spearman")

df <- g_data %>% add_sample %>% left_join(all_env, by=c("location", "sample"))
