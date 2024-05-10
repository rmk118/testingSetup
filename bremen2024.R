#Updated analysis of data from Bremen oyster experiment
#Last modified: May 8, 2024
#Ruby Krasnow

library(tidyverse)
library(patchwork)


# Temp and Salinity -------------------------------------------------------

hobo <- read.csv("./data/hobo.csv") %>% 
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

sal_plot <- ggplot(noAir_roll, aes(x=dt, y=sal, color=loc))+
  geom_line()+
  theme_classic()+ labs(x = NULL, y = "Salinity", color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  theme(legend.position="none")

ggplot(common_roll, aes(x=dt, y=temp_diff))+geom_line()

temp_plot <-ggplot(noAir_roll, aes(x=dt, y=temp, color=loc))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  labs(x=NULL,y="Temperature (°C)", color="Location")



# Turbidity -------------------------------------------------------

t <- read.csv("./data/turbidity.csv") %>% 
  mutate(date=mdy(Date), loc=as.factor(Location),turbidity=Turbidity,.keep="unused")

t <- t %>% group_by(date, loc) %>% summarise(mean=mean(turbidity), sd=sd(turbidity))

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

chl <- chlA %>%
  mutate(chl = ChlFs * (FoFa_max/(FoFa_max-1)) * (Fo-Fa)* ((Acetone_vol)/Vol_Filtered))

chl <- chl %>% 
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
SPM <- SPM_data %>% mutate(TPM_mg = Dry_wt-Filter_wt, POM_mg = Dry_wt-Ashed_wt, Date=mdy(Date))

# Separate and average for formate Blank TPM & POM

blank_df <- SPM %>%
  filter(Type %in% c("Formate Blank", "True Blank"))

true_blank_df <- SPM %>%
  filter(Type == 'True Blank') %>%
  group_by(Date) %>%
  summarize(TB_TPM = mean(TPM_mg, na.rm = TRUE),
            TB_POM =mean(POM_mg, na.rm = TRUE))

formate_blank_df <- SPM %>%
  filter(Type == 'Formate Blank') %>%
  group_by(Date) %>%
  summarize(FB_TPM = mean(TPM_mg, na.rm = TRUE),
            FB_POM =mean(POM_mg, na.rm = TRUE))

SPM <- SPM %>%
  left_join(formate_blank_df, by='Date') %>%
  left_join(true_blank_df, by='Date')

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

SPM_final <- SPM %>% 
  filter(Type %in% c("Inside", "Outside")) %>% 
  select(Date, Type, TPM_TOTAL_mg, POM_TOTAL_mg, PIM_TOTAL_mg, TPM_mg_ml, POM_mg_ml, PIM_mg_ml)

SPM_final <- SPM_final %>%
  group_by(Date, Type) %>%
  summarise(Ave_TOT_TPM_mg = mean(TPM_TOTAL_mg), 
            #TOT_TPM_SE = std.error(TPM_TOTAL_mg),
            TOT_TPM_SD = sd(TPM_TOTAL_mg),
            Ave_TOT_POM_mg = mean(POM_TOTAL_mg), 
            TOT_POM_SD = sd(POM_TOTAL_mg),
            Ave_TOT_PIM_mg = mean(PIM_TOTAL_mg), 
            TOT_PIM_SD = sd(PIM_TOTAL_mg),
            TPM_mg_L = mean(TPM_mg_ml*1000), 
            TPM_mg_L_SD = sd(TPM_mg_ml*1000),
            POM_mg_L = mean(POM_mg_ml*1000), 
            POM_mg_L_SD = sd(POM_mg_ml*1000),
            PIM_mg_L = mean(PIM_mg_ml*1000), 
            PIM_mg_L_SD = sd(PIM_mg_ml*1000))

# Theme for creating extra classy gg plots
mytheme <- theme_classic()+theme(legend.position = "bottom",
                 axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))

TPM_mg_L <- ggplot(SPM_final) +
  geom_point(aes(x = Date, y = TPM_mg_L, colour = Type)) +
  geom_errorbar(aes(x = Date, ymin = TPM_mg_L-TPM_mg_L_SD, ymax = TPM_mg_L+TPM_mg_L_SD, color = Type),
                width = 0.2) +
  geom_line(aes(x = Date, y = TPM_mg_L, colour = Type, group = Type)) +
  ylab("TPM (mg/L)") + labs(x=NULL, color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+ 
  mytheme

POM_mg_L <- ggplot(SPM_final) +
  geom_point(aes(x = Date, y = POM_mg_L, colour = Type)) +
  geom_errorbar(aes(x = Date, ymin = POM_mg_L-POM_mg_L_SD, ymax = POM_mg_L+POM_mg_L_SD, color = Type),
                width = 0.2) +
  geom_line(aes(x = Date, y = POM_mg_L, colour = Type, group = Type)) +
  ylab("POM (mg/L)") + labs(x=NULL, color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  mytheme

PIM_mg_L <- ggplot(SPM_final) +
  geom_point(aes(x = Date, y = PIM_mg_L, colour = Type)) +
  geom_errorbar(aes(x = Date, ymin = PIM_mg_L-PIM_mg_L_SD, ymax = PIM_mg_L+PIM_mg_L_SD, color = Type),
                width = 0.2) +
  geom_line(aes(x = Date, y = PIM_mg_L, colour = Type, group = Type)) +
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
  geom_line(data=noAir %>% filter(loc=="Inside"), aes(x=dt, y=temp))+theme_classic()


oc_hobo <- hobo %>% filter(loc=="Outside", dt > ymd_hms("2022-09-12 14:00:00")) %>% 
  mutate(date=round_date(dt, unit = "second"), trt="hobo") %>% select(-c(dt, sal)) 

oc_seahorse <- cleaned_accel_temp %>% filter(loc=="Outside") %>% 
  mutate(date=round_date(date, unit = "hour")) %>% group_by(date) %>% 
  summarise(temp=mean(temp)) %>% mutate(trt="seahorse")

oc_validation <- bind_rows(oc_hobo, oc_seahorse) %>% mutate(loc="Outside")

ggplot()+
  geom_line(data=oc_validation, aes(x=date, y=temp, color=trt))

ic_hobo <- hobo %>% filter(loc=="Inside", dt > ymd_hms("2022-09-12 14:00:00")) %>% 
  mutate(date=round_date(dt, unit = "second"), trt="hobo") %>% select(-c(dt, sal)) 

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

m_data <- read.csv("./data/bremen_mortality.csv")

ggboxplot(m_data, x = "location", y = "num_remaining", color="gear", add = "jitter")
ggboxplot(m_data, x = "gear", y = "num_remaining", color="location", add = "jitter")


# Weight ---------------------------------------------------------------

w_data <- read.csv("./data/bremen_biofouling_ratios.csv")

# Condition Index ---------------------------------------------------------------

ci_data <- read.csv("./data/bremen_condition_index.csv")

# Growth ---------------------------------------------------------------

g_data <- read.csv("./data/bremen_growth.csv")