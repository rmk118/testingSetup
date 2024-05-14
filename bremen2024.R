#Updated analysis of data from Bremen oyster experiment
#Last modified: May 12, 2024
#Ruby Krasnow

library(tidyverse)
library(patchwork)
library(corrplot)
library(GGally)
library(Hmisc)


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

# ggplot(common, aes(x=dt, y=sal_diff))+geom_line()
# ggplot(noAir, aes(x=dt, y=sal, color=loc))+geom_line()
 ggplot(hobo, aes(x=dt, y=sal, color=loc))+geom_line()+
   theme_classic()+ labs(x = NULL, y = "Salinity", color="Location") +
   scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])
# ggboxplot(hobo, x = "loc", y = "sal", ylab = "Salinity", xlab = "Locations", add = "jitter")
# 
# ggplot(common, aes(x=dt, y=temp_diff))+geom_line()
# ggplot(noAir, aes(x=dt, y=temp, color=loc))+geom_line()+theme_bw()
# ggplot(hobo, aes(x=dt, y=temp, color=loc))+geom_line()+theme_bw()
# ggboxplot(hobo, x = "loc", y = "temp", ylab = "Temp", xlab = "Locations", add = "jitter")

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



temp_plot <-ggplot(noAir_roll, aes(x=dt, y=temp, color=loc))+
  geom_line()+
  theme_classic()+
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])+
  labs(x=NULL,y="Temperature (°C)", color="Location")



# Turbidity -------------------------------------------------------

t_data <- read.csv("./data/turbidity.csv") %>% 
  mutate(date=mdy(Date), loc=as.factor(Location),turbidity=Turbidity,.keep="unused")

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

SPM_final_data <- SPM %>% 
  filter(Type %in% c("Inside", "Outside")) %>% 
  select(Date, Type, TPM_TOTAL_mg, POM_TOTAL_mg, PIM_TOTAL_mg, TPM_mg_ml, POM_mg_ml, PIM_mg_ml)

SPM_final <- SPM_final_data %>%
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

w <- w_data %>% mutate(w = clean_wt_boat-weigh_boat)

ggboxplot(w, x = "location", y = "w", color="gear", add = "jitter")+facet_wrap(~date)
ggboxplot(w, x = "gear", y = "w", color="location", add = "jitter")+facet_wrap(~date)


# Condition Index ---------------------------------------------------------------

ci_data <- read.csv("./data/bremen_condition_index.csv")
ci <- ci_data %>% mutate(tissue = dry_tissue_in_T-t_boat,
                         wet_wt = clean_wt_in_S-s_boat,
                         shell = dry_shell_in_S-s_boat,
                         ci=tissue/(wet_wt-shell)*100) %>% filter(gear!="initial")

ggboxplot(ci, x = "loc", y = "ci", color="gear", add = "jitter")+facet_wrap(~date)
ggboxplot(ci, x = "gear", y = "ci", color="loc", add = "jitter")+facet_wrap(~date)

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
  summarize(across(c(height, length, width, cup_ratio, fan_ratio, chi), 
                   list(mean=mean, sd=sd))) %>% ungroup()



ggplot()+
  geom_line(data=g, aes(x=date, y=height_mean, color=location, linetype = gear))

ggplot()+
  geom_line(data=g, aes(x=date, y=cup_ratio_mean, color=location, linetype = gear))

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

corrplot(M2, 
         #type="lower",
        method = 'circle', 
         #method = "number",
         #method = "color",
         #order = 'AOE',
         order = "FPC",
         # order="hclust",
         diag = FALSE,
         p.mat = testM2$p, 
         insig = 'label_sig', 
         #insig = "blank", 
         sig.level = c(0.001, 0.01, 0.05),
         #addCoef.col = "black",
         pch.cex = 2,
         tl.srt=45,
         tl.col = 'black',
         tl.offset = 1)



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

# Plot all pictures
par(mfrow=c(2,3))

lapply(M3, function(x) {
  corrplot(x$r,tl.col="black", tl.cex=0.7,tl.srt=45, p.mat = x$P, insig = "blank")})

corrplot(M3$BP.Inside$r, diag = FALSE, type="lower", p.mat = M3$BP.Inside$P, insig = "blank")

par(mfrow=c(2,3))
lapply(M3, \(x) function(x) {
  ggcorr(x,label=TRUE, label_round=2)})

corrplot(all_monthly %>% 
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
