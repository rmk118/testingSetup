# What is up with the salinity data?
# Ruby Krasnow
# Last updated: 16 May 2024

library(tidyverse)
library(patchwork)
library(easystats)
library(rtide)
library(scales)

hobo <- read.csv("./data/hobo.csv") %>% 
  mutate(date=mdy_hms(Date.time, tz="America/New_York"), temp=Temp, sal=High.sal,loc=Location,.keep="unused")

roll <- hobo %>% group_by(loc) %>% mutate(across(where(is.double), \(x) rollmean(x, k = 24, fill = NA))) %>% na.omit() %>% mutate(date=floor_date(date, unit="hours"))

correlation(hobo)

hobo %>% group_by(loc) %>% shapiro_test(temp)
gghistogram(data=hobo, x="temp")+facet_wrap("loc")

hobo %>% 
  arrange(temp) %>% 
  mutate(day = date(date)) %>% 
  select(day) %>% 
  distinct() %>% 
 slice_head(n=20)

ggplot(hobo, aes(x=date, y=(temp*1.8+32), color=loc))+geom_line()+
  theme_classic()+ labs(x = NULL, y = "Temperature (°F)", color="Location") +
  scale_color_manual(values=pnw_palette(name="Sailboat",n=4,type="discrete")[c(2,4)])

# Tidal data from rtide package -------------------------------

# tides<-tide_height("Thomaston", from = as.Date("2022-06-08"), to = as.Date("2022-10-17"),
#             minutes = 60L, tz = "EST5EDT") 

tides<-tide_height("Fort Popham", from = as.Date("2022-06-08"), to = as.Date("2022-10-17"),
                   minutes = 60L, tz = "EST5EDT") 

hobo <- tides %>% dplyr::rename(date = DateTime, tide=TideHeight) %>% select(-Station) %>% right_join(hobo)

hobo_roll <- tides %>% dplyr::rename(date = DateTime, tide=TideHeight) %>% select(-Station) %>% right_join(roll)

correlation(hobo, method="spearman")
correlation(hobo %>% group_by(loc), method="spearman")

noAir <- hobo %>% filter(sal>5)
correlation(noAir, method="spearman")
correlation(noAir %>% group_by(loc), method="spearman")

ggplot(data = hobo %>% filter(loc=="Inside"), aes(x =date, y =tide)) +
  geom_line() +
  scale_x_datetime(
    name = "Date",
    labels = date_format("%d %b %Y", tz = "EST5EDT")
  ) +
  scale_y_continuous(name = "Tide Height (m)")

correlation(hobo_roll, method="spearman")
correlation(hobo_roll %>% group_by(loc), method="spearman")

hobo_roll %>% filter(loc=="Outside") %>% select(tide, sal) %>% correlation()


# Tidal data from US Harbors for Round Pond -------------------------------

rp_data <- read.csv("./data/round_pond_tides.csv")

rp <- rp_data %>% mutate(date1 = paste(Month, Day, "2022"),
                         date=mdy(date1))
                         #.keep="unused")
high_am <- rp_data %>% 
  select(high_time_am, high_ft_am, Month, Day) %>% 
  na.omit() %>% 
  mutate(time = paste(Month, Day, 2022, high_time_am, "AM"),
         time=mdy_hm(time, tz="America/New_York"), tide=high_ft_am, .keep="unused")

high_pm <- rp_data %>% 
  select(high_time_pm, high_ft_pm, Month, Day) %>% 
  na.omit() %>% 
  mutate(time = paste(Month, Day, 2022, high_time_pm, "PM"),
         time=mdy_hm(time, tz="America/New_York"), tide=high_ft_pm, .keep="unused")

low_am <- rp_data %>% 
  select(low_time_am, low_ft_am, Month, Day) %>% 
  na.omit() %>% 
  mutate(time = paste(Month, Day, 2022, low_time_am, "AM"),
         time=mdy_hm(time, tz="America/New_York"), tide=low_ft_am, .keep="unused")

low_pm <- rp_data %>% 
  select(low_time_pm, low_ft_pm, Month, Day) %>% 
  na.omit() %>% 
  mutate(time = paste(Month, Day, 2022, low_time_pm, "PM"),
         time=mdy_hm(time, tz="America/New_York"), tide=low_ft_pm, .keep="unused")

rp_tides <- bind_rows(high_am, high_pm, low_am, low_pm) %>% 
  mutate(date=lubridate::round_date(time, unit="hour"), tide_rp=tide, .keep="unused")

ggplot()+
  geom_line(data=rp_tides, aes(x=date, y=tide_rp))

rp_tides <- rp_tides %>% full_join(hobo) %>% na.omit()
correlation(rp_tides)


# Newcastle weather -------------------------------------------------------




