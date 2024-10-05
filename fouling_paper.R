#---------------------------------------------------#
# Interacting effects of environment and cultivation method on biofouling of farmed oysters
#  Ruby Krasnow
#  Last updated October 5, 2024
#---------------------------------------------------#

## List of packages required:
packages <- c(
  "tidyverse",
  "glmmTMB",
  "emmeans",
  "performance",
  "plotrix",
  "MASS",
  "DHARMa",
  "rstatix",
  "PNWColors",
  "broom.mixed",
  "gt",
  "marginaleffects",
  "zoo"
)

# Load packages into session
lapply(packages, require, character.only = TRUE)

set.seed(123)


# Temperature -------------------------------------------------------

# Import data and rename variables
hobo <- read.csv("./data/hobo.csv") %>%
  mutate(
    date = mdy_hms(Date.time),
    temp = Temp,
    sal = High.sal,
    loc = Location,
    .keep = "unused"
  )

# Excludes observations where the temperature was erroneously high due to air
# exposure i.e., the sensor was measuring air temperature instead of water
# temperature
noAir <- hobo %>% filter(sal > 5) 

# Find hours when both sensors logged with no issues
Inside <- noAir %>% filter(loc == "Inside") %>% 
  dplyr::rename(sal_in = sal, temp_in = temp)

Outside <- noAir %>% filter(loc == "Outside") %>% 
  dplyr::rename(sal_out = sal, temp_out = temp)

# Find the difference between the temperature inside and outside the pound
common <- inner_join(Inside, Outside, by = "date") %>% 
  select(-c("loc.x", "loc.y")) %>% 
  mutate(temp_diff = temp_in - temp_out) 

# Total number of observations
total_obs <- length(common$temp_diff)

# Mean and SD of difference in-out
mean(common$temp_diff)
sd(common$temp_diff)

# Number of observations where the temperature inside the pound was warmer than outside
inside_greater <- length(common %>% filter(temp_diff > 0) %>% pull(temp_diff))

# Percentage of total where inside was warmer than outside
inside_greater / total_obs

# Calculate the rolling daily mean
noAir_roll <- noAir %>% group_by(loc) %>%
  mutate(across(where(is.double), \(x) zoo::rollmean(x, k = 24, fill = NA))) %>%
  na.omit()

# Minimum and maximum temperatures in each location
noAir %>%
  group_by(loc) %>%
  summarise(min = min(temp), max = max(temp))

# FIGURE 1 -  Temperature plot
ggplot(noAir_roll, aes(x = date, y = temp, color = loc)) +
  geom_line() +
  theme_classic() +
  scale_color_manual(values = pnw_palette(name = "Sailboat", n = 4, type =
                                            "discrete")[c(2, 4)]) +
  labs(x = NULL, y = "Temperature (°C)", color = "Location") +
  theme(axis.title.y = element_text(margin = margin(r = 10)))

# Plot - not rolling
common %>%
  pivot_longer(cols = c(temp_in, temp_out), names_to = "location") %>%
  ggplot() +
  geom_line(aes(x = date, y = value, color = location)) +
  theme_classic() +
  scale_color_manual(values = pnw_palette(name = "Sailboat", n = 4, type = "discrete")[c(2, 4)]) +
  labs(x = NULL, y = "Temperature (°C)", color = "Location") +
  theme(axis.title.y = element_text(margin = margin(r = 10)))

common %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(
    mean_diff = mean(temp_diff),
    sd_diff = sd(temp_diff),
    temp_in = mean(temp_in),
    temp_out = mean(temp_out)
  )


# Accelerometer -------------------------------------------------------

# Import data
ic_fall <- read.csv("./data/accel/Inner_Cage_Accel_9.12.22to10.6.22.csv") %>% 
  mutate(trt = "ic") #Inside cages
ib_fall <- read.csv("./data/accel/Inner_FB_Accel_9.12.22to10.6.22.csv") %>% 
  mutate(trt = "ib") #Inside bags
oc_fall <- read.csv("./data/accel/Outer_Cage_Accel_9.12.22to10.6.22.csv") %>% 
  mutate(trt = "oc") #Outside cages

accel_dfs <- list(ic_fall, ib_fall, oc_fall)

# This function converts the time column into the correct format, renames
# variables, and selects only the relevant columns
clean_accel_df <- function(df) {
  df %>% mutate(
    date = ymd_hms(ISO.8601.Time),
    accelX = Ax..g.,
    accelY = Ay..g.,
    accelZ = Az..g.
  ) %>%
    select(date, accelX, accelY, accelZ, trt)
}

# This applies the clean_accel_df function to all three data frames in the
# accel_dfs list and combines them into one data frame
cleaned_accel <- map(accel_dfs, clean_accel_df) %>%
  bind_rows()
  
# Calculate the relative orbital velocity, as the SD of the magnitude of the
# accelerations each hour
cleaned_accel <- cleaned_accel %>%
  mutate(
    day = date(date),
    hour = lubridate::round_date(date, unit = "hour"),
    magnitude = sqrt((accelX) ^ 2 + (accelY) ^ 2 + (accelZ) ^ 2)
  ) %>%
  group_by(trt, hour) %>%
  # calculate SD and multiply by standard gravity = 9.80665 m/s^2
  dplyr::summarize(orb = sd(magnitude) * 9.80665) %>%
  # remove outliers from initial deployment
  filter(hour > ymd_hms("2022-09-12 15:00:00"))

# Create location column from treatment names
cleaned_accel <- cleaned_accel %>%
  mutate(loc = if_else(trt == "oc", "Outside", "Inside"))

# FIGURE 2 - Accelerometer
ggplot(data = cleaned_accel, aes(x = hour, y = orb, color = loc)) +
  geom_line() +
  facet_wrap( ~ trt, ncol = 1, labeller = labeller(
    trt = c("ib" = "Inside Bags", "ic" = "Inside Cages", "oc" = "Outside Cages")
  )) +
  theme_classic() + labs(x = "",
                         y = bquote('Hourly SD' ~ (m / s ^ 2)),
                         color = "Location") +
  scale_color_manual(values = pnw_palette(name = "Sailboat", n = 4, type =
                                            "discrete")[c(2, 4)]) +
  theme(axis.title.y = element_text(margin = margin(r = 10)))

# Find the mean and SD of the motion index for each treatment
cleaned_accel %>%
  group_by(trt) %>%
  summarise(motion = mean(orb), motion_sd = sd(orb)) %>%
  mutate(across(c(motion, motion_sd), ~ round(.x, digits = 3)))

# Type/abundance of hard biofouling ---------------------------------------------------

# Import data
types <- read_csv("./data/bremen_biofouling_type.csv", show_col_types = FALSE)

types <- types %>%
  mutate(across(where(is.double), ~ replace_na(.x, 0))) %>% # replace NA values with 0
  mutate(date = mdy(date)) %>% # convert column to date format
  mutate(
    gear = factor(gear, levels = c('BP', 'FC', 'FB')),
    trt = as.factor(trt),
    location = as.factor(location),
    slippers = slipper,
    .keep = "unused"
  ) # convert to factors and rename column for consistency

# Turn missing rows into explicit 0s
grid <- expand_grid(
  date = unique(types$date),
  gear = levels(types$gear),
  location = levels(types$location),
  oyster_num = 1:20
)

types_filled <- right_join(types %>% group_by(date, trt, gear, location) %>%
                             mutate(oyster_num = row_number()),
                           grid) %>%
  arrange(date, gear, location, oyster_num) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

types_long <- types_filled %>%
  select(-c(comments, trt)) %>%
  pivot_longer(
    cols = c(slippers, mussels, barnacles, tunicates),
    values_to = "num",
    names_to = "organism"
  )

types_means <- types_long %>%
  select(-trt) %>%
  group_by(gear, location, organism) %>%
  summarise(mean = mean(num),
            sd = sd(num),
            se = plotrix::std.error(num))

# FIGURE 5 - Mean (±SE) number of each organism per oyster
ggplot(data = types_means  %>%
         mutate(organism = case_match(organism, "slippers" ~ "snails", .default = organism))) +
  geom_bar(aes(x = gear, y = mean, color = str_to_title(organism), fill = str_to_title(organism)),
    stat = "identity",
    position = "dodge",
    alpha = 0.5) +
  geom_errorbar(aes(x = factor(gear, levels = c('BP', 'FC', 'FB')), ymin = mean - se, 
        ymax = mean + se, color = str_to_title(organism)),
    position = position_dodge(0.9), width = 0.25, show.legend = FALSE) +
  facet_wrap( ~ location, labeller = labeller(location = c("in" = "Inside", "out" =
                                                             "Outside"))) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 13),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0))) +
  labs(x = "Gear type", y = "Mean # per oyster", fill = NULL, color = NULL) +
  scale_color_manual(values = pnw_palette("Sailboat", 4)) +
  scale_fill_manual(values = pnw_palette("Sailboat", 4))


# Fouling ratio -----------------------------------------------------------

# Import data
ratios_raw <- read_csv("./data/bremen_biofouling_ratios.csv", show_col_types = FALSE)

ratios <- ratios_raw %>% mutate(
  fouling_wt = dirty_wt_boat - clean_wt_boat,
  whole_wet_wt = clean_wt_boat - weigh_boat,
  prop_fouling = fouling_wt / whole_wet_wt,
  gear = as.factor(gear),
  loc = as.factor(location),
  date = as.factor(date),
  .keep = "unused"
)

# Remove negative values 
ratios <- ratios %>% filter(prop_fouling > 0) 

# FIGURE 3 -  Fouling ratio by gear type on each sampling date
ggplot(data = ratios)+
  geom_boxplot(aes(
    x = factor(gear, levels = c('BP', 'FC', 'FB')),
    y = prop_fouling,
    color = loc))+
  facet_wrap( ~ factor(date, levels = c('8/15/22', '10/17/22')))+
  theme_bw()+
  theme(text = element_text(size = 13), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(x = "Gear type",
       y = bquote("Fouling ratio (g fouling" ~ g ^ -1 ~ "oyster WW)"),
       color = "Location")+
  scale_color_manual(
    values = pnw_palette(name = "Sailboat", n = 4, type = "discrete")[c(2, 4)],
    labels = c("Inside", "Outside"))

# Mean fouling ratio for FB outside the pound in October
ratios %>%
  filter(date == "10/17/22", gear == "FB", loc == "out") %>%
  summarize(mean = mean(prop_fouling))

## Model formulation ####

mod <- glmmTMB(prop_fouling ~ date * gear * loc,
               data = ratios,
               family = beta_family())

# Checking other formulations

beta_formula <- list(
  full = prop_fouling ~ date * gear * loc,
  add_only = prop_fouling ~ gear + loc + date,
  no_date = prop_fouling ~ gear * loc,
  no_loc = prop_fouling ~ gear * date,
  no_gear = prop_fouling ~ loc * date)

beta_models <- tibble(beta_formula,
                      models = purrr::map(
  beta_formula, \(x)
  glmmTMB(
    data = ratios,
    formula = x ,
    family = beta_family()
  ))
) %>%
  add_column(id = names(beta_formula), .before = 1)

beta_models2 <- beta_models %>%
  mutate(tidy_model = map(models, tidy),
         AIC=map(models, AIC),
         resids = map(models, residuals)) %>% unnest(cols=c(AIC)) %>%
  mutate(resids = map(resids, as.numeric()),
         normal_p = map(resids, ~shapiro.test(.x)$p.value)) %>%
  unnest(cols=c(normal_p)) %>%
  mutate(logLik=map(models, ~(logLik(.x)[1]))) %>%
  unnest(cols=c(logLik)) %>% arrange(AIC)

# The full model (including all interaction terms) had the lowest AIC and highest logLik

# Checking other dispersion formulas

disp_formula <- list(
  full = ~ date * gear * loc,
  add_only =  ~ gear + loc + date,
  no_date =  ~ gear * loc,
  no_loc =  ~ gear * date,
  no_gear = ~ loc * date
)

disp_models_fouling <- tibble(disp_formula, models = map(
  disp_formula,
  ~ glmmTMB(
    data = ratios,
    formula = prop_fouling ~ date *
      gear * loc,
    dispformula = .x,
    family = beta_family()
  )
)) %>%
  add_column(id = names(disp_formula), .before = 1)

disp_models_fouling2 <- disp_models_fouling %>%
  mutate(
    tidy_model = map(models, broom.mixed::tidy),
    AIC = map(models, AIC),
    resids = map(models, residuals)
  ) %>% unnest(cols = c(AIC)) %>%
  mutate(
    resids = map(resids, as.numeric()),
    normal_p = map(resids, ~ shapiro.test(.x)$p.value)
  ) %>%
  unnest(cols = c(normal_p)) %>%
  mutate(logLik = map(models, ~ (logLik(.x)[1]))) %>%
  unnest(cols = c(logLik)) %>% arrange(AIC)

# The best model included all factors and interactions in the dispersion formula

mod2 <- update(mod, dispformula =  ~ date * gear * loc)

## Model assessment ####

# Residual checks
plot(DHARMa::simulateResiduals(mod2))
performance::check_residuals(mod2)

# Posterior predictive checks
ppcheck_plot <- performance::check_predictions(mod2, iterations = 1000)

plot(ppcheck_plot) +
  see::theme_lucid(
    base_size = 14,
    axis.text.size = 13,
    axis.title.size = 13,
    legend.text.size = 13
  ) +
  labs(title = NULL,
       subtitle = NULL,
       x = "Fouling ratio") +
  scale_color_manual(values = c("#7bbcd5", "black"))

# Predictive checks (posterior predictive checks, in a Bayesian framework)
# compare predictions from the fitted model to the actual observed data. If the
# model structure is a good fit, the observed data should be consistent with new
# replicated data sets simulated from the fitted model parameters (Gelman &
# Hill, 2007). Predictive checks performed via the `performance` package in R,
# with 1000 iterations (simulated draws). This plot indicates there are no
# systematic discrepancies between observed data and data predicted by the
# model.

# Dispersion
performance::check_overdispersion(mod2)
# equivalent results to DHARMa::testDispersion(mod2)

# Efron's pseudo r-squared
r2_efron(mod2)

## Model results and parameter estimates ####
tidy(mod2)
summary(mod2)

#Table 1
a <- emmeans::joint_tests(mod2) %>%
  select(`model term`, F.ratio, Chisq, p.value) %>%
  dplyr::rename("Term" = "model term",
                "F-ratio" = "F.ratio",
                "P-value" = "p.value") %>%
  mutate(Term = case_when(
      Term == "loc" ~ "location",
      Term == "date:loc" ~ "date:location",
      Term == "gear:loc" ~ "gear:location",
      Term == "date:gear:loc" ~ "date:gear:location",
      .default = Term))

a %>% gt() %>%
  fmt_number(decimals = 3) %>%
  sub_small_vals(threshold = 0.001) %>% cols_label(Chisq = "{{:chi:^2}}") %>%
  opt_horizontal_padding(scale = 3) #%>% as_rtf() %>% write_clip()


# Post-hoc contrasts -----------------------------
# Supplementary tables 1-4

names <- c(
  "Location" = "loc",
  "SE" = "std.error",
  "P-value adj." = "p.value",
  "P-value adj." = "adj.p.value",
  "P-value adj." = "p.value.adj",
  "Odds ratio" = "odds.ratio",
  "Odds ratio" = "ratio"
)

create_supp_gt_avg_comps <- function(mod, variable, by = TRUE) {
  marginaleffects::avg_comparisons(
    mod,
    #vcov = vcov(mod2),
    variables = variable,
    by = by,
    p_adjust = "bonferroni",
    re.form = NA,
    ) %>%
    select(any_of(c("term", "contrast", "date", "gear", "loc", "estimate", "std.error",
        "statistic", "p.value", "conf.low", "conf.high"))) %>%
    dplyr::rename(any_of(names)) %>%
    gt() %>%
    fmt_number(decimals = 4) %>%
    sub_small_vals(threshold = 0.001) %>%
    tab_style(
      locations = cells_column_labels(columns = any_of(
        c("term", "contrast", "date", "gear", "loc", "estimate", "statistic",
          "conf.low", "conf.high"))),
      style = cell_text(transform = "capitalize")) %>%
    tab_style(
      locations = cells_body(columns = "term"),
      style = cell_text(transform = "capitalize"))
}


  # gear, across all locations and dates
  s1 <- create_supp_gt_avg_comps(mod2, variable = list(gear = "pairwise")) 
  
  # location, across all gears and dates
  s2 <- create_supp_gt_avg_comps(mod2, variable = list(loc = "pairwise")) 
  
  # gear contrasts by location, across dates
  s3 <- create_supp_gt_avg_comps(mod2, variable = list("gear" = "pairwise"), by = "loc") 
 
  # location contrasts by gear, across dates
  s4 <- create_supp_gt_avg_comps(mod2,variable=list("loc"="pairwise"), by="gear") 
  
  # gear contrasts for each date and location 
  s5 <- create_supp_gt_avg_comps(mod2, variable = list("gear" = "pairwise"), by = c("date", "loc")) 
  
  supp_gts <- gt_group(s1, s2, s3, s4, s5)
  supp_gts