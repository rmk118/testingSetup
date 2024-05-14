#---------------------------------------------------#
#  Beta regression for biofouling ratios
#  By Ruby Krasnow, based on the template
# "A versatile workflow for linear modelling in R" created by
#  Matteo Santon, Fraenzi Korner-Nievergelt, Nico Michiels, Nils Anthes
#  Last modified: May 14, 2024
#---------------------------------------------------#

# Activate the support 'functions file' for this routine:
source("Linear modelling workflow_support functions.R") 
  # NOTE: The 'functions file' must be located in your current working directory.

#-###############################-#
# 1. Import data  ----
#-###############################-#

ratios_raw <- read_csv("./data/bremen_biofouling_ratios.csv", show_col_types = FALSE)

ratios <- ratios_raw %>% mutate(fouling_wt = dirty_wt_boat-clean_wt_boat,
                                whole_wet_wt = clean_wt_boat-weigh_boat,
                                prop_fouling=fouling_wt/whole_wet_wt,
                                fouling_ratio=prop_fouling*100,
                                gear = as.factor(gear),
                                loc = as.factor(location),
                                date = as.factor(date), .keep="unused")
  
df <- ratios %>%   filter(fouling_ratio>0 & fouling_ratio<60) #remove negative values 
#and one highly influential outlier likely resulting from measurement/recording error


# Carefully check data structure, column names and vector classes
str(df)

#-###############################-#
# 2. Definition of variables ----
#-###############################-#

var_resp <- "prop_fouling"             # Response variable of interest
var_fac <- c("gear", "loc", "date")    # Factors with at least two levels

# No numeric or random predictors for this response variable
var_num<-NA
var_rand<-NA

#-###############################-#
# 3. Raw data exploration ----
#-###############################-#


## 3.1 Extreme values ----
#=========================#
# We graphically inspect variables for extreme values by generating
# barplots for all factor predictors and random terms.

barplot_fac(data = df, variables = c(var_fac))

# Observations are roughly balanced between grouping levels
  

## 3.2 Predictor collinearity ----
#================================#

# Graphical inspection
#-------------------------------------------------------------#
coll_fac(data = df, predictors = var_fac)  # Pairwise mosaic plots for all factor predictors.
# All combinations of predictor levels have roughly similar sample sizes.


# Variance Inflation Factors (VIFs)
#------------------------------------------------#
corvif(data = df, variables = c(var_fac))  # VIFs are calculated for all fixed predictors 
# Derived from Zuur, Ieno & Elphick (2010).
# All GVIF values are very close to 1, as desired


## 3.3 Predictor-response relationships ----
#==========================================#
# The function generates plots of the response variable against each of the potential predictor variables

relat_single(data = df,
             response = var_resp,                # Name of response variable
             predictors = c(var_fac))            # Name of fixed predictors


## 3.4 Response distribution ----
#===============================#
# Visualize the distribution of your response variable.
distr_response(data = df, response = var_resp) 
  # Observed distribution of the response confirms beta distribution is appropriate


#-#######################-#
# 4. Model formulation ----
#-#######################-#.


#---------------------------------------#  
## 4.1 Initial model formulation ----
#---------------------------------------#  

mod <- glmmTMB(prop_fouling ~ date*gear*loc,       
               data = df,                         
               family = beta_family())

#---------------------------------------#  
## 4.2 Checking other formulations ----
#---------------------------------------#  

# beta_formula <- list(
#   full = prop_fouling ~ date*gear*loc,
#   add_only=prop_fouling~gear+loc+date,
#   no_date=prop_fouling~gear*loc,
#   no_loc=prop_fouling~gear*date,
#   no_gear = prop_fouling~loc*date,
#   no_loc2=prop_fouling~gear*date+gear:loc+gear:loc:date)
# 
# beta_models <- tibble(beta_formula, 
#             models = map(beta_formula, ~glmmTMB(data=df, formula=.x , family = beta_family()))) %>%
#   add_column(id=names(beta_formula), .before=1)
# 
# beta_models <- beta_models %>% 
#   mutate(tidy_model = map(models, tidy),
#          AIC=map(models, AIC),
#          resids = map(models, residuals)) %>% unnest(cols=c(AIC)) %>%
#   mutate(resids = map(resids, as.numeric()),
#          normal_p = map(resids, ~shapiro.test(.x)$p.value)) %>%
#   unnest(cols=c(normal_p)) %>%
#   mutate(logLik=map(models, ~(logLik(.x)[1]))) %>% 
#   unnest(cols=c(logLik)) %>% arrange(AIC)

# The full model (including all interaction terms) had the lowest AIC and highest logLik

#---------------------------------------#  
## 4.3 Adjusting dispersion ----
#---------------------------------------#  

disp_formula <- list(
  full = ~date*gear*loc,
  add_only=~gear+loc+date,
  no_date=~gear*loc,
  no_loc=~gear*date,
  no_gear = ~loc*date)

disp_models <- tibble(disp_formula,
                      models = map(disp_formula,
                                   ~glmmTMB(data=df, 
                                            formula= prop_fouling ~ date*gear*loc, 
                                            dispformula=.x, 
                                            family = beta_family()))) %>% 
  add_column(id=names(disp_formula), .before=1)

disp_models <- disp_models %>% 
  mutate(tidy_model = map(models, tidy),
         AIC=map(models, AIC),
         resids = map(models, residuals)) %>% unnest(cols=c(AIC)) %>%
  mutate(resids = map(resids, as.numeric()),
         normal_p = map(resids, ~shapiro.test(.x)$p.value)) %>%
  unnest(cols=c(normal_p)) %>%
  mutate(logLik=map(models, ~(logLik(.x)[1]))) %>% 
  unnest(cols=c(logLik)) %>% arrange(AIC)

mod2 <- update(mod, dispformula=~gear*loc*date)


#-######################-#
# 5. Model assessment ----  
#-######################-#

## 5.1 Residual checks ----
#========================#

plot(simulateResiduals(mod2))

residual_plots(data = df,
               modelTMB = mod2,
               response = var_resp)

# >> QQ-plot for residuals: Points should closely follow the diagonal reference line.
# >> Residuals against fitted: Points should homogeneously scatter without any pattern.


## 5.2 Posterior predictive checks ----
#=====================================# 
# Assess model performance with posterior predictive checks.
# These compare the observed data with the distribution of many
# replicate raw dataframes simulated from the model

### 5.2.1 Dispersion ----
#-----------------------#
# Compare the variance of the observed data with the variance distribution in model-simulated dataframes:
dispersion_simulation(data = df, 
                      modelTMB = mod2,
                      response = var_resp,
                      #predictor = var_fac[1], # (optional, but recommended) - A SINGLE factor predictor to split this plot. Use a different index number as needed.
                      n.sim = 1000)            # Number of simulations

# Observed variance is roughly central within the simulated distributions, as desired


### 5.2.2 Data distribution ----
#-------------------------------# 
# Compares the observed raw data distribution with that observed in model-simulated dataframes
ppcheck_fun(data = df,
            modelTMB = mod2,
            response = var_resp,
           # predictor = var_fac[1],   # (optional, but recommended) - A SINGLE factor predictor to split this plot.
            n.sim = 500)                # Number of simulations


# The observed data distribution is roughly central within the pattern 
#    of simulated data from the model, as desired


#-###########################################-#
# 6. Model results and parameter estimates ----
#-###########################################-#

# Overall coefficient estimates and model performance 
#=============================================================#
# Extract coefficient estimates and 95% compatibility intervals:
comp_int(modelTMB = mod2,
           ci_range = 0.95,     # Compatibility interval range.
           effects = "fixed")


summary(mod2)
joint_tests(mod2)
r2_efron(mod2)

