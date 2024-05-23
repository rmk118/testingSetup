#---------------------------------------------------#
#  A versatile workflow for linear modelling in R
#  Matteo Santon, Fraenzi Korner-Nievergelt, Nico Michiels, Nils Anthes
#  Version date: 03 November 2023
#  -> TEMPLATE
#---------------------------------------------------#

#-##############################################-#
# 1. Source the support file for this routine ----
#-##############################################-#
#rm(list = ls()) # OPTIONAL: Execute to remove previous objects from the global environment.

# Activate the support 'functions file' for this routine:
source("Linear modelling workflow_support functions.R") 
  # NOTE-1: The 'functions file' must be located in your current working directory.
  # NOTE-2: At first use, all missing packages required for this routine are automatically installed.
  # NOTE-3: "Step plot settings" in the functions file specifies the ggplot2 theme for the entire routine. 
  #         Modify there as needed, leave it as default otherwise.


#-###########################-#
# 2. Import the dataframe  ----
#-###########################-#
# Headers of this template match the respective section headers of the accompanying article. 
# Consult the article for more detailed explanations of each section. 

# Import data frame (example only. Any other import function of R can be used).
g_data <- read.csv("./data/bremen_growth.csv") %>% 
  mutate(cup_ratio=width/height,
         fan_ratio=length/height,
         sum_dims = length+width+height,
         chi=((sum_dims*0.5-height)^2/(sum_dims*0.5))+
           ((sum_dims*(1/3)-length)^2/(sum_dims/3))+
           ((sum_dims*(1/6)-width)^2/(sum_dims/6)),
         date = mdy(date)) %>% 
  mutate(across(c("gear", "location", "cage", "bag", "trt", "date", "tag_num"), as.factor))

g_data <- g_data %>% mutate(cage= #make cage numbers unique
  case_when(
    cage==1 & location=="out"~ as.factor(1),
    cage==2 & location=="out"~ as.factor(2),
    cage==3 & location=="out"~ as.factor(3),
    cage==1 & location=="in"~  as.factor(4),
    cage==2 & location=="in"~  as.factor(5),
    cage==3 & location=="in"~ as.factor(6),
    .default=as.factor("None")))
# Carefully check data structure, column names and vector classes. Change them as needed.
g <- g_data %>% 
  add_sample %>% 
  left_join(all_env, by=c("location", "sample")) %>% 
  mutate(loc=as.factor(location)) %>% ungroup()

str(g)


#-##############################-#
# 3. Definition of variables  ----
#-##############################-#
# In step 3.1 through 3.4, the names of all variables (= data frame columns) that are potentially relevant 
# for your analysis are stored as STRINGS for further use:
#================================#
# * 3.1 ONE RESPONSE variable ----
#================================#
var_resp <- "response"                     # Replace with the name of the response variable.


#=============================================================================#
# * 3.2 Fixed predictors: quantitative and categorical predictor variables ----
#=============================================================================#
# FACTOR PREDICTOR variable(s)
var_fac <- c("gear", "loc", "date")  # Assure all these are factors with at least two levels.

# NUMERIC or INTEGER PREDICTOR variable(s) 
var_num <- c("chl", "temp", "turbidity", "TPM", "POM", "PIM")  # Assure all these are numeric or integer.

#==============================================================#
# * 3.3 Random predictors: dependency structure of the data ----
#==============================================================#
# RANDOM term(s)
var_rand <- c("tag_num")      # Add cage?



#-###############################-#
# 4. Raw data exploration ----
#-###############################-#
# This step helps to identify patterns that may require consideration for model formulation.
# NOTE: Further checks can be found in the residual plot analysis (section 6.1), in particular for missing predictors or interactions.

#=========================#
# * 4.1 Extreme values ----
#=========================#
# We graphically inspect variables for extreme values.

#---------------------------------------------#
# ** 4.1.1 Extremes in NUMERICAL variables ---- 
#---------------------------------------------#
dotplot_num(data = g, variables = c(var_num, "cup_ratio")) # Dotplots for all numerical predictors plus the response variable.
dotplot_num(data = g, variables = c(var_num, "fan_ratio"))
dotplot_num(data = g, variables = c(var_num, "chi"))
dotplot_num(data = g, variables = c(var_num, "height"))
# What should I look for?
# >> Is any observation CLEARLY separated from the core distribution? Such an observation may represent 'implausible extremes'

# Resolving implausible extreme values: 
# >> Check where such values originate from. If you can trace them to objective (!) typing errors, correct these values.
# >> If extremes are in the response variable: choose an adequate distribution family.
# >> If extremes are in predictor variables: 
#    1. Use model assessment to check if these predictor values cause concern.
#    2. If so, consider data transformation to mitigate the issue.
# >> If extremes cannot be modeled appropriately, consider reporting effect estimates and their SE with and without these extremes.


#------------------------------------------#  
# ** 4.1.2 Extremes in FACTOR variables ----      
#------------------------------------------#
barplot_fac(data = g, variables = c(var_fac, var_rand)) # Barplots for all factor predictors and random terms.

# What should I look for?
# >> Are observations roughly balanced between grouping levels (conforming to a "balanced design")?

# Resolving extreme unbalance: 
# >> If some factor levels arise from typing errors: Correct them.
# >> If there is extreme imbalance in sample size among factor levels, 
#    accept that effect estimates for those with poor replication will associate with large uncertainty.
#    Alternatively, consider pooling levels with very few replicates, but only if this remains biologically meaningful. 

  
#================================#
# * 4.2 Predictor collinearity ----
#================================#
# We graphically and numerically inspect variables for predictor collinearity.

#-------------------------------------------------------------#
# ** 4.2.1: Graphical inspection for predictor collinearity ----
#-------------------------------------------------------------#
# We graphically inspect predictor collinearity for all pairwise combinations of numeric and/or factor predictors.

# *** 4.2.1.1: NUMERIC predictors: Scatterplots ----
# Skip this step if you have < 2 numeric predictors
coll_num(data = g, predictors = var_num) # Pairwise scatterplots for all numeric predictors.
# What should I look for?
# >> Data should distribute rather homogeneously.


# *** 4.2.1.2: FACTOR against NUMERIC predictors: Swarm plots ----
# Skip this step if your predictors are either all numeric, OR all factor.
coll_num_fac(data = g, predictors = c(var_num, var_fac))  # Swarm boxplots for all numeric against all factor predictors.
# What should I look for?
# >> Numeric values on the y axis should distribute roughly homogeneously across levels on the x-axis.
 

# *** 4.2.1.3: FACTOR predictors: Mosaic plots ----   
# Skip this step if you have < 2 factor predictors.
coll_fac(data = g, predictors = var_fac)  # Pairwise mosaic plots for all factor predictors.
# What should I look for?
# >> All combinations of predictor levels should have roughly similar sample sizes.


#------------------------------------------------#
# ** 4.2.2: Variance Inflation Factors (VIFs) ----
#------------------------------------------------#
corvif(data = g, variables = c(var_num, var_fac))  # VIF are calculated for all fixed predictors potentially included in the model.
# Derived from Zuur, Ieno & Elphick (2010).

# What should I look for?
# >> Check column "GVIF": Entirely independent predictors yield GVIF = 1, while larger GVIF values indicate 
#   increasing predictor collinearity (see article section 4.2.2 for details)


#==========================================#
#* 4.3 Predictor-response relationships ----
#==========================================#
# The function generates plots of the response variable against each of the potential predictor variables.
relat_single(data = g,
             response = "cup_ratio",                       
             predictors = c(var_num, var_fac))

relat_single(data = g,response = "fan_ratio", predictors = c(var_num, var_fac))
relat_single(data = g,response = "chi", predictors = c(var_num, var_fac))

#===============================#
# * 4.4 Response distribution ----
#===============================#
# Visualise the distribution of your response variable.
distr_response(data = g, response = "cup_ratio") 
distr_response(data = g, response = "fan_ratio") 
distr_response(data = g, response = "chi") 
distr_response(data = g, response = "height") 
  # What should I look for?
  # >> The observed distribution of the response confirms - at least roughly - your initial expectation.
  #    If not, adjust the distribution family for your initial model accordingly. 

#-#######################-#
# 5. Model formulation ----
#-#######################-#
# Based on the conclusions from data exploration, formulate your preliminary model.


#===============================#
# * Model implementation ----
#===============================#


# Fan ratio ---------------------------------------------------------------

# Other families tested included tweedie, lognormal, and gaussian(link="log"), also tested various dispersion formulas

fan_mod<-glm(fan_ratio~ date*gear+date*loc, data=g, family=Gamma)

glance(fan_mod)
tidy(fan_mod)
Anova(fan_mod)
fan_mod %>% joint_tests()


check_residuals(fan_mod) #equivalent to testUniformity(fan_mod, plot=FALSE)
check_overdispersion(fan_mod)
check_model(fan_mod, check=c("homogeneity", "pp_check", "overdispersion", "qq"))
check_outliers(fan_mod)
check_predictions(fan_mod, iterations = 200)

r2_efron(fan_mod)


# Cup ratio ---------------------------------------------------------------

# Other families tested included beta_family, tweedie, and gaussian(link="log"), also tested various dispersion formulas

cup_mod<-glmmTMB(cup_ratio ~ date*gear*loc, data = g, dispformula = ~date, family="lognormal")

summary(cup_mod)
glance(cup_mod)
tidy(cup_mod)
Anova(cup_mod)
cup_mod %>% joint_tests()

check_residuals(cup_mod) #borderline non-normal but qq plot looks good, test more likely significant with large sample size 
check_overdispersion(cup_mod)
check_model(cup_mod, check=c("qq", "pp_check", "homogeneity"))
check_predictions(cup_mod, iterations = 200)

r2_efron(cup_mod)


# Shell shape -------------------------------------------------------------

chi_mod<-glmmTMB(chi ~ date*gear*location, data =g, family=tweedie)

summary(chi_mod)
glance(chi_mod)
tidy(chi_mod)
Anova(chi_mod)
chi_mod %>% joint_tests()

check_residuals(chi_mod)
check_overdispersion(chi_mod)
plot(simulateResiduals(chi_mod))
check_predictions(chi_mod, iterations = 200)

r2_efron(chi_mod)

# Height ---------------------------------------------------------------

height_mod<-lmer(height ~ temp+date*gear+(1|tag_num), data = g)

summary(height_mod)
glance(height_mod)
tidy(height_mod)
Anova(height_mod)
height_mod %>% joint_tests(lmerTest.limit = 3425, pbkrtest.limit = 3425)

check_residuals(height_mod)
check_overdispersion(height_mod)
check_outliers(height_mod)
check_predictions(height_mod, iterations = 200)

r2(height_mod)
r2_efron(height_mod)

#-######################-#
# 6. Model assessment ----  
#-######################-#

# Before inspecting model results, careful assessment is required.

#========================#
#* 6.1 Residual checks ----
#========================#
# This step helps with checking for 'residual patterns'.

#-----------------------------------------#
# ** 6.1.1: Distribution of residuals -----
#-----------------------------------------#
# This function plots the overall distribution of model residuals.
residual_plots(data = df,
               modelTMB = glmm4,
               response = "cup_ratio") # Name of response variable.

residual_plots(data = growth_rates,
                               modelTMB = growth4,
                              response = "growth_rate")
check_model(growth4)

# What should I look for? 
# >> QQ-plot for residuals: Points should closely follow the diagonal reference line.
# >> Residuals against fitted: Points should homogeneously scatter without any pattern.
# >> QQ-plot for random intercepts, ONLY produced for (G)LMMs: Points should closely follow the diagonal reference line.

# Resolving violations:
# >> Seek a distribution family and/or a link function that better capture the observed residual distribution.
# >> The model may lack an important covariate (-> 6.1.2), or an informative interaction term (-> 6.1.3).
# >> The model may lack relevant non-linear (polynomial) terms (-> 6.1.2).
# >> The model could be over- or underdispersed (-> 6.2.1).


#---------------------------------------------------#
# ** 6.1.2: Residuals against possible PREDICTORS ----
#---------------------------------------------------#
# This function plots residuals against all possible predictors specified in 1.
residual_plots_predictors(data = df,
                          modelTMB = glmm4,
                          predictors = c(var_num, var_fac)) # Name of all fixed predictors in the dataframe.





#=====================================#
# * 6.2 Posterior predictive checks ----
#=====================================# 
# Assess model performance with posterior predictive checks.

#-----------------------#
# ** 6.2.1 Dispersion ----
#-----------------------#
# Compare the variance of the observed data with the variance distribution in model-simulated dataframes:
dispersion_simulation(data = df, 
                      modelTMB = glmm4,
                      response = "cup_ratio",
                      n.sim = 500)            # Number of simulations. Set to >2000 for final model assessment.

# >> Observed variance should be roughly central within the simulated distributions.

check_overdispersion(glmm5)


#-------------------------------#
# ** 6.2.3 Data distribution ----
#-------------------------------# 
# The function compares the observed raw data distribution with that observed in model-simulated dataframes.
ppcheck_fun(data = df,
            modelTMB = glmm4,
            response = "cup_ratio",
            n.sim = 200)                # Number of simulations, set to >2000 for final model assessment.




#-###########################################-#
# 7. Model results and parameter estimates ----
#-###########################################-#
# Perform this step only after the iteration of 3. and 4. results in a final model.

#=============================================================#
#* 7.1 Overall coefficient estimates and model performance ---- 
#=============================================================#
# Extract coefficient estimates and 95% compatibility intervals:
comp_int(modelTMB = glmm4,
           ci_range = 0.95,       # Compatibility interval range.
           effects = "fixed",       # Returns parameters for fixed effects ("fixed")
           component = "all")     # Returns parameters for the conditional model ("cond"), zero-inflation part of the model ("zi"), or the default both ("all").


# Extract marginal and conditional R-squared value.
#r2(model = mod) # Name of model
r2_efron(glmm4)




#==========================================================#
# * 7.3 Estimation of differences between factor levels ---- 
#==========================================================#
# Extract pairwise comparisons among levels of specified factor predictors (means and 95% compatibility intervals).
pairwise_comparisons(data = df,
                     modelTMB = glmm4,
                     predictors = var_fac[1], # Name of one (or more!) factor predictor(s).
                     component = "cond",       # Returns differences for the conditional model ("cond"), or the zero-inflation part of the model ("zi").
                     dispScale = "response",  # "response" returns absolute differences for link identity models, and ratio or odds.ratios for log or logit link models 
                                              # "link" returns estimates on the link scale
                     contrasts = "all")       # "all" returns all pairwise comparisons, "within" only the comparisons among the levels of the first factor specified within each level of the second one

pairwise_comparisons(data = df,
                     modelTMB = glmm4,
                     predictors = var_fac[2], # Name of one (or more!) factor predictor(s).
                     component = "cond",       # Returns differences for the conditional model ("cond"), or the zero-inflation part of the model ("zi").
                     dispScale = "response",  # "response" returns absolute differences for link identity models, and ratio or odds.ratios for log or logit link models 
                     # "link" returns estimates on the link scale
                     contrasts = "all")       # "all" returns all pairwise comparisons, "within" only the comparisons among the levels of the first factor specified within each level of the second one

emmeans(glmm5, specs=c("gear", "location"), type="response") %>% plot()
emmeans(glmm5, specs=c("gear"), type="response") %>% plot()
emmeans(glmm5, specs=c("location"), type="response") %>% plot()



disp_formulas_end <- list(
  a = ~ gear*loc+cage_id,
  b = num ~ gear+gear:cage_id,
  c = num~gear+cage_id,
  d = num~gear*cage_id,
  e= ~gear,
  g= ~loc,
  h= ~gear+loc)