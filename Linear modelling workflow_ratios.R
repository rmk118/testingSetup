#---------------------------------------------------#
#  A versatile workflow for linear modelling in R
#  Matteo Santon, Fraenzi Korner-Nievergelt, Nico Michiels, Nils Anthes
#  Version date: 03 November 2023
#  -> TEMPLATE
#---------------------------------------------------#

#-##############################################-#
# 1. Source the support file for this routine ----
#-##############################################-#
rm(list = ls()) # OPTIONAL: Execute to remove previous objects from the global environment.

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
    .default=NA))
# Carefully check data structure, column names and vector classes. Change them as needed.
df <- g_data %>% 
  add_sample %>% 
  left_join(all_env, by=c("location", "sample")) %>% 
  select(-loc) %>% 
  dplyr::rename(TPM=TPM_mg_ml, POM=POM_mg_ml, PIM=PIM_mg_ml) %>% mutate(location=as.factor(location))

str(df)


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
var_fac <- c("gear", "location", "date")  # Assure all these are factors with at least two levels.

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
dotplot_num(data = df, variables = c(var_num, "cup_ratio")) # Dotplots for all numerical predictors plus the response variable.
dotplot_num(data = df, variables = c(var_num, "fan_ratio")) # Dotplots for all numerical predictors plus the response variable.

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
barplot_fac(data = df, variables = c(var_fac, var_rand)) # Barplots for all factor predictors and random terms.

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
coll_num(data = df, predictors = var_num) # Pairwise scatterplots for all numeric predictors.
# What should I look for?
# >> Data should distribute rather homogeneously.


# *** 4.2.1.2: FACTOR against NUMERIC predictors: Swarm plots ----
# Skip this step if your predictors are either all numeric, OR all factor.
coll_num_fac(data = df, predictors = c(var_num, var_fac))  # Swarm boxplots for all numeric against all factor predictors.
# What should I look for?
# >> Numeric values on the y axis should distribute roughly homogeneously across levels on the x-axis.
 

# *** 4.2.1.3: FACTOR predictors: Mosaic plots ----   
# Skip this step if you have < 2 factor predictors.
coll_fac(data = df, predictors = var_fac)  # Pairwise mosaic plots for all factor predictors.
# What should I look for?
# >> All combinations of predictor levels should have roughly similar sample sizes.


#------------------------------------------------#
# ** 4.2.2: Variance Inflation Factors (VIFs) ----
#------------------------------------------------#
corvif(data = df, variables = c(var_num, var_fac))  # VIF are calculated for all fixed predictors potentially included in the model.
# Derived from Zuur, Ieno & Elphick (2010).

# What should I look for?
# >> Check column "GVIF": Entirely independent predictors yield GVIF = 1, while larger GVIF values indicate 
#   increasing predictor collinearity (see article section 4.2.2 for details)


#==========================================#
#* 4.3 Predictor-response relationships ----
#==========================================#
# The function generates plots of the response variable against each of the potential predictor variables.
relat_single(data = df,
             response = "cup_ratio",                       
             predictors = c(var_num, var_fac))

relat_single(data = df,
             response = "fan_ratio",                       
             predictors = c(var_num, var_fac))

#===============================#
# * 4.4 Response distribution ----
#===============================#
# Visualise the distribution of your response variable.
distr_response(data = df, response = "cup_ratio") 
distr_response(data = df, response = "fan_ratio") 
  # What should I look for?
  # >> The observed distribution of the response confirms - at least roughly - your initial expectation.
  #    If not, adjust the distribution family for your initial model accordingly. 

#-#######################-#
# 5. Model formulation ----
#-#######################-#
# Based on the conclusions from data exploration, formulate your preliminary model.

#===============================================#
# * 5.1 Standardisation of numeric predictors ----
#===============================================#
# This step is optional, but standardisation is recommended.
# The code adds z-transformed numeric covariates to the dataframe. We recommend to use these for model formulation.
df <- cbind(df, z_transform(data = df,         # z-transformation ...
                        predictors = var_num)) # ... for all numeric predictor variables.

    # Users who have already standardised their predictors have two options:
    # 1. Skip this step. and accept that final results plots will display standardised predictor values.
    # 2. Start with predictors on the raw scale, and use our function for scaling to allow correct final plotting.


#===============================#
# * 5.2 Model implementation ----
#===============================#

#---------------------------------------#  
# ** 5.2.1 Initial model formulation ----
#---------------------------------------#  
# Implement the initial model.
# See article section 5.2 for guidance to model formulation.
mod <- glmmTMB(response ~ pred.1 + pred.2,         # Template: Replace with your complete model formula.
               data = df,                          # Name of dataframe.
               family = poisson(link = "log"))     # Template: Replace with your distribution family and link.

# This formulation may need refinement (section 5.2.2 in manuscript) after model assessment (6.).
glm1<-glmmTMB(cup_ratio ~ chl+temp+PIM+turbidity+date+gear*location, data = df, family="beta_family")
glm1<-glmmTMB(cup_ratio ~ chl+PIM+turbidity+date+gear*location, data = df, family="beta_family")
glm1<-glmmTMB(cup_ratio ~ chl+PIM+date+gear*location+(1|tag_num), data = df, family="beta_family")
glmm3<-glmmTMB(cup_ratio ~ chl+PIM+date+gear*location, data = df,dispformula = ~gear*location, family="beta_family")
glmm2<-glmmTMB(cup_ratio ~ chl+PIM+date+gear*location, data = df,dispformula = ~gear, family="beta_family")
glmm1<-glmmTMB(cup_ratio ~ chl+PIM+date+gear*location, data = df, family="beta_family")
glmm4<-glmmTMB(cup_ratio ~ date+gear*location, data = df,dispformula = ~gear*location, family="beta_family")
tidy(glmm1)
glmm1 %>% joint_tests()
check_model(glmm1)
Anova(glmm1)
anova(glm1, glmm1)
anova(glmm2, glmm1, glmm3, glmm4)

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
               modelTMB = glmm3,
               response = "cup_ratio") # Name of response variable.

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
                          modelTMB = glmm1,
                          predictors = c(var_num, var_fac)) # Name of all fixed predictors in the dataframe.
# NOTE: For this check, we recommend that var_num and var_fac ALSO contain available variable(s) 
# that are currently NOT part of the model. 

# What should I look for? 
# >> Points should homogeneously scatter without obvious pattern.

# Resolving violations:
# >> Seek a distribution family and/or link function that better capture the observed residual distribution.
# >> The model may lack an important covariate (shown here: covariates that show residual patterns).
#    or an informative interaction term (-> 6.1.3).
# >> The model may lack relevant non-linear (polynomial) terms (shown here: curvilinear patterns in residuals across a given covariate).
# >> The model could be over- or underdispersed (-> 6.2.1).
# >> The model may suffer from zero-inflation (-> 6.2.2).
# >> Add a dispersion formula to the model to explicitly integrate heterogeneous variance across predictor values / levels.


#------------------------------------------------------------------------------#  
# ** 6.1.3: Residuals against possible TWO-WAY INTERACTIONS AMONG PREDICTORS ----
#------------------------------------------------------------------------------#  
# Graphical displays of all 2-way predictor-response relationships.
residual_plots_interactions(data = df, 
                            modelTMB = mod, 
                            predictors = c(var_num, var_fac))      # All fixed predictors in the dataframe.
# NOTE-1: In numeric vs. numeric display panels, smoothing lines are added as a visual aid to detect non-linear relationships.
# NOTE-2: Up to 9 plots are shown in the plot window. 
#         If > 9 plots are present, they are saved in your working directory as "Two-ways_interactions.pdf".

# What should I look for?   
# >> Points should homogeneously scatter without obvious pattern. 

# Resolving violations:
# >> Consider adding the relevant interaction term to the model if meaningful. 


#------------------------------------------------------------------------#
# ** 6.1.4: Residuals against PREDICTORS split by RANDOM FACTOR LEVELS ----
#------------------------------------------------------------------------#
# These plots can reveal if any random intercept term additionally needs random slopes over a specific fixed predictor.
  # NOTE: ONLY useful when MULTIPLE observations per combination of random and factor levels are present.
  #       Please only select those var_fac, var_num and/or var_rand that meet this criterion.
residual_plots_random_slopes(data = df,                       # Name of dataframe.
                             modelTMB = glmm1,                  # Name of model.
                             fixed_eff = c(var_num, var_fac), # Name of fixed factors meeting criterion described above.
                             random_eff = var_rand)           # Name of random factors meeting criterion described above.

# What should I look for?
# >> Do plot panels indicate diverging slopes (for numeric predictors),
#    or inconsistent effect directions (for factor predictors)?

# What if this is the case? 
# >> Add a random slope for that specific predictor to the model (see article section 5.2.1)


#=====================================#
# * 6.2 Posterior predictive checks ----
#=====================================# 
# Assess model performance with posterior predictive checks.
# These compare the observed data with the distribution of many, ideally >2000, 
# replicate raw dataframes simulated from the model. For initial checks a lower number of n.sim will do, though.

#-----------------------#
# ** 6.2.1 Dispersion ----
#-----------------------#
# Compare the variance of the observed data with the variance distribution in model-simulated dataframes:
dispersion_simulation(data = df, 
                      modelTMB = glmm1,
                      response = "cup_ratio",
                      # predictor = var_fac[1], # (optional, but recommended) - A SINGLE factor predictor to split this plot. Use a different index number as needed.
                      n.sim = 500)            # Number of simulations. Set to >2000 for final model assessment.

# What should I look for?
# >> Observed variance should be roughly central within the simulated distributions.
# >> Check the function's feedback messages in the R console window.

# Resolving violations:
# >> Check if dispersion issues are connected to zero inflation (-> 6.2.2).
# >> Look for a distribution family and link that better capture the observed data dispersion.



#-------------------------------#
# ** 6.2.3 Data distribution ----
#-------------------------------# 
# The function compares the observed raw data distribution with that observed in model-simulated dataframes.
ppcheck_fun(data = df,
            modelTMB = glmm1,
            response = "cup_ratio",
            # predictor = var_fac[1],   # (optional, but recommended) - A SINGLE factor predictor to split this plot.
            n.sim = 500)                # Number of simulations, set to >2000 for final model assessment.

# What should I look for?
# >> Ideally, the observed data distribution should be roughly central within the pattern 
#    of simulated data from the model.

# Resolving violations:
# >> Carefully re-evaluate the preceding steps of model assessment, and reformulate your model.



#-###########################################-#
# 7. Model results and parameter estimates ----
#-###########################################-#
# Perform this step only after the iteration of 3. and 4. results in a final model.

#=============================================================#
#* 7.1 Overall coefficient estimates and model performance ---- 
#=============================================================#
# Extract coefficient estimates and 95% compatibility intervals:
comp_int(modelTMB = glmm1,
           ci_range = 0.95,       # Compatibility interval range.
           effects = "all",       # Returns parameters for fixed effects ("fixed"), random effects ("random"), or both ("all").
           component = "all")     # Returns parameters for the conditional model ("cond"), zero-inflation part of the model ("zi"), or the default both ("all").


# Extract marginal and conditional R-squared value.
r2(model = mod) # Name of model
r2_efron(glmm1)
# NOTE: this works for (G)LMMs, only, i.e., for models that include a random component.


#=========================================#
#* 7.2 Estimation of regression slopes ---- 
#=========================================#
# Extract slope estimates, their 95% compatibility intervals, and an estimate of (proportional) change from simulated data.
# This function is useful to extract slope estimates within each level of a factor predictor that is also part of the model.
slope_estimates(data = df,
                modelTMB = mod,
                num_predictor = var_num[1],  # Name of one numeric predictor for which to estimate slopes. Change index as needed.
                fac_predictors = var_fac[1]) # # Name of one (or two!) factor predictor(s). Slope estimates will be given per level of this factors. Change index as needed. 


#==========================================================#
# * 7.3 Estimation of differences between factor levels ---- 
#==========================================================#
# Extract pairwise comparisons among levels of specified factor predictors (means and 95% compatibility intervals).
pairwise_comparisons(data = df,
                     modelTMB = glmm1,
                     predictors = var_fac[1], # Name of one (or more!) factor predictor(s).
                     component = "cond",       # Returns differences for the conditional model ("cond"), or the zero-inflation part of the model ("zi").
                     dispScale = "response",  # "response" returns absolute differences for link identity models, and ratio or odds.ratios for log or logit link models 
                                              # "link" returns estimates on the link scale
                     contrasts = "all")       # "all" returns all pairwise comparisons, "within" only the comparisons among the levels of the first factor specified within each level of the second one

pairwise_comparisons(data = df,
                     modelTMB = glmm1,
                     predictors = var_fac[2], # Name of one (or more!) factor predictor(s).
                     component = "cond",       # Returns differences for the conditional model ("cond"), or the zero-inflation part of the model ("zi").
                     dispScale = "response",  # "response" returns absolute differences for link identity models, and ratio or odds.ratios for log or logit link models 
                     # "link" returns estimates on the link scale
                     contrasts = "all")       # "all" returns all pairwise comparisons, "within" only the comparisons among the levels of the first factor specified within each level of the second one

#-############################################-#
# 8. Graphical display of model predictions ----
#-############################################-#
# This section produces a combined display of raw data with model-derived predictions and their 95% compatibility intervals.

#==============================================#
# * 8.1 Specify variables for the final plot ---- 
#==============================================#
# Select ONE or TWO model predictor(s) for the final plot:
plot_predictors <- c("pred.1", "pred.2")      # Template: Replace with the names of MAX 2 predictor variable(s).

# Select the random level of interest to display appropriate independent replicates of your raw data:
plot_random <- NA                             # Assign NA if missing.
# plot_random <- c("rand.1", "rand.2")        # Template: Replace with the names of your random term(s).


#==================================#
# * 8.2 Derive model predictions ---- 
#==================================#
# This function generates a grid for all possible predictor combinations. It derives model predictions 
# averaged for the predictors to plot, and projects z-transformed predictors back to their raw scale.
mod_predictions <- post_predict(data = df,
                                modelTMB = mod,
                                plot_predictors = plot_predictors, # Name of predictors specified in 8.1.
                                # offset = NA,                     # (optional) - Name of response offset variable, if present in the model. Default to NA.
                                component = "all")                 # (optional) - Computes predictions for the conditional model ("cond"), zero-inflation part of the model ("zi"), or the default both ("all").
                           
      
                        
#============================#
# * 8.3 Summarise raw data ---- 
#============================#
# This function computes a summary of raw data at the TRUE replicate level. These values will be displayed in the final plot:
data_summary <- display_raw(data = df,
                           modelTMB = mod,
                           plot_predictors = plot_predictors, # Name of predictors specified in 8.1.
                           # plot_random = plot_random,       # (optional) - Name of the random level specified in 8.1.
                           response = var_resp,               # Name of response variable.
                           # offset = NA,                     # (optional) - Name of response offset variable, if present in the model. Default to NA.
                           component = "all")                 # Computes predictions for the conditional model ("cond"), zero-inflation part of the model ("zi"), or the default both ("all").
