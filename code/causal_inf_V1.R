### ### ### ### ### ### ### ### ### ### ### ###

#TITLE
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#DATE

# Explanation of script

#Explain data inputs IN detail, and where analyses were done (if any). Include links where possible.
#List prior workflow script (if any)

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

#Helpful tutorials:
### inverse probability weighting - https://www.andrewheiss.com/blog/2020/12/01/ipw-binary-continuous/
### robust & clustered standard errors - https://evalf21.classes.andrewheiss.com/example/standard-errors/

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse", "WeightIt", 
                      "sandwich", "lmtest", "broom", "fixest", 
                      "kableExtra", "modelsummary", "performance",
                      'tictoc', "cobalt")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(tidyverse) #Tidyverse!
library(WeightIt) #for weighting
library(cobalt) #for evaluating weights
library(here) #for relative path management
library(broom) #convert model objects into data frames
library(tictoc) #benchmarking

#For clustered robust standard errors
library(sandwich)
library(lmtest) #for crse
library(modelsummary) #to handle many different versions of the same model
library(kableExtra) #to allow modelsummary to generate tables and graphs
#webshot::install_phantomjs() <- run this line to allow modelsummary & kable to write out summary tables
library(fixest) #for OLS models with lots of fixed effects, also handles crse
library(performance) #to evaluate model performance

## Clean workspace ----
rm(list=ls()) #Ensure empty workspace if running from the beginning
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)

## Set relative directories structure ----

# Set output directory & create if doesn't already exist
outDir <- here::here("figs")
if (!dir.exists(outDir)){
  dir.create(outDir)
}

outDir <- here(outDir, paste(format(Sys.time(), "%Y-%m-%d-%H-%M-%S-%Z"), sep = "_"))
dir.create(outDir)


## Load data ----

rawDats <- read_csv(here::here("data", "final", "linked_disturbance_data_frame_2023-09-12-19-38-48-MDT", "combined_outputs", "linked_disturbance_data_frame.csv"))

### ### ### ### ### ### ### ### ### ### ### ###

# SCRIPTED ANALYSIS ----

#Confirm that dataset has been filtered appropriately
unique(rawDats$adm_code) #usfs only
unique(rawDats$forestMask) #forest-only
unique(rawDats$roadBuffer) #not near roads


#Filter to only areas that have not had ANY fire or bark beetle disturbance
filtDats <- rawDats %>%
  dplyr::select(-roadBuffer) %>% #remove column of NAs now that we have checked to make sure that we don't have roads
  dplyr::filter(is.na(yrsSinceInsect) & is.na(yrsSinceFire)) #'NA' denotes never occurred in time frame of data

#Get histogram of agbd values
qplot(agbd, data=filtDats, geom="histogram", xlim = c(0,400), xlab = "Aboveground Biomass (Mg/ha)", main = "Aboveground Biomass Histogram") 
ggsave(here::here(outDir, "biomassHist.png"))

unique(filtDats$hotDroughtYrsInPrior20)


### ### ### ### ### ### ### ### ### ### ### ###

# Massive function to run model ----
drought.model <- function(dats, droughtCol, dcName, outDir) {
  
  # Set output directory & create if doesn't already exist
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  
  colAsChar <- deparse(substitute(droughtCol)) #get column name as character for use in formulas
  
  # Take a look at the balance of the covariates before matching
  examine_balance <- dats %>%
    mutate({{droughtCol}} := as.factor({{droughtCol}})) %>%
    dplyr::select(shot_numbe, {{droughtCol}}, aetNorm, defNorm, elevation, speiCum20yrPrior) %>%
    pivot_longer(cols = c(aetNorm, defNorm, elevation, speiCum20yrPrior))
  
  examine_balance %>%
    ggplot(aes(x = name, y = value, col = {{droughtCol}})) +
    geom_boxplot() +
    labs(title = "Confounder Balance", x = "Confounding Variable", y = "Confounder Value")
  ggsave(here::here(outDir, paste("examineBalance_", dcName, ".png", sep="")))
  
  examine_balance %>%
    filter(name == "speiCum20yrPrior") %>%
    ggplot(aes(x = name, y = value, col = {{droughtCol}})) +
    geom_boxplot() +
    labs(title = "Confounder Balance", x = "Confounding Variable", y = "Confounder Value")
  ggsave(here::here(outDir, paste("examineBalanceSPEI_", dcName, ".png", sep="")))
  
  filtDats <- dats %>%
    mutate({{droughtCol}} := as.factor({{droughtCol}})) %>%
    mutate(forestCode = as.factor(forestCode))
  
  #Before running anything, check for missing cases (initially 2 observations deleted due to missingness)
  #which(!complete.cases(filtDats %>% select(aetNorm, defNorm, elevation, speiCum20yrPrior, hotDroughtYrsInPrior20, US_L4CODE, forestCode,chili, aspect, slope, mtpi, modisTreeVeg2000)))
  #t <- filtDats[2776, ] #looks like the problem is US_L4CODE
  filtDats <- filtDats %>% filter(!is.na(US_L4CODE))
  

  #Inverse probability weighting on cumulative SPEI, CWD, AET, Elevation
  weightForm <- as.formula(paste(colAsChar, "~ aetNorm + defNorm + elevation + speiCum20yrPrior"))
  
  print("Weighting")
  weights <- WeightIt::weightit(data = filtDats,
                                weightForm,
                                stabalize = TRUE)
  weights
  summary(weights)
  
  #Add weights to dataset
  weightedDats <- filtDats %>%
    dplyr::mutate(ipw = weights$weights)
  
  
  #################QUESTION: HOW TO VISUALIZE COVARIATE BALANCE AFTER INVERSE PROBABILITY WEIGHTING?...
  #scatterplots of the weight assigned to a unit vs its covariate value
  
  ggplot(data = weightedDats) +
    geom_point(aes(x = ipw, y = elevation))
  ggsave(here::here(outDir, paste("weightBalanceElev", dcName, ".png", sep="")))
  
  ggplot(data = weightedDats) +
    geom_point(aes(x = ipw, y = aetNorm))
  ggsave(here::here(outDir, paste("weightBalanceAet", dcName, ".png", sep="")))
  
  ggplot(data = weightedDats) +
    geom_point(aes(x = ipw, y = defNorm))
  ggsave(here::here(outDir, paste("weightBalanceDef", dcName, ".png", sep="")))
  
  ggplot(data = weightedDats) +
    geom_point(aes(x = ipw, y = speiCum20yrPrior))
  ggsave(here::here(outDir, paste("weightBalanceSPEI", dcName, ".png", sep="")))
  
  
  #Model GEDI abgb ~ hotter drought, ecoregion, forest type, topography, VCF2000
  # Fixed effect on GEDI collection year
  # Fixed effect on EPA lvl 4
  form <- as.formula(paste(" agbd ~ ", colAsChar, " + US_L4CODE + forestCode + chili + aspect + slope + mtpi + modisTreeVeg2000 + aetNorm + defNorm + elevation + speiCum20yrPrior"))

  print("modeling")
  model <- lm(data = weightedDats,
              form,
              weights = ipw)
  
  summary(model)
  broom::tidy(model, conf.int = TRUE)
  
  #################QUESTION: STILL INCLUDE CONFOUNDERS THAT WE WEIGHTED ON, OR NO?
  ####People have shown that we still get better estimates
  #################QUESTION: HOW TO IMPLEMENT FIXED EFFECT? HOW IS THIS DIFFERENT FROM JUST INCLUDING IT IN THE MODEL?
  ###it's not! By including a categorical variable you have implemented a fixed effect
  
  ###########################And how do we decide to do this vs running the model on subsets?
  ######both should get you to the same place but people will do both for a robustness check - also depends on the story you're trying to tell
  
  #Can also do unit-level fixed effects
  
  print("naive modeling")
  model_naive <- lm(data = filtDats,
                    form)
  
  broom::tidy(model_naive, conf.int = TRUE)
  
  
  # Clustered robust standard errors ----
  #new tutorial: https://evalf21.classes.andrewheiss.com/example/standard-errors/
  
  #check for heterosketasticity
  fit_dats <- broom::augment(model, data = weightedDats)
  
  # Look at relationship between fitted values and residuals
  ggplot(fit_dats, aes(x = .fitted, y = .resid)) + 
    geom_point(aes(color = forestCode), alpha = 0.3) + #there's a bit of a pattern here when colored by US_L4CODE, forest code, slope, and modisTreeVeg2000 - suggests something spatial going on, but surprised not elevation
    geom_smooth(method = "lm") +
    labs(title = "Residuals & Fitted Values by forest code", y = "Residual", x = "Fitted Value")
  ggsave(here::here(outDir, paste("fittedVsResidualsForestCode", dcName, ".png", sep="")))
  
  ggplot(fit_dats, aes(x = .fitted, y = .resid)) + 
    geom_point(aes(color = US_L4CODE), alpha = 0.3) + #there's a bit of a pattern here when colored by US_L4CODE, forest code, slope, and modisTreeVeg2000 - suggests something spatial going on, but surprised not elevation
    geom_smooth(method = "lm") +
    labs(title = "Residuals & Fitted Values by EPA level 4 code", y = "Residual", x = "Fitted Value")
  ggsave(here::here(outDir, paste("fittedVsResidualsUS_L4CODE", dcName, ".png", sep="")))
  
  # # Look at relationship between fitted values and residuals - tried with smaller sample to see if more obvious clustering
  # ggplot(fit_dats[sample(nrow(fit_dats), size = 10000),], aes(x = .fitted, y = .resid)) + 
  #   geom_point(aes(color = forestCode), alpha = 0.3) + #there's a bit of a pattern here when colored by US_L4CODE, forest code, slope, and modisTreeVeg2000 - suggests something spatial going on, but surprised not elevation
  #   geom_smooth(method = "lm") +
  #   labs(title = "Residuals & Fitted Values by forest code (subset)", y = "Residual", x = "Fitted Value")
  # ggsave(here::here(outDir, paste("fittedVsResidualsForestCode_subset", dcName, ".png", sep="")))
  # 
  # ggplot(fit_dats[sample(nrow(fit_dats), size = 10000),], aes(x = .fitted, y = .resid)) + 
  #   geom_point(aes(color = US_L4CODE), alpha = 0.3) + #there's a bit of a pattern here when colored by US_L4CODE, forest code, slope, and modisTreeVeg2000 - suggests something spatial going on, but surprised not elevation
  #   geom_smooth(method = "lm") +
  #   labs(title = "Residuals & Fitted Values by EPA level 4 code (subset)", y = "Residual", x = "Fitted Value")
  # ggsave(here::here(outDir, paste("fittedVsResidualsUS_L4CODE_subset", dcName, ".png", sep="")))
  # 
  #histogram of residuals
  ggplot(fit_dats, aes(x = .resid)) +
    geom_histogram(binwidth = 100, color = "white", boundary = 500)
  
  
  # #Run model again using fixest package -------- THIS ISN'T WORKING, SEEMS TO HAVE 2 EXTRA WEIGHTS?...
  # fixest_model <- fixest::feols(data = weightedDats,
  #             form,
  #             weights = ipw)
  # 
  
  #use lmtest package to crse
  model_crse_epacode <- lmtest::coeftest(model,
                                         vcov = vcovCL,
                                         type = "HC1",
                                         cluster = ~US_L4CODE)
  broom::tidy(model_crse_epacode, conf.int = TRUE)
  
  model_crse_forestcode <- lmtest::coeftest(model,
                                            vcov = vcovCL,
                                            type = "HC1",
                                            cluster = ~forestCode)
  
  model_crse_hex100000 <- lmtest::coeftest(model,
                                           vcov = vcovCL,
                                           type = "HC1",
                                           cluster = ~hex_id_100000)
  
  model_crse_hex50000 <- lmtest::coeftest(model,
                                          vcov = vcovCL,
                                          type = "HC1",
                                          cluster = ~hex_id_50000)
  
  model_crse_hex10000 <- lmtest::coeftest(model,
                                          vcov = vcovCL,
                                          type = "HC1",
                                          cluster = ~hex_id_10000)
  
  model_crse_hex5000 <- lmtest::coeftest(model,
                                         vcov = vcovCL,
                                         type = "HC1",
                                         cluster = ~hex_id_5000)
  
  model_crse_hex10000_l4 <- lmtest::coeftest(model,
                                         vcov = vcovCL,
                                         type = "HC1",
                                         cluster = ~hex_id_10000 + US_L4CODE)
  
  
  #Get summary statistics from all the models
  modelsummary(
    list('naive lm' = model_naive,
         'basic lm' = model,
         'lm + vcovCL(US_L4CODE)' = model_crse_epacode,
         'lm + vcovCL(forestCode)' = model_crse_forestcode,
         'lm + vcovCL(hex_100000)' = model_crse_hex100000,
         'lm + vcovCL(hex_50000)' = model_crse_hex50000,
         'lm + vcovCL(hex_10000)' = model_crse_hex10000,
         'lm + vcovCL(hex_5000)' = model_crse_hex5000,
         'lm + vcovCL(hex_10000 + L4)' = model_crse_hex10000_l4),
    statistic = c('p.value', 'conf.int'),
    stars = TRUE,
    coef_omit = "^(?!.*hotD)",
    output = here::here(outDir, paste("modelSummary", dcName, ".png", sep="")))
  
  #Visualize confidence intervals from all models
  modelsummary::modelplot(
    list('naive lm' = model_naive,
         'basic lm' = model,
         'lm + vcovCL(US_L4CODE)' = model_crse_epacode,
         'lm + vcovCL(forestCode)' = model_crse_forestcode,
         'lm + vcovCL(hex_id_100000)' = model_crse_hex100000,
         'lm + vcovCL(hex_id_50000)' = model_crse_hex50000,
         'lm + vcovCL(hex_id_10000)' = model_crse_hex10000,
         'lm + vcovCL(hex_id_5000)' = model_crse_hex5000,
         'lm + vcovCL(hex_10000 + L4)' = model_crse_hex10000_l4),
    coef_omit = "^(?!.*hotD)" #?modelplot to see where this syntax came from
  ) +  
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    labs(title = paste("Model results for hotter-drought presence in the last ", dcName, "s", sep = "")) + 
    guides(color = guide_legend(reverse = TRUE)) #flip the order of the legend so that it matches graphical output
  ggsave(here::here(outDir, paste("allModels", dcName, ".png", sep="")), bg="white")
  
  
  ggplot(filtDats, aes(x = {{droughtCol}})) +
    geom_bar() +
    geom_text(aes(label = ..count..), stat = "count", vjust = -0.4) +
    labs(title = "Sample sizes", y = "Sample count")
  ggsave(here::here(outDir, paste("groupSizes", dcName, ".png", sep="")), bg="white")

  
  return(list(model_crse_hex10000_l4, weights))
}

### ### ### ### ### ### ### ### ### ### ### ###


# Run models & get best model results ----
tic("run model once")
outs20yr <- drought.model(filtDats, hotDroughtYrsInPrior20, "20yr", here::here(outDir, '20yr'))
toc() #about 10 minutes for run w/ 25/173 datasets

weights_20yr <- outs20yr[[2]]
summary(weights_20yr)
mod_20yr <- outs20yr[[1]]

outs15yr <- drought.model(filtDats, hotDroughtYrsInPrior15, "15yr", here::here(outDir, '15yr'))
mod_15yr <- outs15yr[[1]]
weights_15yr <- outs15yr[[2]]
summary(weights_15yr)

outs10yr <- drought.model(filtDats, hotDroughtYrsInPrior10, "10yr", here::here(outDir, '10yr'))
mod_10yr <- outs10yr[[1]]
weights_10yr <- outs10yr[[2]]
summary(weights_10yr)

outs5yr <- drought.model(filtDats, hotDroughtYrsInPrior5, "5yr", here::here(outDir, '5yr'))
mod_5yr <- outs5yr[[1]]
weights_5yr <- outs5yr[[2]]
summary(weights_5yr)

noneInLast10 <- filtDats %>% filter(hotDroughtYrsInPrior10 == 0) #no hot drought in last ten years
noneInFirst10 <- filtDats %>% filter(hotDroughtYrsInPrior11 == hotDroughtYrsInPrior20) #no hot drought in first ten years

mod_20_10 <- drought.model(noneInLast10, hotDroughtYrsInPrior20, "10-20yr", here::here(outDir, '20-10yr'))
mod_10_0 <- drought.model(noneInFirst10, hotDroughtYrsInPrior10, "0-10yr", here::here(outDir, '0-10yr'))

hd_only_0_5 <- filtDats %>% filter(hotDroughtYrsInPrior5 == hotDroughtYrsInPrior20) #no hot drought in first 15 years
hd_only_5_10 <- filtDats %>% filter(hotDroughtYrsInPrior11 == hotDroughtYrsInPrior20 & hotDroughtYrsInPrior5 == 0) #no hot drought in last 5 years or the first 10 years
hd_only_10_15 <- filtDats %>% filter(hotDroughtYrsInPrior16 == hotDroughtYrsInPrior20 & hotDroughtYrsInPrior10 == 0) #no hot drought in last 10 years or the first 5 years
hd_only_15_20 <- filtDats %>% filter(hotDroughtYrsInPrior15 == 0) #no hot drought in last 15 years

mod_hd_only_0_5 <- drought.model(hd_only_0_5, hotDroughtYrsInPrior20, "0-5yr", here::here(outDir, '0-5yr'))
mod_hd_only_5_10 <- drought.model(hd_only_5_10, hotDroughtYrsInPrior20, "5-10yr", here::here(outDir, '5-10yr'))
#It appears that there was no hotter drought in the 10-15 year period! Cannot compute - for test dataset
mod_hd_only_10_15 <- drought.model(hd_only_10_15, hotDroughtYrsInPrior20, "10-15yr", here::here(outDir, '10-15yr'))
mod_hd_only_15_20 <- drought.model(hd_only_15_20, hotDroughtYrsInPrior20, "15-20yr", here::here(outDir, '15-20yr'))





#Combine model results from multiple runs



###############NORMAL RUNS

modelsummary::modelplot(
  list('20 year lm + cvovCL(hex_10000 + L4)' = mod_20yr,
       '10 year lm + cvovCL(hex_10000 + L4)' = mod_10yr,
       '5 year lm + cvovCL(hex_10000 + L4)' = mod_5yr),
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior204" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior205" = "5 Hot Drought Instances",
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior103" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior51" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior52" = "2 Hot Drought Instances"),
  coef_omit = "^(?!.*hotD)" #?modelplot to see where this syntax came from
) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Model results for hotter-drought presence") + 
  guides(color = guide_legend(reverse = TRUE)) #flip the order of the legend so that it matches graphical output
ggsave(here::here(outDir, "allModels.png", 7, 7, "in"), bg="white")

modelsummary(
  list('20 year lm + cvovCL(hex_10000 + L4)' = mod_20yr,
       '10 year lm + cvovCL(hex_10000 + L4)' = mod_10yr,
       '5 year lm + cvovCL(hex_10000 + L4)' = mod_5yr),
  statistic = c('p.value', 'conf.int'),
  stars = TRUE,
  coef_omit = "^(?!.*hotD)",
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior204" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior205" = "5 Hot Drought Instances",
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior103" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior51" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior52" = "2 Hot Drought Instances"),
  output = here::here(outDir, "allModelSummary.png"))

#Normal runs, but remove minimal sample set cases
modelsummary::modelplot(
  list('20 year lm + cvovCL(hex_10000 + L4)' = mod_20yr,
       '10 year lm + cvovCL(hex_10000 + L4)' = mod_10yr,
       '5 year lm + cvovCL(hex_10000 + L4)' = mod_5yr),
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior204" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior205" = "5 Hot Drought Instances",
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior103" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior51" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior52" = "2 Hot Drought Instances"),
  coef_omit = "spei|elev|Norm|modis|mtpi|slope|aspect|chili|forestCode|US_L4|Intercept|hotDroughtYrsInPrior52|hotDroughtYrsInPrior103|hotDroughtYrsInPrior205" #?modelplot to see where this syntax came from
) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Model results for hotter-drought presence \n [small samples dropped, most conserverative CRSE]") + 
  guides(color = guide_legend(reverse = TRUE)) #flip the order of the legend so that it matches graphical output
ggsave(here::here(outDir, "allModelsDropSmallSample.png"), bg="white")

modelsummary(
  list('20 year lm + cvovCL(hex_10000 + L4)' = mod_20yr,
       '10 year lm + cvovCL(hex_10000 + L4)' = mod_10yr,
       '5 year lm + cvovCL(hex_10000 + L4)' = mod_5yr),
  statistic = c('p.value', 'conf.int'),
  stars = TRUE,
  coef_omit = "spei|elev|Norm|modis|mtpi|slope|aspect|chili|forestCode|US_L4|Intercept|hotDroughtYrsInPrior52|hotDroughtYrsInPrior103|hotDroughtYrsInPrior205", #?modelplot to see where this syntax came from
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior204" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior205" = "5 Hot Drought Instances",
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior103" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior51" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior52" = "2 Hot Drought Instances"),
  output = here::here(outDir, "allModelSummaryDropSmallSample.png"))


modelsummary::modelplot(
  list('20 year lm + cvovCL(hex_10000 + L4)' = mod_20yr,
       '15 year lm + cvovCL(hex_10000 + L4)' = mod_15yr,
       '10 year lm + cvovCL(hex_10000 + L4)' = mod_10yr,
       '5 year lm + cvovCL(hex_10000 + L4)' = mod_5yr),
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior204" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior151" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior152" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior153" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior154" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior205" = "5 Hot Drought Instances",
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior103" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior51" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior52" = "2 Hot Drought Instances"),
  coef_omit = "spei|elev|Norm|modis|mtpi|slope|aspect|chili|forestCode|US_L4|Intercept|hotDroughtYrsInPrior52|hotDroughtYrsInPrior154|hotDroughtYrsInPrior205" #?modelplot to see where this syntax came from
) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Model results for hotter-drought presence \n [small samples dropped, most conserverative CRSE]") + 
  guides(color = guide_legend(reverse = TRUE)) #flip the order of the legend so that it matches graphical output
ggsave(here::here(outDir, "allModelsDropSmallSample.png"), bg="white")

modelsummary(
  list('20 year lm + cvovCL(hex_10000 + L4)' = mod_20yr,
       '15 year lm + cvovCL(hex_10000 + L4)' = mod_15yr,
       '10 year lm + cvovCL(hex_10000 + L4)' = mod_10yr,
       '5 year lm + cvovCL(hex_10000 + L4)' = mod_5yr),
  statistic = c('p.value', 'conf.int'),
  stars = TRUE,
  coef_omit = "spei|elev|Norm|modis|mtpi|slope|aspect|chili|forestCode|US_L4|Intercept|hotDroughtYrsInPrior52|hotDroughtYrsInPrior154|hotDroughtYrsInPrior205", #?modelplot to see where this syntax came from
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior204" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior205" = "5 Hot Drought Instances",
                  "hotDroughtYrsInPrior151" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior152" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior153" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior154" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior103" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior51" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior52" = "2 Hot Drought Instances"),
  output = here::here(outDir, "allModelSummaryDropSmallSample.png"))

modelsummary::modelplot(
  list('20 year lm + cvovCL(hex_10000 + L4)' = mod_20yr,
       '15 year lm + cvovCL(hex_10000 + L4)' = mod_15yr,
       '10 year lm + cvovCL(hex_10000 + L4)' = mod_10yr,
       '5 year lm + cvovCL(hex_10000 + L4)' = mod_5yr),
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior204" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior151" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior152" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior153" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior154" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior205" = "5 Hot Drought Instances",
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior103" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior51" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior52" = "2 Hot Drought Instances"),
  coef_omit = "spei|elev|Norm|modis|mtpi|slope|aspect|chili|forestCode|US_L4|Intercept|hotDroughtYrsInPrior52|hotDroughtYrsInPrior103|hotDroughtYrsInPrior154|hotDroughtYrsInPrior153|hotDroughtYrsInPrior205|hotDroughtYrsInPrior204" #?modelplot to see where this syntax came from
) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Model results for hotter-drought presence \n [small samples dropped, most conserverative CRSE]") + 
  guides(color = guide_legend(reverse = TRUE)) #flip the order of the legend so that it matches graphical output
ggsave(here::here(outDir, "allModelsDropBadCommonSupport.png"), bg="white")

modelsummary(
  list('20 year lm + cvovCL(hex_10000 + L4)' = mod_20yr,
       '15 year lm + cvovCL(hex_10000 + L4)' = mod_15yr,
       '10 year lm + cvovCL(hex_10000 + L4)' = mod_10yr,
       '5 year lm + cvovCL(hex_10000 + L4)' = mod_5yr),
  statistic = c('p.value', 'conf.int'),
  stars = TRUE,
  coef_omit = "spei|elev|Norm|modis|mtpi|slope|aspect|chili|forestCode|US_L4|Intercept|hotDroughtYrsInPrior52|hotDroughtYrsInPrior103|hotDroughtYrsInPrior154|hotDroughtYrsInPrior153|hotDroughtYrsInPrior205|hotDroughtYrsInPrior204", #?modelplot to see where this syntax came from
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior204" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior205" = "5 Hot Drought Instances",
                  "hotDroughtYrsInPrior151" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior152" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior153" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior154" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior103" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior51" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior52" = "2 Hot Drought Instances"),
  output = here::here(outDir, "allModelSummaryDropBadCommonSupport.png"))

############## SEQUENCING 10/20

modelsummary::modelplot(
  list('20-10 year lm + cvovCL(hex_10000 + L4)' = mod_20_10,
       '10-0 year lm + cvovCL(hex_10000 + L4)' = mod_10_0),
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior204" = "4 Hot Drought Instances",
                  "hotDroughtYrsInPrior103" = "3 Hot Drought Instances",
                  
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances"),
  coef_omit = "^(?!.*hotD)" #?modelplot to see where this syntax came from
) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Model results for hotter-drought presence") + 
  guides(color = guide_legend(reverse = TRUE)) #flip the order of the legend so that it matches graphical output
ggsave(here::here(outDir, "10_20_Models.png"), bg="white")

modelsummary(
  list('20-10 year lm + cvovCL(hex_10000 + L4)' = mod_20_10,
       '10-0 year lm + cvovCL(hex_10000 + L4)' = mod_10_0),
  statistic = c('p.value', 'conf.int'),
  stars = TRUE,
  coef_omit = "^(?!.*hotD)",
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances",
                  "hotDroughtYrsInPrior101" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior102" = "2 Hot Drought Instances"),
  output = here::here(outDir, "10_20_ModelSummary.png"))


############## SEQUENCING 5/10/15/20

modelsummary::modelplot(
  list('0-5 year lm + cvovCL(hex_10000 + L4)' = mod_hd_only_0_5,
       '5-10 year lm + cvovCL(hex_10000 + L4)' = mod_hd_only_5_10,
       '10-15 year lm + cvovCL(hex_10000 + L4)' = mod_hd_only_10_15,
       '15-20 year lm + cvovCL(hex_10000 + L4)' = mod_hd_only_15_20),
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances"),
  coef_omit = "^(?!.*hotD)" #?modelplot to see where this syntax came from
) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Model results for hotter-drought presence") + 
  guides(color = guide_legend(reverse = TRUE)) #flip the order of the legend so that it matches graphical output
ggsave(here::here(outDir, "5_10_15_20_Models.png"), bg="white")


modelsummary(
  list('0-5 year lm + cvovCL(hex_10000 + L4)' = mod_hd_only_0_5,
       '5-10 year lm + cvovCL(hex_10000 + L4)' = mod_hd_only_5_10,
       '15-20 year lm + cvovCL(hex_10000 + L4)' = mod_hd_only_15_20),
  statistic = c('p.value', 'conf.int'),
  stars = TRUE,
  coef_omit = "^(?!.*hotD)",
  coef_rename = c("hotDroughtYrsInPrior201" = "1 Hot Drought Instance",
                  "hotDroughtYrsInPrior202" = "2 Hot Drought Instances",
                  "hotDroughtYrsInPrior203" = "3 Hot Drought Instances"),
  output = here::here(outDir, "5_10_15_20_ModelSummary.png"))








#plot(model)



######
######NOTE: these CSREs may be too conservative given that our sample includes a high fraction of possible clusters


# performance::model_performance(model)
# 
# #to get this to show performance correctly we would need to include these adjustments in the model & use feols()
# performance::compare_performance(model, model_crse_epacode, 
#                                  model_crse_hex100000, model_crse_hex50000, 
#                                  model_crse_hex10000, model_crse_hex5000)


