library(spatialRF)
library(pdp)
library(tidyverse)
library(sf)
library(randomForestExplainer)
library("spdep")
unzip("data/linked_disturbance_data_frame.zip", exdir = "data")

# reading in data
d <- read_csv("data/linked_disturbance_data_frame.csv") %>%
  replace_na(list("yrsSinceFire" = 99,
                  "yrsSinceInsect" = 99,
                  "yrsSinceHotDrought" = 99)) %>%
  # getting rid of columns with NAs - might change years since columns to 99s instead of NA
  dplyr::select(-peakNDVI, -US_L4CODE, -US_L4NAME, -spei22YrPrior, -starts_with("collectionYr"),
                -hotDroughtYrsInPrior22, #-yrsSinceFire, -yrsSinceInsect, 
                -fireYrsInPrior22, -insectYrsInPrior22,
                -yrsSinceCombo, -comboYrsInPrior22, -hotDroughtYrsInPrior1,
                -comboYrsInPrior1, -forestMask, -gediYear, -algorithm_, -adm_code,
                -starts_with("hex_id"), -forestCode) %>%
  mutate(agbd_l = log(agbd + 1))


# sort(apply(d, 2, var) == 0)
d %>% glimpse()
d %>% summary()

# defining predictor variables
preds <- names(d)[c(20:length(d)-2)]

# creating an xy object 
xy <- d[,c("utm_z13n_easting","utm_z13n_northing")]

# creating a distance matrix 
dmat <- dist(xy, upper=TRUE, diag=TRUE)
dim(dmat)

# reducing multicollinearity
preds_reduced <- spatialRF::auto_cor(x = d[, preds], cor.threshold = 0.6) %>% 
  spatialRF::auto_vif(vif.threshold = 2.5)


# doing a non-spatial model, and checking how much time it takes
# adding the distance matrix make it take longer
t0 <- Sys.time()
nsrf <- spatialRF::rf(data = d, 
                      dependent.variable.name = "agbd_l",
                      predictor.variable.names = preds_reduced$selected.variables,
                      distance.matrix = NULL)
t1 <- Sys.time()
t1-t0


# doing a spatial model, and checking how much time it takes
t0 <- Sys.time()
srf <- spatialRF::rf_spatial(data = d, 
                     dependent.variable.name = "agbd_l",
                     predictor.variable.names = preds_reduced$selected.variables,
                     distance.matrix = dmat)
t1 <- Sys.time()
t1-t0

spatialRF::plot_residuals_diagnostics(
  nsrf,
  verbose = FALSE
)

spatialRF::plot_importance(
  nsrf,
  verbose = FALSE
)

plot_response_curves(nsrf, ncol=3)
print_performance(nsrf)
