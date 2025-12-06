
## load libraries
libs <- c('data.table', 'dplyr', 'amt', 'lubridate', 'tidyr', 'ggplot2', 'glmmTMB', 'patchwork', 
          'broom.mixed', 'raster', 'ggthemes', 'performance', 'AICcmodavg')
lapply(libs, require, character.only = TRUE)


# Load data
DT <- readRDS("output/ZOE/5-rdm-locs-sri-NN-N10.RDS")

DT[, .N, by = c("IDYr")]

DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

## create unique step id by animal
DT[,'elk_step_id_'] <- paste(DT$IDYr, DT$step_id_, sep = '_')

## Remove rows with Not Available habitat value, these either fell outside the aci raster or in open water
DT<- filter (DT, Cover_end != "NotAvailable")
DT<- filter (DT, Cover_start != "NotAvailable")

##Code discrete variables as factors
DT$Cover_end<- as.factor(DT$Cover_end)
DT$Cover_start <- as.factor(DT$Cover_start)
DT$Calving <- as.factor(DT$Calving)
DT$IDYr <- as.factor(DT$IDYr)
DT$Open_start <- ifelse(DT$Cover_start == 'Open', 1, 0)
DT$Open_end <- ifelse(DT$Cover_end == 'Open', 1, 0)

# Log and transform covariates
DT[, log_sl_ := log(sl_ + 1e-6)]
DT[, cos_ta_ := cos(ta_)]

DT[, ldist_forest_end := log(dist_to_forest_end + 0.125)]
DT[, ldist_forest_start := log(dist_to_forest_start + 0.125)]
DT[, lsri_startNN := log(sri_startNN + 0.125)]
DT[, lsri_endNN := log(sri_EndNN + 0.125)]
DT[, lStartDist := log(StartDist + 0.125)]
DT[, lEndDist := log(EndDist + 0.125)]

# Basic covariates
base_covs <-  c(
  'I(log(sl_ + 1))',
  'cos(ta_)',
  '(1|elk_step_id_)'
)

# Relatedness covariates
# Test if elk starting closer to a relative end up in open habitat
wang_covs_d <- c(
  'Open_end',
  'Wang_Start_NN',
  'Wang_Start_NN:Open_end',
  '(1 | ANIMAL_ID)',
  '(0 + Wang_Start_NN:Open_end | ANIMAL_ID)'
)

# SRI covariates
# Test if elk starting closer to an elk with higher SRI end up in open habitat
sri_covs_d <- c(
  'Open_end',
  'lsri_startNN',
  'lsri_startNN:Open_end',
  '(1 | ANIMAL_ID)',
  '(0 + lsri_startNN:Open_end | ANIMAL_ID)'
)

# Proximity covariates
# Test if elk starting closer to another elk end up in open habitat
prox_covs_d <- c(
  'Open_end',
  'lStartDist',
  'Open_end:lStartDist',
  '(1 | ANIMAL_ID)',
  '(0 + lStartDist:Open_end | ANIMAL_ID)'
)

# Fit temp model to figure out variance-covariance structure
tmp <- glmmTMB(
  reformulate(c(base_covs, wang_covs_d), response = "case_"),
  family = poisson(),
  data = DT
)

# Isolate thetas
par_vec <- tmp$fit$par
n_thetas <- length(par_vec[grep("^theta", names(par_vec))])

# Calculate nvar_parm
nvar_parm = (n_thetas) - 1

# Function to fit models
fit_mod <- function(covs, nvar_parm, dat) {
  # Set up model without fitting
  model_form <- suppressWarnings(
    glmmTMB(reformulate(covs, response = 'case_'),
            family = poisson(), 
            map = list(theta = factor(c(NA, 1:nvar_parm))),
            data = dat, doFit = F))
  # Set variance of random intercept to 10^6
  model_form$parameters$theta[1] <- log(1e4)
  # Fit model using large fixed variance
  model_fit <- glmmTMB:::fitTMB(model_form)
  # Return the glmmTMB object
  return(model_fit)
}

# Fit models
model_sri_d <- fit_mod(c(base_covs, sri_covs_d), nvar_parm = nvar_parm, DT)
model_prox_d <- fit_mod(c(base_covs, prox_covs_d), nvar_parm = nvar_parm, DT)
model_wang_d <- fit_mod(c(base_covs, wang_covs_d), nvar_parm = nvar_parm, DT)

# Save the models for RSS
saveRDS(model_sri_d, 'models/issa_sri.rds')
saveRDS(model_prox_d, 'models/issa_prox.rds')
saveRDS(model_prox_d, 'models/issa_wang.rds')

