
## Modified iSSA (smaller models for hypothesis testing) ##
## By Levi Newediuk, modified December 2025 from Zoe Melvin

## 1- Prep workspace ====

# Load libraries
libs <- c('data.table', 'amt', 'tidyverse', 'glmmTMB')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/ZOE/5-rdm-locs-sri-NN-N10.RDS")

## 2- Prep data for models ====

# Convert used vs. available to 1 and 0
DT$Use[DT$case_ == "TRUE"] <- 1
DT$Use[DT$case_ == "FALSE"] <- 0

# Create unique step id by animal
DT[,'elk_step_id_'] <- paste(DT$IDYr, DT$step_id_, sep = '_')

# Remove rows with Not Available habitat value
# These either fell outside the aci raster or in open water
DT<- filter (DT, Cover_end != "NotAvailable")
DT<- filter (DT, Cover_start != "NotAvailable")

# Convert open and closed habitat to 1 = open, 0 = closed
DT$Open_start <- ifelse(DT$Cover_start == 'Open', 1, 0)
DT$Open_end <- ifelse(DT$Cover_end == 'Open', 1, 0)

# Center, scale, and log variables for model convergence/intepretation

# Step lengths and turn angles
DT[, log_sl_ := log(sl_ + 1e-6)]
DT[, cos_ta_ := cos(ta_)]

# SRI of nearest neighbour at start of step
# Log
DT[, lsri_startNN := log(sri_startNN + 0.125)]
# Centre
DT[, lsri_start_c := (lsri_startNN - mean(lsri_startNN, na.rm=TRUE))]

# Distance to nearest neighbour at start of step
# Log
DT[, lStartDist := log(StartDist + 0.125)]
# Centre
DT[, lStartDist_c := (lStartDist - mean(lStartDist, na.rm=TRUE))]

# Relatedness of nearest neighbour at start of step (centre only)
DT[, Wang_Start_c := (Wang_Start_NN - mean(Wang_Start_NN, na.rm=TRUE))]

## 3- Define model covariates ====

# Base covariates
base_covs <-  c(
  'I(log(sl_ + 1))',
  'cos(ta_)',
  '(1|elk_step_id_)'
)

# Nearest neighbour hypothesis (do elk select more for open habitat when
# starting their step closer to any other elk?)
prox_covs_d <- c(
  'Open_end',
  'lStartDist_c',
  'Open_end:lStartDist_c',
  '(1 | ANIMAL_ID)',
  '(0 + lStartDist_c:Open_end | ANIMAL_ID)'
)

# SRI hypothesis (do elk select more for open habitat when starting their step
# closer to an individual with whom they share a higher SRI?)
sri_covs_d <- c(
  'Open_end',
  'lsri_start_c',
  'lsri_start_c:Open_end',
  '(1 | ANIMAL_ID)',
  '(0 + lsri_start_c:Open_end | ANIMAL_ID)'
)

# Kinship hypothesis (do elk select more for open habitat when starting their
# step closer to an individual to whom they are more closely related?)
wang_covs_d <- c(
  'Open_end',
  'Wang_Start_c',
  'Wang_Start_c:Open_end',
  '(1 | ANIMAL_ID)',
  '(0 + Wang_Start_c:Open_end | ANIMAL_ID)'
)

## 4- Fit models ====

# Fit temp model to figure out variance-covariance structure
tmp <- glmmTMB(
  reformulate(c(base_covs, prox_covs_d), response = "case_"),
  family = poisson(),
  data = DT
)

# Isolate thetas
par_vec <- tmp$fit$par
n_thetas <- length(par_vec[grep("^theta", names(par_vec))])

# Calculate number of thetas in model
nvar_parm = (n_thetas) - 1

# Function to fit models
fit_mod <- function(covs, nvar_parm, dat) {
  # Set up model without fitting
  model_form <- suppressWarnings(
    glmmTMB(reformulate(covs, response = 'case_'),
            family = poisson(), 
            map = list(theta = factor(c(NA, 1:nvar_parm))),
            data = dat, doFit = F))
  # Set variance of random intercept to large number (10,000)
  model_form$parameters$theta[1] <- log(1e4)
  # Fit model using large fixed variance
  model_fit <- glmmTMB:::fitTMB(model_form)
  # Return the glmmTMB object
  return(model_fit)
}

# Fit the models
model_sri_d <- fit_mod(c(base_covs, sri_covs_d), nvar_parm = nvar_parm, DT)
model_prox_d <- fit_mod(c(base_covs, prox_covs_d), nvar_parm = nvar_parm, DT)
model_wang_d <- fit_mod(c(base_covs, wang_covs_d), nvar_parm = nvar_parm, DT)

# Save the models
saveRDS(model_sri_d, 'models/issa_sri.rds')
saveRDS(model_prox_d, 'models/issa_prox.rds')
saveRDS(model_wang_d, 'models/issa_wang.rds')

