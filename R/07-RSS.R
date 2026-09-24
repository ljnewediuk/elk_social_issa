
### 07 - RSS for open across social gradient ====

## 1- Prep workspace ====

# Load libraries
libs <- c('tidyverse')
lapply(libs, require, character.only = TRUE)

# Load models
issa_prox_d <- readRDS('models/issa_prox_d.rds')
issa_sri_d <- readRDS('models/issa_sri_d.rds')
issa_wang_d <- readRDS('models/issa_wang_d.rds')

# Load data
DT <- readRDS('output/cleaned_model_data.rds')

# Function to estimate the individual and population RSS from models
get_rss <- function(mod, dat, coeff_name) {
  
  # Random effects from model
  re <- ranef(mod)$cond$ANIMAL_ID %>%
    rownames_to_column("ANIMAL_ID")
  
  # Rename intercept and slope cols
  colnames(re)[c(2,3)] <- c("b_0", "b_i")
  
  # Get fixed effects
  beta_open <- fixef(mod)$cond["Open_end"]
  beta_intxn_open <- fixef(mod)$cond[paste0("Open_end:", coeff_name)]
  
  # Estimate RSS (individuals)
  rss <- expand.grid(
    ANIMAL_ID = unique(re$ANIMAL_ID),
    coeff = seq(min(dat[[coeff_name]]), 
                max(dat[[coeff_name]]), 
                length.out = 100)) %>%
    left_join(re) %>%
    mutate(
      coeff_exp = exp(coeff),
      log_RSS = b_0 + beta_open +
        (beta_intxn_open + b_i) * coeff,
      RSS = exp(log_RSS))
  
  # Estimate RSS (population)
  
  # Confidence intervals
  
  # Fixed effects from model
  b <- fixef(mod)$cond
  
  # Variance-covariance matrix of fixed effects
  V <- vcov(mod)$cond
  
  # Social values over which we want RSS
  social <- seq(min(dat[[coeff_name]]), max(dat[[coeff_name]]), length.out = 100)
  
  # Log RSS
  log_rss <- b["Open_end"] +
    b[paste0("Open_end:", coeff_name)] * social
  
  # SE of RSS
  var_log_rss <- V["Open_end", "Open_end"] +
    social^2 * V[paste0("Open_end:", coeff_name), 
                 paste0("Open_end:", coeff_name)] +
    2 * social * V["Open_end", paste0("Open_end:", coeff_name)]
  
  se_log_rss <- sqrt(var_log_rss)
  
  # Calculate 95% CI
  lowerci <- log_rss - 1.96 * se_log_rss
  upperci <- log_rss + 1.96 * se_log_rss
  
  # Population RSS
  rss_pop <- data.frame(
    coeff = seq(min(dat[[coeff_name]]), 
                max(dat[[coeff_name]]), 
                length.out = 100)) %>%
    mutate(
      coeff_exp = exp(coeff),
      log_RSS = beta_open +
        beta_intxn_open * coeff,
      RSS = exp(log_RSS),
      lower = lowerci,
      upper = upperci
    )
  
  # Return a list with individual and population-level RSS
  return(
    list(pop = rss_pop, id = rss)
  )
}

# Estimate the RSS
sri_rss <- get_rss(issa_sri_d, DT, "lsri_startNN")
wang_rss <- get_rss(issa_wang_d, DT, "Wang_Start_NN")
prox_rss <- get_rss(issa_prox_d, DT, "lStartDist")

# Save the RSS data
saveRDS(sri_rss, 'models/rss_sri.rds')
saveRDS(wang_rss, 'models/rss_wang.rds')
saveRDS(prox_rss, 'models/rss_prox.rds')

# Calculate the relative effects for reporting in results

# Relative selection for open at 75th vs. 25th quantile SRI
q_sri <- quantile(DT$lsri_startNN, probs = c(0.25, 0.75))

beta_open <- fixef(issa_sri_d)$cond["Open_end"]
beta_intxn  <- fixef(issa_sri_d)$cond["Open_end:lsri_startNN"]

exp(beta_open + beta_intxn * q_sri)[2]/exp(beta_open + beta_intxn * q_sri)[1]

# Relative selection for open when 0 vs. 500 m from NN
beta_open <- fixef(issa_prox_d)$cond["Open_end"]
beta_intxn  <- fixef(issa_prox_d)$cond["Open_end:lStartDist"]

exp(beta_open + beta_intxn * log(5))/exp(beta_open + beta_intxn * log(500))

# Relative selection for open at 75th vs. 25th quantile genetic relatedness
q_sri <- quantile(DT$Wang_Start_NN, probs = c(0.25, 0.75))

beta_open <- fixef(issa_wang_d)$cond["Open_end"]
beta_intxn  <- fixef(issa_wang_d)$cond["Open_end:Wang_Start_NN"]

exp(beta_open + beta_intxn * q_sri)[2]/exp(beta_open + beta_intxn * q_sri)[1]

