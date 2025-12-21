
## Modified RSS ##
## By Levi Newediuk, modified December 2025 from Zoe Melvin

## 1- Prep workspace ====

# Load libraries
libs <- c('data.table', 'amt', 'tidyverse', 'glmmTMB')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS("output/cleaned_model_data.rds")

# Load models
issa_wang <- readRDS("models/issa_wang.rds")
issa_sri <- readRDS("models/issa_sri.rds")
issa_prox <- readRDS("models/issa_prox.rds")

## 2- Define functions to calculate RSS ====

# Function for population
p.pop <- function(dat, mod, hab) {
  
  # New data (sequences for all predictors)
  nd <- tibble(
    sl_ = mean(dat$sl_, na.rm = T),
    ta_ = mean(dat$ta_, na.rm = T),
    Open_end = hab,
    lsri_start_c = seq(min(dat$lsri_start_c), max(dat$lsri_start_c), length.out = 100),
    Wang_Start_c = seq(min(dat$Wang_Start_c), max(dat$Wang_Start_c), length.out = 100),
    lStartDist = seq(min(dat$lStartDist), max(dat$lStartDist), length.out = 100),
    elk_step_id_ = NA,
    ANIMAL_ID = NA
  )
  
  # Predict selection
  return(
    predict(
      mod, newdata = nd, type = "link", re.form = NA, se.fit = T
    )
  )
  
}

# Function for individuals
p.id <- function(dat, mod, hab, id) {
  
  nd <- tibble(
    sl_ = mean(dat$sl_, na.rm = T),
    ta_ = mean(dat$ta_, na.rm = T),
    Open_end = hab,
    lsri_start_c = seq(min(dat$lsri_start_c), max(dat$lsri_start_c), length.out = 100),
    Wang_Start_c = seq(min(dat$Wang_Start_c), max(dat$Wang_Start_c), length.out = 100),
    lEndDist_c = seq(min(dat$lEndDist_c), max(dat$lEndDist_c), length.out = 100),
    elk_step_id_ = NA,
    ANIMAL_ID = id
  )
  
  return(
    predict(mod, newdata = nd, type = "link", re.form = NULL
    )
  )
  
}

# Function to compile RSS estimates into tables
RSS.tables <- function(m) {
  
  # Get model
  issa <- get(m)
  
  # Get name of the social/distance variable from model
  var_name <- colnames(issa[["frame"]])[6]
  
  # Population RSS
  predopen <- p.pop(dat = DT, mod = issa, hab = 1)
  predclosed <- p.pop(dat = DT, mod = issa, hab = 0)
  
  # Subtract the open vs closed (RSS difference)
  logRSS = exp(predopen[[1]] - predclosed[[1]])
  
  # Individual RSS
  predclosedid <- lapply(
    unique(DT$ANIMAL_ID), function(X) p.id(dat = DT, mod = issa, hab = 0, id = X)
  )
  predopenid <- lapply(
    unique(DT$ANIMAL_ID), function(X) p.id(dat = DT, mod = issa, hab = 1, id = X)
  )
  
  # Data frame for individual RSS
  logRSSid <- data.frame(
    RSSdiff = unlist(Map(`-`, predopenid, predclosedid)),
    ANIMAL_ID = rep(unique(DT$ANIMAL_ID), each = 100),
    Soc_Var = rep(seq(min(DT[[var_name]]), max(DT[[var_name]]), 
                      length.out = 100), times = 18)
  ) %>%
    mutate(logRSS = exp(RSSdiff))
  
  # Data frame for population RSS
  logRSSpop <- data.frame(
    Soc_Var = seq(min(DT[[var_name]]), 
                  max(DT[[var_name]]), 
                  length.out = 100), 
    logRSS)
  
  # Rename the variables
  if(colnames(issa[["frame"]])[6] == 'lsri_startNN') {
    logRSSpop <- dplyr::rename(logRSSpop, SRI = Soc_Var)
    logRSSid <- dplyr::rename(logRSSid, SRI = Soc_Var)
  }
  if(colnames(issa[["frame"]])[6] == 'Wang_Start_c') {
    logRSSpop <- dplyr::rename(logRSSpop, Relatedness = Soc_Var)
    logRSSid <- dplyr::rename(logRSSid, Relatedness = Soc_Var)
  }
  if(colnames(issa[["frame"]])[6] == 'lStartDist_c') {
    logRSSpop <- dplyr::rename(logRSSpop, NN_Distance = Soc_Var)
    logRSSid <- dplyr::rename(logRSSid, NN_Distance = Soc_Var)
  }
  
  # Return a list with individual and population-level RSS
  return(
    list(pop = logRSSpop, id = logRSSid)
  )
  
}

## 3- Estimate RSS and compile in tables ====
RSS_sri <- RSS.tables(m = 'issa_sri')
RSS_wang <- RSS.tables(m = 'issa_wang')
RSS_prox <- RSS.tables(m = 'issa_prox')

## 4- Save RSS ====
saveRDS(RSS_sri['pop'], 'rss/rss_pop_sri.rds')
saveRDS(RSS_sri['id'], 'rss/rss_id_sri.rds')
saveRDS(RSS_wang['pop'], 'rss/rss_pop_wang.rds')
saveRDS(RSS_wang['id'], 'rss/rss_id_wang.rds')
saveRDS(RSS_prox['pop'], 'rss/rss_pop_NNdist.rds')
saveRDS(RSS_prox['id'], 'rss/rss_id_NNdist.rds')

