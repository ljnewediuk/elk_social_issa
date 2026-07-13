
# Testing out some new methods for calculating relatedness because our small 
# sample size might lead to biased relatedness estimates. See Wang (2017) Heredity.

# Load packages
library(related)
library(tidyverse)
library(Demerelate)

# Load microsat data (related)
microsats <- readgenotypedata("data/Vita-elk-1/GenotypeData.txt")

# Load microsat data (Demerelate)
microsats <- read.table("data/Vita-elk-1/GenotypeData.txt", header = F) %>%
  mutate(Population = 'Vita') %>%
  relocate(Population, .after = V1)

# Estimate relatedness (related)
related_ests <- coancestry(genotype.data = microsats$gdata, wang = 1, lynchrd = 1)$relatedness

# Estimate relatedness (Demerelate) using Wang for small sample size
results <- Demerelate::Demerelate(inputdata = microsats, object = TRUE, value = "wang.fin", file.output = F)

# Save Demerelate results (pair-wise relatedness estimates come from results$Empirical_Relatedness)
saveRDS(results, 'output/Wang_relatedness.rds')

# Convert relatedness into data frame
wang <- enframe(results$Empirical_Relatedness$Vita) %>%
  rename(pair = name)

# Make reverse data frame for alternate matches
wang_reverse <- wang %>%
  mutate(pair = paste0("VE_", substr(pair, 10, 11), "_VE_", substr(pair, 4, 5)))

# Bind together
wang_full <- wang %>% bind_rows(wang_reverse)

# Load data
DT <- readRDS("output/ZOE/5-rdm-locs-sri-NN-N10.RDS") %>%
  # Add columns to join with pairs
  mutate(pair_end = paste0("VE_", substr(IDYr, 6, 7), "_VE_", substr(EndNN_ID, 6, 7)),
         pair_start = paste0("VE_", substr(IDYr, 6, 7), "_VE_", substr(StartNN_ID, 6, 7)))

# Join end pairs
end_pairs <- wang_full %>%
  rename(pair_end = pair,
         Wang_End_NN_corrected = value) %>%
  right_join(DT)

# Join start pairs
start_pairs <- wang_full %>%
  rename(pair_start = pair,
         Wang_Start_NN_corrected = value) %>% 
  right_join(end_pairs) %>%
  # Remove pairs columns
  select(! c(pair_start, pair_end))

# Save updated data for model
saveRDS(start_pairs, "output/corrected_iSSA_data.rds")

