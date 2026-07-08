
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
