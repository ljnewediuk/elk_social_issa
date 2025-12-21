
## Plot the RSS
## By Levi Newediuk December 2025

## 1- Prep workspace ====

# Load libraries
libs <- c('data.table', 'tidyverse')
lapply(libs, require, character.only = TRUE)

# Load data
dat <- readRDS("output/cleaned_model_data.rds")

# Load RSS
rss_id_NNdist <- readRDS('rss/rss_id_NNdist.rds')
rss_pop_NNdist <- readRDS('rss/rss_pop_NNdist.rds')
rss_id_wang <- readRDS('rss/rss_id_wang.rds')
rss_pop_wang <- readRDS('rss/rss_pop_wang.rds')

## 2- Unscale and uncentre variables ====

# Variable means
mu_NNdist <- mean(dat$lStartDist, na.rm = T)
mu_wang <- mean(dat$Wang_Start_NN, na.rm = T)

# Unscale and convert to km if distance
rss_id_NNdist$NN_Distance_unsc <- (exp(rss_id_NNdist$NN_Distance + mu_NNdist) - 0.125)/1000
rss_pop_NNdist$NN_Distance_unsc <- (exp(rss_pop_NNdist$NN_Distance + mu_NNdist) - 0.125)/1000
rss_pop_wang$Relatedness_unsc <- rss_pop_wang$Relatedness + mu_wang
rss_id_wang$Relatedness_unsc <- rss_id_wang$Relatedness + mu_wang


## 3- Plot the RSS ====

# Nearest neighbour distance
ggplot() +
  geom_line(data = rss_id_NNdist, 
            aes(x = NN_Distance_unsc, y = logRSS, group = ANIMAL_ID),
            linewidth = 0.25, colour = 'chartreuse') +
  geom_line(data = rss_pop_NNdist, 
            aes(x = NN_Distance_unsc, y = logRSS),
            linewidth = 1, colour = 'chartreuse') +
  theme(panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.25, 0.25, 1, 1), 'cm'),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 15, colour = 'black'),
        legend.title = element_text(size = 18, colour = 'black'),
        axis.line = element_line(linewidth = 0.5),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5)) +
  labs(x = 'Distance to nearest neighbour (km)', y = 'log relative selection strength')

# Relatedness
ggplot() +
  geom_line(data = rss_id_wang, 
            aes(x = Relatedness_unsc, y = logRSS, group = ANIMAL_ID),
            linewidth = 0.25, colour = 'darksalmon') +
  geom_line(data = rss_pop_wang, 
            aes(x = Relatedness_unsc, y = logRSS),
            linewidth = 1, colour = 'darksalmon') +
  theme(panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.25, 0.25, 1, 1), 'cm'),
        axis.text = element_text(size = 18, colour = 'black'),
        legend.text = element_text(size = 15, colour = 'black'),
        legend.title = element_text(size = 18, colour = 'black'),
        axis.line = element_line(linewidth = 0.5),
        axis.title.x = element_text(size = 18, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 18, colour = 'black', vjust = 5)) +
  labs(x = 'Relatedness', y = 'log relative selection strength')
