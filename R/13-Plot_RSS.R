
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
rss_id_sri <- readRDS('rss/rss_id_sri.rds')
rss_pop_sri <- readRDS('rss/rss_pop_sri.rds')

## 2- Unscale and uncentre variables ====

# Variable means
mu_NNdist <- mean(dat$lStartDist, na.rm = T)
mu_wang <- mean(dat$Wang_Start_NN, na.rm = T)
mu_sri <- mean(dat$lsri_startNN, na.rm = T)

# Unscale and convert to km if distance
rss_id_NNdist$id$NN_Distance_unsc <- exp(rss_id_NNdist$id$NN_Distance + mu_NNdist) - 0.125
rss_pop_NNdist$pop$NN_Distance_unsc <- exp(rss_pop_NNdist$pop$NN_Distance + mu_NNdist) - 0.125
rss_pop_wang$pop$Relatedness_unsc <- rss_pop_wang$pop$Relatedness + mu_wang
rss_id_wang$id$Relatedness_unsc <- rss_id_wang$id$Relatedness + mu_wang
rss_id_sri$id$Soc_Var_unsc <- exp(rss_id_sri$id$Soc_Var + mu_sri) - 0.125
rss_pop_sri$pop$Soc_Var_unsc <- exp(rss_pop_sri$pop$Soc_Var + mu_sri) - 0.125

# Log RSS
rss_id_NNdist$id$logRSS <- log(rss_id_NNdist$id$logRSS)
rss_pop_NNdist$pop$logRSS <- log(rss_pop_NNdist$pop$logRSS)
rss_pop_wang$pop$logRSS <- log(rss_pop_wang$pop$logRSS)
rss_id_wang$id$logRSS <- log(rss_id_wang$id$logRSS)
rss_id_sri$id$logRSS <- log(rss_id_sri$id$logRSS)
rss_pop_sri$pop$logRSS <- log(rss_pop_sri$pop$logRSS)

## 3- Plot the RSS (presentations) ====

# Nearest neighbour distance
# Log RSS for open habitat versus closed
NN_plot_pres <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#CFE3D8") +
  geom_line(data = rss_id_NNdist$id, 
            aes(x = NN_Distance_unsc, y = logRSS, group = ANIMAL_ID),
            linewidth = 0.25, colour = '#9FB7AF') +
  geom_line(data = rss_pop_NNdist$pop, 
            aes(x = NN_Distance_unsc, y = logRSS),
            linewidth = 1, colour = '#F2C14E') +
  scale_x_continuous(breaks = c(100, 900), labels = c("50 metres", "1 kilometer"), limits = c(0, 1000)) +
  theme(plot.background = element_rect(colour = '#345a49', fill = '#345a49'),,
        panel.background = element_rect(colour = '#345a49', fill = '#345a49'),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.25, 0.25, 1, 1), 'cm'),
        axis.text = element_text(size = 18, colour = '#CFE3D8'),
        legend.text = element_text(size = 15, colour = '#CFE3D8'),
        legend.title = element_text(size = 18, colour = '#CFE3D8'),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 18, colour = '#F4F7F5', vjust = -5),
        axis.title.y = element_text(size = 18, colour = '#F4F7F5', vjust = 5)) +
  labs(x = 'Distance to nearest neighbour', y = 'Strength of selection for open habitat')

# Relatedness
# Log RSS for open habitat versus closed
Wang_plot_pres <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#CFE3D8") +
  geom_line(data = rss_id_wang$id, 
            aes(x = Relatedness_unsc, y = logRSS, group = ANIMAL_ID),
            linewidth = 0.25, colour = '#9FB7AF') +
  geom_line(data = rss_pop_wang$pop, 
            aes(x = Relatedness_unsc, y = logRSS),
            linewidth = 1, colour = '#F2C14E') +
  scale_x_continuous(breaks = c(0.05, 0.45), labels = c("unrelated", "full siblings"), limits = c(0, 0.5)) +
  theme(plot.background = element_rect(colour = '#345a49', fill = '#345a49'),,
        panel.background = element_rect(colour = '#345a49', fill = '#345a49'),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.25, 0.25, 1, 1), 'cm'),
        axis.text = element_text(size = 18, colour = '#CFE3D8'),
        legend.text = element_text(size = 15, colour = '#CFE3D8'),
        legend.title = element_text(size = 18, colour = '#CFE3D8'),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 18, colour = '#F4F7F5', vjust = -5),
        axis.title.y = element_text(size = 18, colour = '#F4F7F5', vjust = 5)) +
  labs(x = 'Relatedness index', y = 'Strength of selection for open habitat') +
  ylim(0, 1.5)

# SRI
# Log RSS for open habitat versus closed
SRI_plot_pres <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#CFE3D8") +
  geom_line(data = rss_id_sri$id, 
            aes(x = Soc_Var_unsc, y = logRSS, group = ANIMAL_ID),
            linewidth = 0.25, colour = '#9FB7AF') +
  geom_line(data = rss_pop_sri$pop, 
            aes(x = Soc_Var_unsc, y = logRSS),
            linewidth = 1, colour = '#F2C14E') +
  scale_x_continuous(breaks = c(0.05, 0.25), labels = c("low familiarity", "high familiarity"), limits = c(0, 0.3)) +
  theme(plot.background = element_rect(colour = '#345a49', fill = '#345a49'),,
        panel.background = element_rect(colour = '#345a49', fill = '#345a49'),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.25, 0.25, 1, 1), 'cm'),
        axis.text = element_text(size = 18, colour = '#CFE3D8'),
        legend.text = element_text(size = 15, colour = '#CFE3D8'),
        legend.title = element_text(size = 18, colour = '#CFE3D8'),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 18, colour = '#F4F7F5', vjust = -5),
        axis.title.y = element_text(size = 18, colour = '#F4F7F5', vjust = 5)) +
  labs(x = 'Simple ratio index', y = 'Strength of selection for open habitat') +
  ylim(0, 40)

## 4- Plot the RSS (manuscript) ====

# Nearest neighbour distance
# Log RSS for open habitat versus closed
NN_plot_MS <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_line(data = rss_id_NNdist$id, 
            aes(x = NN_Distance_unsc, y = logRSS, group = ANIMAL_ID),
            linewidth = 0.25, colour = '#0057D9') +
  geom_line(data = rss_pop_NNdist$pop, 
            aes(x = NN_Distance_unsc, y = logRSS),
            linewidth = 1, colour = '#1F4E79') +
  scale_x_continuous(breaks = c(1000, 5000, 10000), labels = c(1, 5, 10), limits = c(0, 10100)) +
  theme(plot.background = element_rect(colour = 'white', fill = 'white'),,
        panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.25, 0.25, 1, 1), 'cm'),
        axis.text = element_text(size = 13, colour = 'black'),
        legend.text = element_text(size = 13, colour = 'black'),
        legend.title = element_text(size = 13, colour = 'black'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        axis.title.x = element_text(size = 13, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 13, colour = 'black', vjust = 5)) +
  labs(x = 'Distance to nearest neighbour (km)', y = 'Log RSS for open habitat')

# Relatedness
# Log RSS for open habitat versus closed
Wang_plot_MS <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_line(data = rss_id_wang$id, 
            aes(x = Relatedness_unsc, y = logRSS, group = ANIMAL_ID),
            linewidth = 0.25, colour = '#0057D9') +
  geom_line(data = rss_pop_wang$pop, 
            aes(x = Relatedness_unsc, y = logRSS),
            linewidth = 1, colour = '#1F4E79') +
  theme(plot.background = element_rect(colour = 'white', fill = 'white'),,
        panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.25, 0.25, 1, 1), 'cm'),
        axis.text = element_text(size = 13, colour = 'black'),
        legend.text = element_text(size = 13, colour = 'black'),
        legend.title = element_text(size = 13, colour = 'black'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        axis.title.x = element_text(size = 13, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 13, colour = 'black', vjust = 5)) +
  labs(x = 'Relatedness index', y = 'Log RSS for open habitat') 

# SRI
# Log RSS for open habitat versus closed
SRI_plot_MS <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_line(data = rss_id_sri$id, 
            aes(x = Soc_Var_unsc, y = logRSS, group = ANIMAL_ID),
            linewidth = 0.25, colour = '#0057D9') +
  geom_line(data = rss_pop_sri$pop, 
            aes(x = Soc_Var_unsc, y = logRSS),
            linewidth = 1, colour = '#1F4E79') +
  theme(plot.background = element_rect(colour = 'white', fill = 'white'),,
        panel.background = element_rect(colour = 'white', fill = 'white'),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.25, 0.25, 1, 1), 'cm'),
        axis.text = element_text(size = 13, colour = 'black'),
        legend.text = element_text(size = 13, colour = 'black'),
        legend.title = element_text(size = 13, colour = 'black'),
        axis.line = element_line(colour = 'black', linewidth = 1),
        axis.title.x = element_text(size = 13, colour = 'black', vjust = -5),
        axis.title.y = element_text(size = 13, colour = 'black', vjust = 5)) +
  labs(x = 'Simple ratio index', y = 'Log RSS for open habitat')

## 4- Write plots ====

ggsave("plots/presentation_NN_plot.pdf", NN_plot_pres, device = "pdf", width = 6, height = 5, units = "in", dpi = 400)
ggsave("plots/presentation_Wang_plot.pdf", Wang_plot_pres, device = "pdf", width = 6, height = 5, units = "in", dpi = 400)
ggsave("plots/presentation_SRI_plot.pdf", SRI_plot_pres, device = "pdf", width = 6, height = 5, units = "in", dpi = 400)

ggsave("plots/MS_NN_plot.tiff", NN_plot_MS, device = "tiff", width = 4.5, height = 4, units = "in", dpi = 400)
ggsave("plots/MS_Wang_plot.tiff", Wang_plot_MS, device = "tiff", width = 4.5, height = 4, units = "in", dpi = 400)
ggsave("plots/MS_SRI_plot.tiff", SRI_plot_MS, device = "tiff", width = 4.5, height = 4, units = "in", dpi = 400)

