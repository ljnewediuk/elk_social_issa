
### 08 - Plot the RSS ====

## 1- Prep workspace ====

# Load libraries
libs <- c('data.table', 'tidyverse')
lapply(libs, require, character.only = TRUE)

# Load RSS
rss_sri <- readRDS('models/rss_sri.rds')
rss_prox <- readRDS('models/rss_prox.rds')
rss_wang <- readRDS('models/rss_wang.rds')

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
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#CFE3D8") +
  geom_ribbon(data = prox_rss$pop, 
              aes(x = coeff_exp, ymin = lower, ymax = upper),
              fill = '#F2C14E50', colour = NA) +
  geom_line(data = prox_rss$pop,
            aes(x = coeff_exp, y = log_RSS),
            linewidth = 1, colour = '#F2C14E') +
  geom_line(data = prox_rss$id, 
            aes(x = coeff_exp, y = log_RSS, group = ANIMAL_ID),
            linewidth = 0.25, colour = '#9FB7AF') +
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
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#CFE3D8") +
  geom_ribbon(data = wang_rss$pop, 
              aes(x = log(coeff_exp), ymin = lower, ymax = upper),
              fill = '#F2C14E50', colour = NA) +
  geom_line(data = wang_rss$pop,
            aes(x = log(coeff_exp), y = log_RSS),
            linewidth = 1, colour = '#F2C14E') +
  geom_line(data = wang_rss$id, 
            aes(x = log(coeff_exp), y = log_RSS, group = ANIMAL_ID),
            linewidth = 0.1, colour = '#9FB7AF') +
  scale_x_continuous(breaks = c(-0.6, 0.45), labels = c("unrelated", "full siblings")) +
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
  labs(x = 'Relatedness index', y = 'Strength of selection for open habitat') 

# SRI
# Log RSS for open habitat versus closed
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#CFE3D8") +
  
  geom_ribbon(data = sri_rss$pop, 
              aes(x = coeff_exp, ymin = lower, ymax = upper),
              fill = '#F2C14E50', colour = NA) +
  geom_line(data = sri_rss$pop,
            aes(x = coeff_exp, y = log_RSS),
            linewidth = 1, colour = '#F2C14E') +
  geom_line(data = sri_rss$id, 
            aes(x = coeff_exp, y = log_RSS, group = ANIMAL_ID),
            linewidth = 0.1, colour = '#9FB7AF') +
  scale_x_continuous(breaks = c(0.1, 1), labels = c("low familiarity", "high familiarity")) +
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
  labs(x = 'Simple ratio index', y = 'Strength of selection for open habitat')

## 4- Plot the RSS (manuscript) ====

# Nearest neighbour distance
# Log RSS for open habitat versus closed
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_ribbon(data = prox_rss$pop, 
              aes(x = coeff_exp, ymin = lower, ymax = upper),
              fill = '#1F4E7950', colour = NA) +
  geom_line(data = prox_rss$pop,
            aes(x = coeff_exp, y = log_RSS),
            linewidth = 1, colour = '#1F4E79') +
  geom_line(data = prox_rss$id, 
            aes(x = coeff_exp, y = log_RSS, group = ANIMAL_ID),
            linewidth = 0.1, colour = '#0057D9') +
  scale_x_continuous(breaks = c(0, 500, 1000, 1500, 2000), labels = c(0, 0.5, 1.0, 1.5, 2.0), limits = c(0, 2100)) +
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

# Save
ggsave("plots/MS_NN_plot.tiff", last_plot(), device = "tiff", width = 6.5, height = 5.5, units = "in", dpi = 400)

# Relatedness
# Log RSS for open habitat versus closed
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_ribbon(data = wang_rss$pop, 
              aes(x = log(coeff_exp), ymin = lower, ymax = upper),
              fill = '#1F4E7950', colour = NA) +
  geom_line(data = wang_rss$pop,
            aes(x = log(coeff_exp), y = log_RSS),
            linewidth = 1, colour = '#1F4E79') +
  geom_line(data = wang_rss$id, 
            aes(x = log(coeff_exp), y = log_RSS, group = ANIMAL_ID),
            linewidth = 0.1, colour = '#0057D9') +
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

# Save
ggsave("plots/MS_Wang_plot.tiff", last_plot(), device = "tiff", width = 6.5, height = 5.5, units = "in", dpi = 400)

# SRI
# Log RSS for open habitat versus closed
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_ribbon(data = sri_rss$pop, 
              aes(x = coeff_exp, ymin = lower, ymax = upper),
              fill = '#1F4E7950', colour = NA) +
  geom_line(data = sri_rss$pop,
            aes(x = coeff_exp, y = log_RSS),
            linewidth = 1, colour = '#1F4E79') +
  geom_line(data = sri_rss$id, 
            aes(x = coeff_exp, y = log_RSS, group = ANIMAL_ID),
            linewidth = 0.1, colour = '#0057D9') +
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

# Save
ggsave("plots/MS_SRI_plot.tiff", last_plot(), device = "tiff", width = 6.5, height = 5.5, units = "in", dpi = 400)
