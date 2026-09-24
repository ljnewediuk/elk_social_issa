
### 10 - Visualize the distributions of social variables ====

## 1- Prep workspace ====

# Load libraries
libs <- c('tidyverse')
lapply(libs, require, character.only = TRUE)

# Load data
DT <- readRDS('output/cleaned_model_data.rds')

# Create density plots of variables

# SRI
DT %>%
  ggplot(aes(x = sri_startNN)) +
  geom_density(colour = '#F2C14E', fill = '#F2C14E50') +
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
  labs(x = 'Social relatedness index at start of step', y = 'Density')

# Nearest-neighbour
DT %>%
  ggplot(aes(x = StartDist)) +
  geom_density(colour = '#345a49', fill = '#345a4950') +
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
  labs(x = 'Nearest-neighbour distace at start of step (m)', y = 'Density')

# Relatedness
DT %>%
  ggplot(aes(x = Wang_Start_NN)) +
  geom_density(colour = '#0057D9', fill = '#0057D950') +
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
  labs(x = 'Index of relatedness to elk at start of step', y = 'Density')
