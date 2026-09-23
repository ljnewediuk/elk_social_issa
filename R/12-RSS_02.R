

# Load models


# Convert to function

mod <- model_sri_d

dat <- DT

coeff_name <- "lsri_startNN"

get_rss <- function(mod, dat, coeff_name) {
  # Random effects from model
  re <- ranef(mod)$cond$ANIMAL_ID %>%
    rownames_to_column("ANIMAL_ID")
  
  # Rename intercept and slope cols
  colnames(re)[c(2,3)] <- c("b_0", "b_i")
  # rename(b_0 = `(Intercept)`,
  #        b_i = `lsri_startNN:Open_end`)
  
  beta_open <- fixef(mod)$cond["Open_end"]
  beta_intxn_open <- fixef(mod)$cond[paste0("Open_end:", coeff_name)]
  
  # b_i <- re[, "sri_startNN:Open_end"]
  
  rss <- expand.grid(
    ANIMAL_ID = unique(re$ANIMAL_ID),
    coeff = seq(min(dat[[coeff_name]]), max(dat[[coeff_name]]), length.out = 100)
  ) %>%
    left_join(re) %>%
    mutate(
      coeff_exp = exp(coeff),
      # b_i = b_i[ANIMAL_ID],
      log_RSS = b_0 + beta_open +
        (beta_intxn_open + b_i) * coeff,
      RSS = exp(log_RSS)
    )
  
  # Population RSS 
  
  # Get confidence intervals
  
  # Fixed effects
  b <- fixef(mod)$cond
  
  # Variance-covariance matrix of fixed effects
  V <- vcov(mod)$cond
  
  # SRI values over which you want RSS
  sri <- seq(min(dat[[coeff_name]]), max(dat[[coeff_name]]), length.out = 100)
  
  # Log RSS
  log_rss <- b["Open_end"] +
    b[paste0("Open_end:", coeff_name)] * sri
  
  var_log_rss <- V["Open_end", "Open_end"] +
    sri^2 * V[paste0("Open_end:", coeff_name), paste0("Open_end:", coeff_name)] +
    2 * sri * V["Open_end", paste0("Open_end:", coeff_name)]
  
  se_log_rss <- sqrt(var_log_rss)
  
  lowerci <- log_rss - 1.96 * se_log_rss
  upperci <- log_rss + 1.96 * se_log_rss
  
  # Population RSS
  rss_pop <- data.frame(
    coeff = seq(min(dat[[coeff_name]]), max(dat[[coeff_name]]), length.out = 100)
  ) %>%
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

sri_rss <- get_rss(model_sri_d, DT, "lsri_startNN")
wang_rss <- get_rss(model_wang_d, DT, "Wang_Start_NN")
prox_rss <- get_rss(model_prox_d, DT, "lStartDist")

# PLot
ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_ribbon(data = wang_rss$pop, 
              aes(x = coeff_exp, ymin = lower, ymax = upper),
              fill = '#1F4E7950', colour = NA) +
  geom_line(data = wang_rss$pop,
            aes(x = coeff_exp, y = log_RSS),
            linewidth = 1, colour = '#1F4E79') +
  geom_line(data = wang_rss$id, 
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

# ggsave("plots/MS_Wang_plot.tiff", last_plot(), device = "tiff", width = 6.5, height = 5.5, units = "in", dpi = 400)
