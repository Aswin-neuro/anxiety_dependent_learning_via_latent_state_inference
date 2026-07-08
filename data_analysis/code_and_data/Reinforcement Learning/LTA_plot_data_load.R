library(hBayesDM)
library(rstan)
library(dplyr)
library(ggplot2)
library(tidybayes)

# Load LTA models
lb1 <- readRDS('b1_lta_gngm4.rds')
lb2 <- readRDS('b2_lta_gngm4.rds')
lb3v1 <- readRDS('b3v1_lta_gngm4.rds')
lb3v2 <- readRDS('b3v2_lta_gngm4.rds')
lb4 <- readRDS('b4_lta_gngm4.rds')

# Extract summaries
s_lb1 <- summary(lb1$fit)$summary
mu_lb1 <- s_lb1[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew", "mu_rhoPun"),]
lb1_df <- data.frame(param = rownames(mu_lb1), mean = mu_lb1[, "mean"], se = mu_lb1[, "se_mean"])

s_lb2 <- summary(lb2$fit)$summary
mu_lb2 <- s_lb2[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew", "mu_rhoPun"),]
lb2_df <- data.frame(param = rownames(mu_lb2), mean = mu_lb2[, "mean"], se = mu_lb2[, "se_mean"])

s_lb3v1 <- summary(lb3v1$fit)$summary
mu_lb3v1 <- s_lb3v1[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew", "mu_rhoPun"),]
lb3v1_df <- data.frame(param = rownames(mu_lb3v1), mean = mu_lb3v1[, "mean"], se = mu_lb3v1[, "se_mean"])

s_lb3v2 <- summary(lb3v2$fit)$summary
mu_lb3v2 <- s_lb3v2[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew", "mu_rhoPun"),]
lb3v2_df <- data.frame(param = rownames(mu_lb3v2), mean = mu_lb3v2[, "mean"], se = mu_lb3v2[, "se_mean"])

s_lb4 <- summary(lb4$fit)$summary
mu_lb4 <- s_lb4[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew", "mu_rhoPun"),]
lb4_df <- data.frame(param = rownames(mu_lb4), mean = mu_lb4[, "mean"], se = mu_lb4[, "se_mean"])

# Add model labels
lb1_df$model <- "LB1"
lb2_df$model <- "LB2"
lb3v1_df$model <- "LB3v1"
lb3v2_df$model <- "LB3v2"
lb4_df$model <- "LB4"

# Combine
all_lb_df <- bind_rows(lb1_df, lb2_df, lb3v1_df, lb3v2_df, lb4_df)

# Posterior draws
lb1_draws <- lb1$fit %>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "LB1")
lb2_draws <- lb2$fit %>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "LB2")
lb3v1_draws <- lb3v1$fit %>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "LB3v1")
lb3v2_draws <- lb3v2$fit %>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "LB3v2")
lb4_draws <- lb4$fit %>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "LB4")

all_lb_draws <- bind_rows(lb1_draws, lb2_draws, lb3v1_draws, lb3v2_draws, lb4_draws)

# Plot
plot_lb <- ggplot(all_lb_draws, aes(x = .variable, y = .value, fill = model)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.2) +
  labs(x = "Parameter", y = "Posterior mu", fill = "Model") +
  theme_classic(base_size = 14)

plot_lb
ggsave("RL3_LTA.png", plot = plot_lb, width = 8, height = 6, dpi = 1000)

# Optional: model comparison (if implemented for these models)
printFit(lb1, lb2, lb3v1, lb3v2, lb4)
