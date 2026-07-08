library(hBayesDM)
library(rstan)
library(dplyr)
library(ggplot2)
library(tidybayes)
hb1 <- readRDS('b1_hta_gngm4.rds')
hb2 <- readRDS('b2_hta_gngm4.rds')
hb3v1 <- readRDS('b3v1_hta_gngm4.rds')
hb3v2 <- readRDS('b3v2_hta_gngm4.rds')
hb4 <- readRDS('b4_hta_gngm4.rds')

hb1$fit
hb2$fit
hb3v1$fit
hb3v2$fit
hb4$fit

# HB1
# data extract
s_hb1 <- summary(hb1$fit)$summary
mu_hb1 <- s_hb1[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew","mu_rhoPun"),]
hb1_mean <- mu_hb1[, "mean"]
hb1_se <- mu_hb1[, "se_mean"]
hb1_param <- rownames(mu_hb1)
hb1_df <- data.frame(hb1_param, hb1_mean, hb1_se)
View(hb1_df)

# HB2
s_hb2 <- summary(hb2$fit)$summary
mu_hb2 <- s_hb2[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew", "mu_rhoPun"),]
hb2_mean <- mu_hb2[, "mean"]
hb2_se <- mu_hb2[, "se_mean"]
hb2_param <- rownames(mu_hb2)
hb2_df <- data.frame(param = hb2_param, mean = hb2_mean, se = hb2_se)

#HB3v1
s_hb3v1 <- summary(hb3v1$fit)$summary
mu_hb3v1 <- s_hb3v1[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew", "mu_rhoPun"),]
hb3v1_mean <- mu_hb3v1[, "mean"]
hb3v1_se <- mu_hb3v1[, "se_mean"]
hb3v1_param <- rownames(mu_hb3v1)
hb3v1_df <- data.frame(param = hb3v1_param, mean = hb3v1_mean, se = hb3v1_se)

#HB3v2
s_hb3v2 <- summary(hb3v2$fit)$summary
mu_hb3v2 <- s_hb3v2[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew", "mu_rhoPun"),]
hb3v2_mean <- mu_hb3v2[, "mean"]
hb3v2_se <- mu_hb3v2[, "se_mean"]
hb3v2_param <- rownames(mu_hb3v2)
hb3v2_df <- data.frame(param = hb3v2_param, mean = hb3v2_mean, se = hb3v2_se)

#HB4
s_hb4 <- summary(hb4$fit)$summary
mu_hb4 <- s_hb4[c("mu_xi", "mu_ep", "mu_b", "mu_pi", "mu_rhoRew", "mu_rhoPun"),]
hb4_mean <- mu_hb4[, "mean"]
hb4_se <- mu_hb4[, "se_mean"]
hb4_param <- rownames(mu_hb4)
hb4_df <- data.frame(param = hb4_param, mean = hb4_mean, se = hb4_se)

## Plotting

# Add model labels
hb1_df$model <- "HB1"
hb2_df$model <- "HB2"
hb3v1_df$model <- "HB3v1"
hb3v2_df$model <- "HB3v2"
hb4_df$model <- "HB4"

# Combine all
all_df <- bind_rows(hb1_df, hb2_df, hb3v1_df, hb3v2_df, hb4_df)
# Bar plot
hb_list <- list(HB1 = hb1, HB2 = hb2, HB3v1 = hb3v1, HB3v2 = hb3v2, HB4 = hb4)


hb1_draws  <- hb1$fit  %>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "HB1")
hb2_draws  <- hb2$fit  %>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "HB2")
hb3v1_draws<- hb3v1$fit%>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "HB3v1")
hb3v2_draws<- hb3v2$fit%>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "HB3v2")
hb4_draws  <- hb4$fit  %>% gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>% mutate(model = "HB4")

all_draws <- bind_rows(hb1_draws, hb2_draws, hb3v1_draws, hb3v2_draws, hb4_draws)


plot <- ggplot(all_draws, aes(x = .variable, y = .value, fill = model)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.2) +
  coord_flip() +
  labs(x = "Parameter", y = "Posterior mu", fill = "Model") +
  theme_classic(base_size = 14)

ggsave("RL3.png", plot = plot1, width = 8, height = 6, dpi = 1000)

## expt
library(tidybayes)

hb1_draws <- hb1$fit %>%
  gather_draws(mu_xi, mu_ep, mu_b, mu_pi, mu_rhoRew, mu_rhoPun) %>%
  mutate(model = "HB1")

# Repeat for HB2–HB4, then bind_rows()

plot1 <- ggplot(all_draws, aes(x = .variable, y = .value, fill = model)) +
  geom_boxplot(position = position_dodge(width = 0.7)) +
  labs(x = "Parameter", y = "Posterior Samples") +
  theme_classic(base_size = 9)
plot1

## Compare models
printFit(hb1,hb2,hb3v1,hb3v2,hb4)
