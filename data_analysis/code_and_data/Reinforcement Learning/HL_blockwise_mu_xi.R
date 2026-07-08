library(dplyr)
library(tidybayes)
library(ggplot2)

# Add 'block' and 'condition' metadata for mu_xi
hb1_draws  <- hb1$fit  %>% gather_draws(mu_xi) %>% mutate(block = "1", condition = "HTA")
hb2_draws  <- hb2$fit  %>% gather_draws(mu_xi) %>% mutate(block = "2", condition = "HTA")
hb3v1_draws<- hb3v1$fit %>% gather_draws(mu_xi) %>% mutate(block = "3v1", condition = "HTA")
hb3v2_draws<- hb3v2$fit %>% gather_draws(mu_xi) %>% mutate(block = "3v2", condition = "HTA")
hb4_draws  <- hb4$fit  %>% gather_draws(mu_xi) %>% mutate(block = "4", condition = "HTA")

lb1_draws  <- lb1$fit  %>% gather_draws(mu_xi) %>% mutate(block = "1", condition = "LTA")
lb2_draws  <- lb2$fit  %>% gather_draws(mu_xi) %>% mutate(block = "2", condition = "LTA")
lb3v1_draws<- lb3v1$fit %>% gather_draws(mu_xi) %>% mutate(block = "3v1", condition = "LTA")
lb3v2_draws<- lb3v2$fit %>% gather_draws(mu_xi) %>% mutate(block = "3v2", condition = "LTA")
lb4_draws  <- lb4$fit  %>% gather_draws(mu_xi) %>% mutate(block = "4", condition = "LTA")

# Combine all
all_mu_xi_draws <- bind_rows(
  hb1_draws, hb2_draws, hb3v1_draws, hb3v2_draws, hb4_draws,
  lb1_draws, lb2_draws, lb3v1_draws, lb3v2_draws, lb4_draws
)

# Plot
ggplot(all_mu_xi_draws, aes(x = block, y = .value, fill = condition)) +
  geom_boxplot(position = position_dodge(width = 0.6), outlier.alpha = 0.2) +
  labs(title = "mu_xi: HTA vs LTA by Block", x = "Block", y = "Posterior Samples", fill = "Condition") +
  theme_classic(base_size = 12)

ggsave("mu_xi.png", width = 6, height = 4, dpi = 1000)
