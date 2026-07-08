data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
library(dplyr)
library(ggplot2)
library(tidyr)

stim_acc <- data %>%
  filter(block == '1') %>% 
  group_by(stimulus) %>%
  summarize(
    acc_mean = mean(accuracy, na.rm = TRUE),
    acc_se = sd(accuracy, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

stim_acc$stimulus <- factor(stim_acc$stimulus, levels = 1:4, labels = c("GW", "GAL", "NAL", "NW"))

ggplot(stim_acc, aes(x = stimulus, y = acc_mean)) +
  geom_col(fill = "blue", alpha = 0.7, width = 0.6) +
  geom_pointrange(aes(ymin = acc_mean - acc_se, ymax = acc_mean + acc_se),
                  color = "black", size = 0.2) +
  geom_text(aes(label = round(acc_mean, 2)), vjust = -0.5, size = 2.5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Stimulus", y = "Mean Accuracy", title = "Stimulus-wise Accuracy") +
  theme_classic(base_size = 10)

ggsave("b1_stimuluswise_accuracy.png", width = 6, height = 4, dpi = 1000)

#### reaction time ######
stim_rt <- data %>%
  group_by(stimulus) %>%
  summarize(
    rt_mean = mean(reactionTime, na.rm = TRUE),
    rt_se = sd(reactionTime, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

stim_rt$stimulus <- factor(stim_rt$stimulus, levels = 1:4, labels = c("GW", "GAL", "NAL", "NW"))

ggplot(stim_rt, aes(x = stimulus, y = rt_mean)) +
  geom_col(fill = "blue", alpha = 0.7, width = 0.6) +
  geom_pointrange(aes(ymin = rt_mean - rt_se, ymax = rt_mean + rt_se),
                  color = "black", size = 0.2) +
  geom_text(aes(label = round(rt_mean, 2)), vjust = -0.5, size = 2.5) +
  coord_cartesian(ylim = c(0, max(stim_rt$rt_mean + stim_rt$rt_se))) +
  labs(x = "Stimulus", y = "Mean RT (ms)", title = "Stimulus-wise Reaction Time") +
  theme_classic(base_size = 10)

ggsave("1_stimuluswise_reactiontime.png", width = 6, height = 4, dpi = 1000)
