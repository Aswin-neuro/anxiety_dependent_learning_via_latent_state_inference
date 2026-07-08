# Load and preprocess
data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
library(dplyr)
library(ggplot2)

data$stimulus <- factor(data$stimulus, levels = 1:4, labels = c("GW", "GAL", "NAL", "NW"))

# Boxplot: Accuracy by stimulus
ggplot(data, aes(x = stimulus, y = accuracy)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, outlier.size = 0.5) +
  labs(x = "Stimulus", y = "Accuracy", title = "Stimulus-wise Accuracy (Boxplot)") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 10)

ggsave("boxplot_stimuluswise_accuracy.png", width = 6, height = 4, dpi = 1000)

# Barplot with error bars: Accuracy mean ± SE
stim_acc <- data %>%
  group_by(stimulus) %>%
  summarize(
    acc_mean = mean(accuracy, na.rm = TRUE),
    acc_se = sd(accuracy, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(stim_acc, aes(x = stimulus, y = acc_mean)) +
  geom_col(fill = "blue", alpha = 0.7, width = 0.6) +
  geom_pointrange(aes(ymin = acc_mean - acc_se, ymax = acc_mean + acc_se),
                  color = "black", size = 0.2) +
  geom_text(aes(label = round(acc_mean, 2)), vjust = -0.5, size = 2.5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Stimulus", y = "Mean Accuracy", title = "Stimulus-wise Accuracy") +
  theme_classic(base_size = 10)

ggsave("barplot_stimuluswise_accuracy.png", width = 6, height = 4, dpi = 1000)
