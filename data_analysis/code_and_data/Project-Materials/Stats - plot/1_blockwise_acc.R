data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
View(data)
library(dplyr)
library(ggplot2)
library(tidyr)

# Blockwise accuracy without HL or pav differentiation
block_acc <- data %>% 
  group_by(userId, block, stimulus) %>% 
  summarize(
    accuracy = mean(accuracy, na.rm = TRUE),
    .groups = 'drop'
  )
block_vals <- sort(unique(data$block))
# Calculate mean and SE across users per block
block_ci <- block_acc %>% 
  group_by(block, stimulus) %>% 
  summarize(
    acc_mean = mean(accuracy, na.rm = TRUE),
    acc_sd = sd(accuracy, na.rm = TRUE),
    acc_se = acc_sd/sqrt(n()),
    .groups = 'drop'
  ) %>% 
  rename(proportion = acc_mean, 
         se = acc_se)

# Plot
ggplot(block_ci, aes(x = factor(block), y = proportion)) +
  geom_col(position = position_dodge(0.8), width = 0.7, fill = "red", alpha = 0.8) +
  geom_pointrange(aes(y = proportion,
                       ymin = proportion - se, 
                       ymax = proportion + se), 
                   position = position_dodge(0.8),
                   color = "black", alpha = 1, size = 0.01)+
  geom_text(aes(label = round(proportion, 2)),
            position = position_dodge(0.8),
            vjust = -0.5, size = 3) +
  scale_x_discrete(breaks = block_vals, labels = paste0("B", block_vals)) +
  labs(x = "Blocks", y = "Proportion", title = "Blockwise Accuracy") +
  facet_wrap(~stimulus) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 10)

ggsave("1_blockwise-accuracy.png", width = 6, height = 4, dpi = 1000)
