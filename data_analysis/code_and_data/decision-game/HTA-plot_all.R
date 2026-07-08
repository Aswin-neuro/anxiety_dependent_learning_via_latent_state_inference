data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
View(data)

library(dplyr)
library(tidyr)
library(ggplot2)

View(data)
# Prepare the data
ab_rt <- data %>%
  select(block, reactionTime, HL) %>%
  filter(!is.na(HL)) %>%  
  group_by(block, HL) %>%
  summarize(
    rt = mean(reactionTime, na.rm = TRUE),
    se = sd(reactionTime, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(block, HL)

ggplot(ab_rt, aes(x = block, y = rt, fill = HL)) +
  geom_bar(stat = 'identity', position = position_dodge(.8)) +
  geom_pointrange(aes(ymin = rt - se, ymax = rt + se, group = HL),
                  position = position_dodge(.8),
                  color = "black", alpha = 1, size = 0.001) +
  geom_text(aes(label = round(rt,2)),
            position = position_dodge(width = 1),
            vjust = -0.3, size = 3)+
  labs(x = 'Block', y = 'RT(ms)', fill = 'HL',
       title = 'Blockwise-RT HTA&LTA') +
  theme_classic(base_size = 9) +
  scale_fill_brewer(palette = 'Set1')

ggsave("HLTA-blockwise-RT.png", width = 6, height = 4, dpi = 1000)

#########################
#### ACC - Blockwise #####

# Prepare the data
ab_acc <- data %>%
  select(block, accuracy, HL) %>%
  filter(!is.na(HL)) %>%  
  group_by(block, HL) %>%
  summarize(
    acc = mean(accuracy, na.rm = TRUE),
    se = sd(accuracy, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(block, HL)

ggplot(ab_acc, aes(x = block, y = acc, fill = HL)) +
  geom_bar(stat = 'identity', position = position_dodge(.8)) +
  geom_pointrange(aes(ymin = acc - se, ymax = acc + se, group = HL),
                  position = position_dodge(.8),
                  color = "black", alpha = 1, size = 0.001) +
  geom_text(aes(label = round(acc,2)),
            position = position_dodge(width = 1),
            vjust = -0.3, size = 3)+
  labs(x = 'Block', y = 'ACC', fill = 'HL',
       title = 'Blockwise ACC HTA&LTA') +
  theme_classic(base_size = 9) +
  scale_fill_brewer(palette = 'Set1')

ggsave("HLTA-blockwise_ACC.png", width = 6, height = 4, dpi = 1000)
