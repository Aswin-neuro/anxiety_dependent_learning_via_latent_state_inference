# all_u -> b{1[x],2[x],3[x],4[x]}
# x = {PC , PI} -> plot %acc -> plot x2 x4 = 8
# x = {PC[go], PI[go]} or stimulus {1, 2}-> plot rt x2 x4 = 8
data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
library(dplyr)
library(ggplot2)
library(tidyr)

u_acc <- data %>% 
  group_by(userId,block,pav,HL,stimulus) %>% 
  summarize(
    accuracy = mean(accuracy, na.rm = TRUE),
    .groups = 'drop'
  )
View(u_acc)
# mean and sd across user per blockxpav
ubp_ci <- u_acc %>% 
  filter(!is.na(HL)) %>%   # <-- Filter NA HL here
  group_by(block, pav, HL, stimulus) %>% 
  summarize(
    acc_mean = mean(accuracy, na.rm = TRUE),
    acc_sd = sd(accuracy, na.rm = TRUE),
    acc_se = acc_sd/sqrt(n()),
    stimulus = stimulus,
    .groups = 'drop'
  )
View(ubp_ci)

#Plotting renaming on long
ubp_ci_long <- ubp_ci %>% 
  rename(proportion = acc_mean, 
         se = acc_se, 
         congruence = pav,
         stimulus = stimulus)


ubp_ci_long$congruence <- factor(ubp_ci_long$congruence, levels = c('PC', 'PI'))
# ubp_ci_long$HL <- factor(ubp_ci_long$HL, levels = c('HTA', 'LTA'))#sorting blocks
block_vals <- sort(unique(data$block))
#plot
ggplot(ubp_ci_long, aes(x = factor(block), y = proportion, fill = HL))+
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  geom_pointrange(aes(y = proportion,
                      ymin = proportion - se, 
                      ymax = proportion + se), 
                  position = position_dodge(0.8),
                  color = "black", alpha = 1, size = 0.01)+
  geom_text(aes(label = round(proportion, 2)),
            position = position_dodge(0.8),
            vjust = -0.5, size = 2.5) +
  scale_x_discrete(breaks = block_vals, labels = paste0("B", block_vals)) +
  labs(x = "Blocks", y = "Proportion", fill = 'pavlovian congruence', title = "Blockwise PC & PI Accuracy (HTA vs LTA)") +
  facet_wrap(~stimulus)+
  coord_cartesian(ylim = c(0,1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic(base_size = 8)

ggsave("HLTA-blockwise-PC&PI-accuracy.png", width = 6, height = 4, dpi = 1000)
################# without HTA and  LTA ######################

ub_ci_block_pav <- u_acc %>%
  group_by(block, pav) %>%
  summarize(
    proportion = mean(accuracy, na.rm = TRUE),
    se = sd(accuracy, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  rename(congruence = pav)

block_vals <- sort(unique(data$block))

ggplot(ub_ci_block_pav, aes(x = factor(block), y = proportion, fill = congruence)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  geom_pointrange(aes(ymin = proportion - se, ymax = proportion + se),
                  position = position_dodge(0.8),
                  color = "black", size = 0.2) +
  geom_text(aes(label = round(proportion, 2)),
            position = position_dodge(0.8), vjust = -0.5, size = 2.5) +
  scale_x_discrete(breaks = block_vals, labels = paste0("B", block_vals)) +
  labs(x = "Blocks", y = "Accuracy", fill = "Pavlovian Congruence", title = "PC vs PI Accuracy per Block") +
  scale_fill_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0,1)) +
  theme_classic(base_size = 8)

ggsave("1_HLTA-blockwise-PC&PI-accuracy.png", width = 6, height = 4, dpi = 1000)
