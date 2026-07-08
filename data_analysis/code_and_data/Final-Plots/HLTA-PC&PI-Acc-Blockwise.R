data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')

View(data)
library(dplyr)
library(ggplot2)
library(tidyr)


# data <- data[ ,c('userId','block','stimulus','reactionTime','pav', 'HA')]
u_acc <- data %>% 
  group_by(userId,block,pav,HL) %>% 
  summarize(
    acc = mean(accuracy, na.rm = TRUE),
    .groups = 'drop'
  )

ubp_ci <- u_acc %>% 
  filter(!is.na(HL)) %>% 
  group_by(block, pav, HL) %>% 
  summarize(
    acc_mean = mean(acc, na.rm = TRUE),
    acc_sd = sd(acc, na.rm = TRUE),
    acc_se = acc_sd/sqrt(n()),
    .groups = 'drop'
  )

#Plotting renaming on long
ubp_ci_long <- ubp_ci %>% 
  rename(proportion = acc_mean,
         se = acc_se, 
         congruence = pav)
ubp_ci_long$HL <- factor(ubp_ci_long$HL, levels = c('HTA','LTA'))
#sorting blocks
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
  labs(x = "Blocks", y = "Accuracy", fill = 'pavlovian congruence', title = "Blockwise PC&PI Accuracy (HTAvsLTA)") +
  facet_wrap(~congruence)+
  # coord_cartesian(ylim = c(0,1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic(base_size = 8)

ggsave("HLTA-blockwise-PC&PI-Accuracy.png", width = 6, height = 4, dpi = 1000)

### Without LTA and HTA ###############################################

# Load and prepare
data <- read.csv("/home/aswin/R/Projects/final_game-data.csv")
library(dplyr)
library(ggplot2)
library(tidyr)
View(data)

# Collapse HL (remove separation)
u_acc <- data %>% 
  group_by(userId, block, pav) %>% 
  summarize(
    acc = mean(accuracy, na.rm = TRUE),
    .groups = 'drop'
  )

# Compute summary statistics
ub_ci <- u_acc %>% 
  group_by(block, pav) %>% 
  summarize(
    acc_mean = mean(acc, na.rm = TRUE),
    acc_sd = sd(acc, na.rm = TRUE),
    acc_se = acc_sd / sqrt(n()),
    .groups = 'drop'
  )

# Prepare long format for plotting
ub_ci_long <- ub_ci %>%
  rename(proportion = acc_mean,
         se = acc_se,
         congruence = pav)

block_vals <- sort(unique(data$block))

# Plot
ggplot(ub_ci_long, aes(x = factor(block), y = proportion, fill = congruence)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.8) +
  geom_pointrange(aes(ymin = proportion - se, ymax = proportion + se),
                  position = position_dodge(width = 0.7),
                  color = "black", size = 0.01) +
  geom_text(aes(label = round(proportion, 2)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 2.5) +
  scale_x_discrete(breaks = block_vals, labels = paste0("B", block_vals)) +
  labs(x = "Blocks", y = "Accuracy", fill = "Pavlovian Congruence", title = "Blockwise Pavlovian - Accuracy") +
  scale_fill_brewer(palette = "Set1") +
  theme_classic(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
    axis.ticks.length = unit(0.25, "cm"),
    axis.text.x.top = element_blank(),
    axis.line.x = element_line(),
    plot.margin = margin(10, 10, 10, 10),
    panel.spacing = unit(2, "lines")
  )

ggsave("Blockwise-pavlovian-Accuracy.png", width = 6, height = 4, dpi = 1000)

