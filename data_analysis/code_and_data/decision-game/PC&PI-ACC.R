data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
View(data)
data <- data[,-c(14:26)]
data$accuracy <- data$correct == "1"
data$pav <- ifelse(data$stimulus %in% c(1, 3), "PC", "PI")
data$rt <- data$reactionTime * 0.001

library(dplyr)
library(ggplot2)
library(tidyr)

# user-level accuracy
u_acc <- data %>% 
  group_by(userId,block,pav) %>% 
  summarize(
    accuracy = mean(accuracy, na.rm = TRUE),
  .groups = 'drop'
  )
View(u_acc)
# mean and sd across user per blockxpav
ubp_ci <- u_acc %>% 
  group_by(block, pav) %>% 
  summarize(
    acc_mean = mean(accuracy, na.rm = TRUE),
    acc_sd = sd(accuracy, na.rm = TRUE),
    num = n(),
    acc_se = acc_sd/num,
    .groups = 'drop'
  )

#Plotting renaming on long
ubp_ci_long <- ubp_ci %>% 
  rename(proportion = acc_mean, acc_se = acc_se, congruence = pav)
#sorting blocks
block_vals <- sort(unique(data$block))
#plot
ggplot(ubp_ci_long, aes(x = factor(block), y = proportion, fill = congruence))+
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  geom_pointrange(aes(y = proportion,
                      ymin = proportion - acc_se, 
                      ymax = proportion + acc_se), 
                  position = position_dodge(0.8),
                  color = "black", alpha = 1, size = 0.01)+
  scale_x_discrete(breaks = block_vals, labels = paste0("B", block_vals)) +
  labs(x = "Blocks", y = "Proportion", title = "Blockwise PC & PI Accuracy") +
  coord_cartesian(ylim = c(0,1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic(base_size = 8)

ggsave("blockwise-PC&PI-accuracy.png", width = 6, height = 4, dpi = 1000)


## x = {PC[go], PI[go]} or stimulus {1, 2}-> plot rt x2 x4 = 8



