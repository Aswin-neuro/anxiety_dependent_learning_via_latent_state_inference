data <- read.csv("/home/aswin/R/Projects/decision-game/game_data.csv")
data <- data[,-c(14:26)]
data$accuracy <- data$correct == "1" # will get TF for true or false
pav <- ifelse(data$stimulus == "1" | data$stimulus == "3", "PC", "PI")
data$pav <- pav 
rt <- data$reactionTime*0.001
data$rt <- rt

library(dplyr)
library(ggplot2)
library(tidyr)

data <- data[ ,c('userId','block','stimulus','reactionTime','pav')]
View(data)

u_rt <- data %>% 
  group_by(userId,block,pav) %>% 
  summarize(
    rt = mean(reactionTime, na.rm = TRUE),
    .groups = 'drop'
  )


ubp_rt <- u_rt %>% 
  group_by(block, pav) %>% 
  summarize(
    rt_mean = mean(rt, na.rm = TRUE),
    rt_sd = sd(rt, na.rm = TRUE),
    num = n(),
    rt_se = rt_sd/num,
    .groups = 'drop'
  )

View(ubp_rt)

#Plotting renaming on long
ubp_rt_long <- ubp_rt %>% 
  rename(proportion = rt_mean, rt_se = rt_se, congruence = pav)
#sorting blocks
block_vals <- sort(unique(data$block))
#plot
ggplot(ubp_rt_long, aes(x = factor(block), y = proportion, fill = congruence))+
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  geom_pointrange(aes(y = proportion,
                      ymin = proportion - rt_se, 
                      ymax = proportion + rt_se), 
                  position = position_dodge(0.8),
                  color = "black", alpha = 1, size = 0.01)+
  scale_x_discrete(breaks = block_vals, labels = paste0("B", block_vals)) +
  labs(x = "Blocks", y = "Proportion", title = "Blockwise-PC&PI-RT") +
  coord_cartesian(ylim = c(0,1000)) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic(base_size = 8)
ubp_rt$rt_mean
ggsave("blockwise-PC&PI-RT.png", width = 6, height = 4, dpi = 1000)


