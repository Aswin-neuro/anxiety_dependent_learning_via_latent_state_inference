# Initial file process
data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
library(dplyr)
library(ggplot2)
library(tidyr)
View(data)

##############################
# Blockwise accuracy and rt
  # mutate(users = paste0("p", match(userId, unique(userId)))) %>%
ab_acc <- data %>%
  select(c(block, accuracy)) %>%
  group_by(block) %>%
  summarize(
    acc_mean = mean(accuracy, na.rm = TRUE),
    acc_sd = sd(accuracy, na.rm = TRUE),
    n = n(),
    acc_se = acc_sd/sqrt(n),
    .groups = "drop"
  )%>%
  arrange(block)


View(ab_acc)
ggplot(ab_acc) +
  geom_bar(aes(x = block, y = acc_mean, fill = factor(block)), 
           stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  geom_pointrange(aes(x = block, y = acc_mean,
                      ymin = acc_mean - acc_se, 
                      ymax = acc_mean + acc_se), 
                  color = "black", alpha = 1, size = 0.01) +
  scale_x_discrete(labels = paste0("B", ab_acc$block)) +
  geom_text(aes(x = block, y = acc_mean, label = round(acc_mean, 2)),
            vjust = -0.5, size = 2.5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Blocks", y = "Mean-Accuracy", title = "Blockwise-Accuracy") +
  theme_classic(base_size = 10) +
  theme(legend.position = "none")


ggsave("n_Blockwise-accuracy.png", width = 6, height = 4, dpi = 1000)

## Blockwise rt , se*5
ab_rt <- data %>%
  select(block, reactionTime) %>%
  group_by(block) %>%
  summarize(
    rt_mean = mean(reactionTime, na.rm = TRUE),
    rt_sd = sd(reactionTime, na.rm = TRUE),
    n = n(),
    rt_se = rt_sd / sqrt(n),
    .groups = "drop"
  ) %>%
  arrange(block)

View(ab_rt)

# Plot blockwise RT
ggplot(ab_rt) +
  geom_bar(aes(x = block, y = rt_mean, fill = factor(block)), 
           stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  geom_pointrange(aes(x = block, y = rt_mean,
                      ymin = rt_mean - rt_se, 
                      ymax = rt_mean + rt_se), 
                  color = "black", alpha = 1, size = 0.01) +
  scale_x_discrete(labels = paste0("B", ab_rt$block)) +
  geom_text(aes(x = block, y = rt_mean, label = round(rt_mean, 2)),
            vjust = -0.5, size = 2.5) +
  coord_cartesian(ylim = c(0, max(ab_rt$rt_mean + ab_rt$rt_se, na.rm = TRUE))) +
  labs(x = "Blocks", y = "Mean-RT (ms)", title = "Blockwise-Reaction Time") +
  theme_classic(base_size = 10) +
  theme(legend.position = "none")
  ggsave("blockwise-RT.png", width = 6, height = 4, dpi = 1000)
#####################################  
# 3. all_u -> b{1[x],2[x],3[x],4[x]} -> av_ac -> plot = 4
# x = {RBI,PBS} = plots x2 x4 = 8

  #acc
b1_acc <- data %>%
  select(c(block, accuracy, stimulus)) %>%
  group_by(block, stimulus) %>%
  summarize(
    b1acc_mean = mean(accuracy, na.rm = TRUE),
    b1acc_sd = sd(accuracy, na.rm = TRUE),
    n = n(),
    b1acc_se = b1acc_sd/sqrt(n),
    .groups = "drop"
  )%>%
  arrange(block, stimulus)

View(b1_acc)
#plot
b1_acc$stimulus <- factor(b1_acc$stimulus,
                          levels = 1:4,
                          labels = c("GW", "GAL", "NAL", "NW"))

ggplot(b1_acc) +
  geom_bar(aes(x = block, y = b1acc_mean, fill = factor(block)), 
           stat = "identity", alpha = 1) +
  facet_wrap(~ stimulus)+
  scale_fill_brewer(palette = "Set1")+
  geom_pointrange(aes(x = block, y = b1acc_mean,
                      ymin = b1acc_mean - b1acc_se, 
                      ymax = b1acc_mean + b1acc_se), 
                  color = "black", alpha = 1, size = 0.001) +
  scale_x_discrete(breaks = b1_acc$block, labels = paste0("B", b1_acc$block)) +
  geom_text(aes(x = block, y = b1acc_mean, label = round(b1acc_mean, 2)),
            vjust = -0.5, size = 2.5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Blocks", y = "Mean-Accuracy", title = "Blockwise-Stimulus-Accuracy") +
  theme_classic(base_size = 8) +
  theme(legend.position = "none")
ggsave("blockwise-stimulus-accuracy.png", width = 6, height = 4, dpi = 1000)

# rt - start, sd * 5
b1_rt <- data %>%
  select(c(block, reactionTime, stimulus)) %>%
  group_by(block, stimulus) %>%
  summarize(
    b1rt_mean = mean(reactionTime, na.rm = TRUE),
    b1rt_sd = sd(reactionTime, na.rm = TRUE),
    n = n(),
    b1rt_se = b1rt_sd/sqrt(n),
    .groups = "drop"
  )%>%
  arrange(block, stimulus)

View(b1_rt)
# Filtering Go only for rt
# New stimulus colums
b1_rt$stimulus <- factor(b1_rt$stimulus,
                          levels = 1:4,
                          labels = c("GW", "GAL", "NAL", "NW"))
b1_rt
# Only having go expts
b1_rt_filtered <- b1_rt %>%
  filter(stimulus %in% c("GW", "GAL"))

block_vals <- sort(unique(b1_rt_filtered$block))
b1_rt_filtered
View(block_vals)
#plot
ggplot(b1_rt_filtered) +
  geom_bar(aes(x = block, y = b1rt_mean, fill = factor(block)), 
           stat = "identity", alpha = 1) +
  facet_wrap(~ stimulus)+
  scale_fill_brewer(palette = "Set1")+
  geom_pointrange(aes(x = block, y = b1rt_mean,
                      ymin = b1rt_mean - b1rt_se, 
                      ymax = b1rt_mean + b1rt_se), 
                  color = "black", alpha = 1, size = 0.001) +
  scale_x_discrete(breaks = block_vals, 
                     labels = paste0("B", block_vals)) +
  geom_text(aes(x = block, y = b1rt_mean, label = round(b1rt_mean, 2)),
            vjust = -0.5, size = 2.5) +
  coord_cartesian(ylim = c(0, 1200)) +
  labs(x = "Blocks", y = "RT-mean", title = "Blockwise-Stimuliwise-RT") +
  theme_classic(base_size = 8) +
  theme(legend.position = "none")
ggsave("blockwise-GW-GAL_RT.png", width = 6, height = 4, dpi = 1000)

##############################
## RBI and PBS - blockwise####

data_L <- data[ ,c('block','accuracy','stimulus')]
pr_main <- data_L %>% 
  group_by(block) %>%  # only group by block, since you're filtering stimulus
  summarize(
    #b1 go neutral
    # sqnum = sqrt(n()),
    bg_win = sum((stimulus == 1 | stimulus == 2) & accuracy),
    bg_tot = sum((stimulus == 1| stimulus == 2)),
    RBI = bg_win/bg_tot,
    #b1 go win
    bn_win = sum((stimulus == 3|stimulus ==4) & accuracy, na.rm = TRUE),
    bn_tot = sum((stimulus == 3| stimulus == 4), na.rm = TRUE),
    PBS = bn_win/ bn_tot,
    
    #PPI = mean of RBI and PBS
    PPI = (RBI+PBS)/2,
    .groups = "drop")
View(pr_main)

# plot
## prepare data in long format.
pr_long <- pr_main %>%
  pivot_longer(
    cols = c(RBI, PBS, PPI),
    names_to = "Metric",
    values_to = "Proportion"
  )
# ordering the PBS,RBI,PPI
pr_long$Metric <- factor(pr_long$Metric, levels = c("PBS", "RBI", "PPI"))

# for scale_x_dis
block_vals <- sort(unique(data$block))

ggplot(pr_long, aes(x = factor(block), y = Proportion, fill = Metric)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  scale_x_discrete(breaks = block_vals, labels = paste0("B", block_vals)) +
  geom_text(aes(label = round(Proportion, 2)),
            position = position_dodge(0.8),
            vjust = -0.5, size = 2.5) +
  labs(x = "Block", y = "Proportion", title = "Blockwise-PBS, RBI & PPI") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic(base_size = 9)

ggsave("blockwise-PBS,RBI&PPI.png", width = 6, height = 4, dpi = 1000)

#######################################
#5. all_u -> b{1[x],2[x],3[x],4[x]}
#x = {PC , PI} -> plot %acc -> plot x2 x4 = 8

data_ci <- data[ ,c('block','stimulus','accuracy', 'pav')]
pr_ci <- data_ci %>% 
  group_by(block) %>%  # only group by block, since you're filtering stimulus
  summarize(
    #PC
    pc_win = sum((stimulus == 1 | stimulus == 4) & accuracy),
    pc_tot = sum((stimulus == 1| stimulus == 4)),
    PC = (pc_win/pc_tot),
    #PI
    pi_win = sum((stimulus == 2|stimulus ==3) & accuracy, na.rm = TRUE),
    pi_tot = sum((stimulus == 2| stimulus == 3), na.rm = TRUE),
    PI = (pi_win/pi_tot),
    .groups = "drop"
  )%>%
  arrange(block)

View(pr_ci)

# plot
## prepare data in long format.
pr_ci_long <- pr_ci %>%
  pivot_longer(
    cols = c(PC,PI),
    names_to = "congruence",
    values_to = "Proportion_ci"
  )
# ordering the PBS,RBI,PPI
pr_ci_long$congruence <- factor(pr_ci_long$congruence, levels = c("PC","PI"))

# for scale_x_dis
block_vals <- sort(unique(data$block))

ggplot(pr_ci_long, aes(x = factor(block), y = Proportion_ci, fill = congruence)) +
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.7) +
  scale_x_discrete(breaks = block_vals, 
                   labels = paste0("B", block_vals))+
  labs(x = "Blocks", y = "Proportion", title = "Blockwise-PC&PI-Accuracy") +
  coord_cartesian(ylim = c(0,1))+
  scale_fill_brewer(palette = "Set1") +
  theme_classic(base_size = 8)
ggsave("blockwise-PC&PI-Accuracy.png", width = 6, height = 4, dpi = 1000)

# x = {PC[go], PI[go]} or stimulus {1, 2}-> plot rt x2 x4 = 8


data_gci <- data[ ,c('userId','block','stimulus','accuracy')]
pr_gci <- data_gci %>% 
  group_by(block) %>%  # only group by block, since you're filtering stimulus
  summarize(
    #PC
    pcg_win = sum(stimulus == 1 & accuracy, na.rm = TRUE),
    pcg_tot = sum(stimulus == 1, na.rm = TRUE),
    PC_GO = (pcg_win/pcg_tot),
    #PCN
    pig_win = sum(stimulus == 2 & accuracy, na.rm = TRUE),
    pig_tot = sum(stimulus == 2, na.rm = TRUE),
    PI_GO = (pig_win/pig_tot),
    .groups = "drop"
  )%>%
  arrange(block)

View(pr_gci)





