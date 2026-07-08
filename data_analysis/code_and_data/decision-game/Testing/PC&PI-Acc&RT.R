# Initial file process
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
View(data)

# all_u -> b{1[x],2[x],3[x],4[x]}
#x = {PC , PI} -> plot %acc -> plot x2 x4 = 8

data_ci <- data[ ,c('block','stimulus','accuracy', 'pav')]

View(data_ci)
pr_ci <- data_ci %>% 
  group_by(block,pav) %>%  # only group by block, since you're filtering stimulus
  summarize(
    #PC - dev
    acc_mean = mean(accuracy, na.rm = TRUE),
    acc_sds = sd(accuracy, na.rm = TRUE),
    .groups = 'drop'
  )
pr_ci

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





