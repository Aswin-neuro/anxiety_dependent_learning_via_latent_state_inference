# Initial file process
data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')

library(dplyr)
library(ggplot2)
library(tidyr)
View(data)


## RBI and PBS - blockwise####

data <- data[ ,c('userId','block','accuracy','stimulus','knocked')]
data_user <- data %>%
  group_by(block, userId) %>%
  summarize(
    RBI_x = sum(knocked == 1 & (stimulus == 1 | stimulus == 3), na.rm = TRUE),
    RBI_y = sum(knocked == 1, na.rm = TRUE),
    RBI = RBI_x / RBI_y,
    
    PBS_x = sum(knocked == 0 & (stimulus == 2 | stimulus == 4), na.rm = TRUE),
    PBS_y = sum((stimulus == 2 | stimulus == 4), na.rm = TRUE),
    PBS = PBS_x / PBS_y,
    
    PPI = (RBI + PBS) / 2,
    .groups = 'drop'
  )
View(data_user)

data_rp <- data_user %>%
  group_by(block) %>%
  summarize(
    RBI_mean = mean(RBI, na.rm = TRUE),
    RBI_sd = sd(RBI, na.rm = TRUE),
    RBI_se = RBI_sd / sqrt(n()),
    
    PBS_mean = mean(PBS, na.rm = TRUE),
    PBS_sd = sd(PBS, na.rm = TRUE),
    PBS_se = PBS_sd/ sqrt(n()),
    
    PPI_mean = mean(PPI) ,
    PPI_sd = sd(PPI),
    PPI_se = PBS_sd/sqrt(n()),
    .groups = 'drop'
  )

View(data_rp)
   
## PLOT ##
# ordering RBI,PBS,PPI
rp_long <- data_rp %>%
  select(block,
         RBI_mean, PBS_mean, PPI_mean,
         RBI_se, PBS_se, PPI_se) %>%
  pivot_longer(
    cols = -block,
    names_to = c("Metric", ".value"),
    names_pattern = "(RBI|PBS|PPI)_(mean|se)"
  ) %>%
  mutate(Metric = factor(Metric, levels = c("PBS", "RBI", "PPI")))

ggplot(rp_long, aes(x = factor(block), y = mean, fill = Metric)) +
  geom_col(position = position_dodge(0.9), width = 0.8, alpha = 0.7) +
  geom_pointrange(
    aes(
      ymin = mean-se,
      ymax = mean+se,
      group = Metric,
      color = Metric
    ),
    position = position_dodge(0.9),
    size = 0.01
  ) +
  scale_x_discrete(
    breaks = sort(unique(data$block)),
    labels = paste0("B", sort(unique(data$block)))
  ) +
  labs(x = "Block", y = "Proportion", title = "Blockwise-PBS,RBI&PPI") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = rep("black", length(unique(rp_long$Metric)))) +
  theme_classic(base_size = 9)

ggsave("blockwise-PBS,RBI&PPI.png", width = 6, height = 4, dpi = 1000)
