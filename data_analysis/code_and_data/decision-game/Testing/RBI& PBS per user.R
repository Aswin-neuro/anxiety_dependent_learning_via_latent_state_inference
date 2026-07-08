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

data_L <- data[ ,c('block','accuracy','stimulus')]
pr_main <- data_L %>% 
  group_by(block) %>%  # only group by block, since you're filtering stimulus
  summarize(
    #b1 go neutral
    sqnum = sqrt(n()),
    bg_win = sum((stimulus == 1 | stimulus == 2) & accuracy, na.rm = TRUE),
    bg_tot = sum((stimulus == 1| stimulus == 2), na.rm = TRUE),
    RBI = bg_win/bg_tot,
    RBI_sd = sd(RBI, na.rm = TRUE),
    RBI_se = RBI_sd / sqrt(sum(!is.na(RBI))),
    #b1 go win
    bn_win = sum((stimulus == 3|stimulus ==4) & accuracy, na.rm = TRUE),
    bn_tot = sum((stimulus == 3| stimulus == 4), na.rm = TRUE),
    PBS = bn_win/ bn_tot,
    PBS_sd = sd(PBS, na.rm = TRUE),
    PBS_se = PBS_sd / sqrt(sum(!is.na(PBS))),
    
    #PPI = mean of RBI and PBS
    PPI = (RBI+PBS)/2,
    PPI_sd = sd(PPI, na.rm = TRUE),
    PPI_se = PPI_sd/sqnum,
    
    .groups = "drop")
View(pr_main)

# plot
## prepare data in long format.
pr_long <- pr_main %>%
  select(block, RBI, RBI_se, PBS, PBS_se, PPI, PPI_se) %>%
  mutate(
    RBI_metric = "RBI",
    PBS_metric = "PBS",
    PPI_metric = "PPI"
  ) %>%
  select(block,
         RBI, RBI_se, RBI_metric,
         PBS, PBS_se, PBS_metric,
         PPI, PPI_se, PPI_metric) %>%
  rename(RBI_value = RBI, PBS_value = PBS, PPI_value = PPI) %>%
  pivot_longer(
    cols = c(RBI_value, PBS_value, PPI_value,
             RBI_se, PBS_se, PPI_se,
             RBI_metric, PBS_metric, PPI_metric),
    names_to = c("Metric", ".value"),
    names_pattern = "(RBI|PBS|PPI)_(.*)"
  )

ggplot(pr_long, aes(x = factor(block), y = value, fill = Metric)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = value - se, ymax = value + se),
                position = position_dodge(0.8), width = 0.2,
                color = "black") +
  labs(x = "Block", y = "Proportion", title = "Grouped Metrics with SE") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
