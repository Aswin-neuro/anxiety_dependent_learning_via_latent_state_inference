library(dplyr)
library(ggplot2)
library(tidyr)

data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
data <- data[ ,c('userId','block','accuracy','stimulus','knocked','HL')]

data_user <- data %>%
  filter(block == '1') %>%  # Only block 1
  group_by(HL, userId) %>%
  summarize(
    RBI_x = sum(knocked == 1 & (stimulus == 1 | stimulus == 4), na.rm = TRUE),
    RBI_y = sum(knocked == 1, na.rm = TRUE),
    RBI = ifelse(RBI_y == 0, NA, RBI_x / RBI_y),
    
    PBS_x = sum(knocked == 0 & (stimulus == 2 | stimulus == 3), na.rm = TRUE),
    PBS_y = sum(knocked == 0, na.rm = TRUE),
    PBS = ifelse(PBS_y == 0, NA, PBS_x / PBS_y),
    
    PPI = ifelse(is.na(RBI) | is.na(PBS), NA, (RBI + PBS) / 2),
    .groups = 'drop'
  )

data_rp <- data_user %>%
  group_by(HL) %>%
  summarize(
    RBI_mean = mean(RBI, na.rm = TRUE),
    RBI_se = sd(RBI, na.rm = TRUE) / sqrt(n()),
    
    PBS_mean = mean(PBS, na.rm = TRUE),
    PBS_se = sd(PBS, na.rm = TRUE) / sqrt(n()),
    
    PPI_mean = mean(PPI, na.rm = TRUE),
    PPI_se = sd(PPI, na.rm = TRUE) / sqrt(sum(!is.na(PPI))),
    .groups = 'drop'
  )


ggplot(data_rp, aes(x = HL, y = PPI_mean)) +
  geom_col(fill = "forestgreen", alpha = 0.7, width = 0.6) +
  geom_pointrange(
    aes(ymin = PPI_mean - PPI_se, ymax = PPI_mean + PPI_se),
    position = position_dodge(width = 0.6),
    color = "black",
    size = 0.3
  ) +
  geom_text(aes(label = round(PPI_mean, 2)), vjust = -0.5, size = 3) +
  labs(x = "Trait Anxiety", y = "PPI", title = "PPI - Block 1") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 10)

ggsave("B1HL_PPI.png", width = 5, height = 4, dpi = 1000)