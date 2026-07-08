data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
library(dplyr)
library(ggplot2)
library(tidyr)

data <- data[ ,c('userId','block','accuracy','stimulus','knocked')]
data_user <- data %>%
  group_by(block, userId) %>%
  summarize(
    RBI_x = sum(knocked == 1 & (stimulus == 1 | stimulus == 4), na.rm = TRUE),
    RBI_y = sum(knocked == 1, na.rm = TRUE),
    RBI = ifelse(RBI_y == 0, NA, RBI_x / RBI_y),
    
    PBS_x = sum(knocked == 0 & (stimulus == 2 | stimulus == 3), na.rm = TRUE),
    PBS_y = sum(knocked == 0, na.rm = TRUE),
    PBS = ifelse(PBS_y == 0, NA, PBS_x / PBS_y),
    
    PPI = ifelse(is.na(RBI) | is.na(PBS), NA, (RBI + PBS)/2),
    .groups = 'drop'
  )

data_rp <- data_user %>%
  group_by(block) %>%
  summarize(
    RBI_mean = mean(RBI, na.rm = TRUE),
    RBI_sd = sd(RBI, na.rm = TRUE),
    RBI_se = RBI_sd / sqrt(n()),
    
    PBS_mean = mean(PBS, na.rm = TRUE),
    PBS_sd = sd(PBS, na.rm = TRUE),
    PBS_se = PBS_sd/ sqrt(n()),
    
    PPI_mean = mean(PPI, na.rm = TRUE) ,
    PPI_sd = sd(PPI, na.rm = TRUE),
    PPI_se = PPI_sd/sqrt(sum(!is.na(PPI))),
    .groups = 'drop'
  )

# PPI
# Extract PPI only
ppi_data <- data_rp %>%
  select(block, mean = PPI_mean, se = PPI_se)

# Plot
ggplot(ppi_data, aes(x = factor(block), y = mean)) +
  geom_col(fill = "forestgreen", alpha = 0.7, width = 0.7) +
  geom_pointrange(
    aes(ymin = mean - se, ymax = mean + se),
    color = "black", size = 0.2
  ) +
  geom_text(aes(label = round(mean, 2)),
            vjust = -0.5, size = 2.8) +
  scale_x_discrete(labels = paste0("B", ppi_data$block)) +
  labs(x = "Block", y = "PPI (Proportion)", title = "Blockwise-PPI") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 9)

ggsave("latest_blockwise-PPI.png", width = 6, height = 4, dpi = 1000)

#RBI
# Extract RBI only
rbi_data <- data_rp %>%
  select(block, mean = RBI_mean, se = RBI_se)

# Plot
ggplot(rbi_data, aes(x = factor(block), y = mean)) +
  geom_col(fill = "steelblue", alpha = 0.7, width = 0.7) +
  geom_pointrange(
    aes(ymin = mean - se, ymax = mean + se),
    color = "black", size = 0.2
  ) +
  geom_text(aes(label = round(mean, 2)),
            vjust = -0.5, size = 2.8) +
  scale_x_discrete(labels = paste0("B", rbi_data$block)) +
  labs(x = "Block", y = "RBI (Proportion)", title = "Blockwise-RBI") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 9)

ggsave("latest_blockwise-RBI.png", width = 6, height = 4, dpi = 1000)

##PBS
# Extract PBS only
pbs_data <- data_rp %>%
  select(block, mean = PBS_mean, se = PBS_se)

# Plot
ggplot(pbs_data, aes(x = factor(block), y = mean)) +
  geom_col(fill = "red", alpha = 0.7, width = 0.7) +
  geom_pointrange(
    aes(ymin = mean - se, ymax = mean + se),
    color = "black", size = 0.2
  ) +
  geom_text(aes(label = round(mean, 2)),
            vjust = -0.5, size = 2.8) +
  scale_x_discrete(labels = paste0("B", pbs_data$block)) +
  labs(x = "Block", y = "PBS (Proportion)", title = "Blockwise-PBS") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 9)

ggsave("latest_blockwise-PBS.png", width = 6, height = 4, dpi = 1000)
