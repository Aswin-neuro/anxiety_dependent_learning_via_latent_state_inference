# Initial file process
data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
library(dplyr)
library(ggplot2)
library(tidyr)
View(data)


## RBI and PBS - blockwise#### -> change all LTA and HTA 
data <- data[ ,c('userId','block','accuracy','stimulus','knocked','HL')]
data_user <- data %>%
  group_by(block, userId) %>%
  summarize(
    RBI_x = sum(knocked == 1 & (stimulus == 1 | stimulus == 3) & HL == 'HTA', na.rm = TRUE),
    RBI_y = sum(knocked == 1 & HL == 'HTA', na.rm = TRUE),
    RBI = ifelse(RBI_y == 0, NA, RBI_x / RBI_y),
    
    PBS_x = sum(knocked == 0 & (stimulus == 2 | stimulus == 4)& HL == 'HTA', na.rm = TRUE),
    PBS_y = sum((stimulus == 2 | stimulus == 4) & HL == 'HTA', na.rm = TRUE),
    PBS = ifelse(PBS_y == 0, NA, PBS_x / PBS_y),
    
    PPI = ifelse(is.na(RBI) | is.na(PBS), NA, (RBI + PBS)/2),
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
    
    PPI_mean = mean(PPI, na.rm = TRUE) ,
    PPI_sd = sd(PPI, na.rm = TRUE),
    PPI_se = PPI_sd/sqrt(sum(!is.na(PPI))),
    .groups = 'drop'
  )

View(data_rp)

## PLOT ##
# ordering RBI,PBS,PPI
rp_long <- data_rp %>%
  select(block,
         RBI_mean, PBS_mean,
         RBI_se, PBS_se) %>%
  pivot_longer(
    cols = -block,
    names_to = c("Metric", ".value"),
    names_pattern = "(.+)_(mean|se)"
  ) %>%
  mutate(Metric = factor(Metric, levels = c("PBS", "RBI")))


ggplot(rp_long, aes(x = factor(block), y = mean, fill = Metric)) +
  geom_col(position = position_dodge(0.9), width = 0.8, alpha = 1) +
  geom_pointrange(
    aes(
      ymin = mean-se,
      ymax = mean+se,
      group = Metric,
      color = Metric
    ),
    position = position_dodge(0.9),
    size = 0.01
  )+
  geom_text(aes(label = round(mean,2)),
             position = position_dodge(width = 1),
             vjust = -0.3, size = 3)+
  scale_x_discrete(
    breaks = sort(unique(data$block)),
    labels = paste0("B", sort(unique(data$block)))
  ) +
  labs(x = "Block", y = "Proportion", title = "HTA-Blockwise-PBS,RBI&PPI") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_manual(values = rep("black", length(unique(rp_long$Metric)))) +
  theme_classic(base_size = 9)

ggsave("HTA-blockwise-PBS& RBI.png", width = 6, height = 4, dpi = 1000)
#############################################################################

# Filter only PPI from data_rp
ppi_data <- data_rp %>%
  select(block, PPI_mean, PPI_se) %>%
  mutate(Metric = "PPI") %>%
  rename(mean = PPI_mean, se = PPI_se)

# Plot PPI
ggplot(ppi_data, aes(x = factor(block), y = mean, fill = Metric)) +
  geom_col(width = 0.8, alpha = 0.7) +
  geom_pointrange(
    aes(
      ymin = mean - se,
      ymax = mean + se
    ),
    color = "black",
    size = 0.01
  ) +
  geom_text(aes(label = round(mean, 2)),
            vjust = -0.3, size = 3) +
  scale_x_discrete(
    breaks = sort(unique(data$block)),
    labels = paste0("B", sort(unique(data$block)))
  ) +
  labs(x = "Block", y = "Proportion", title = "LTA-Blockwise-PPI") +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic(base_size = 9) +
  theme(legend.position = "none")

ggsave("1_LTA-blockwise-PPI.png", width = 6, height = 4, dpi = 1000)

#############################################################################
## After getting upto the plot part on above data
library(ggplot2)
library(dplyr)

# Filter to RBI only
rbi_data <- data_rp %>%
  select(block, RBI_mean, RBI_se)

# Plot
ggplot(rbi_data, aes(x = factor(block), y = RBI_mean)) +
  geom_col(fill = "steelblue", alpha = 0.7, width = 0.7) +
  geom_pointrange(aes(ymin = RBI_mean - RBI_se, ymax = RBI_mean + RBI_se),
                  color = "black", size = 0.2) +
  geom_text(aes(label = round(RBI_mean, 2)), vjust = -0.5, size = 2.8) +
  scale_x_discrete(labels = paste0("B", rbi_data$block)) +
  labs(x = "Block", y = "RBI (Proportion)", title = "Blockwise-RBI") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 9)
ggsave("blockwise-RBI.png", width = 6, height = 4, dpi = 1000)

# PBS plotting
pbs_data <- data_rp %>% 
  select(block, PBS_mean, PBS_se)

ggplot(pbs_data, aes(x = factor(block), y = PBS_mean)) +
  geom_col(fill = "red", alpha = 0.7, width = 0.7) +
  geom_pointrange(aes(ymin = PBS_mean - PBS_se, ymax = PBS_mean + PBS_se),
                  color = "black", size = 0.2) +
  geom_text(aes(label = round(PBS_mean, 2)), vjust = -0.5, size = 2.8) +
  scale_x_discrete(labels = paste0("B", pbs_data$block)) +
  labs(x = "Block", y = "PBS (Proportion)", title = "Blockwise-PBS") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 9)
ggsave("blockwise-PBS.png", width = 6, height = 4, dpi = 1000)

# PPI plotting
ppi_data <- data_rp %>% 
  select(block, PPI_mean, PPI_se)

ggplot(ppi_data, aes(x = factor(block), y = PPI_mean)) +
  geom_col(fill = "darkgreen", alpha = 0.7, width = 0.7) +
  geom_pointrange(aes(ymin = PPI_mean - PPI_se, ymax = PPI_mean + PPI_se),
                  color = "black", size = 0.2) +
  geom_text(aes(label = round(PPI_mean, 2)), vjust = -0.5, size = 2.8) +
  scale_x_discrete(labels = paste0("B", pbs_data$block)) +
  labs(x = "Block", y = "PPI (Proportion)", title = "Blockwise-PPI") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic(base_size = 9)
ggsave("blockwise-PPI.png", width = 6, height = 4, dpi = 1000)
