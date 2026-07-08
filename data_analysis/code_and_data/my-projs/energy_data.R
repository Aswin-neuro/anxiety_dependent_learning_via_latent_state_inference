library(tidyverse)
library(ggpubr)
library(rstatix)
data <- read.csv("energy-data.csv", check.names = FALSE)
colnames(data)[16] <- "12:00:00 AM"
colnames(data)[17] <- "01:00:00 AM"

long_data <- data %>%
  pivot_longer(cols = -days, names_to = "time", values_to = "energy") %>%
  mutate(
    hour_numeric = case_when(
      time == "12:00:00 AM" ~ 24,
      time == "01:00:00 AM" ~ 25,
      grepl("AM", time) ~ as.numeric(substr(time, 1, 2)),
      grepl("PM", time) ~ (as.numeric(substr(time, 1, 2)) %% 12) + 12,
      TRUE ~ NA_real_
    )
  )
summary_data <- long_data %>%
  group_by(hour_numeric) %>%
  summarise(
    avg_energy = mean(energy, na.rm = TRUE),
    sd_energy = sd(energy, na.rm = TRUE),
    .groups = "drop"
  )

#labels
time_breaks <- 9:25
time_labels <- c(
  "09:00 AM", "10:00 AM", "11:00 AM", "12:00 PM",
  "01:00 PM", "02:00 PM", "03:00 PM", "04:00 PM", "05:00 PM",
  "06:00 PM", "07:00 PM", "08:00 PM", "09:00 PM", "10:00 PM",
  "11:00 PM", "12:00 AM", "01:00 AM"
)

#plot
energy_plot <- ggplot(summary_data, aes(x = hour_numeric, y = avg_energy)) +
  geom_ribbon(aes(ymin = avg_energy - sd_energy, ymax = avg_energy + sd_energy),
              fill = "blue", alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkblue", linewidth = 1.2, span = 0.3) +
  scale_x_continuous(
    limits = c(9, 25),
    breaks = time_breaks,
    labels = time_labels,
    expand = c(0, 0)
  ) +
  labs(
    x = "Time of Day", 
    y = "Energy Consumption", 
    title = "Energy levels in a day - sem5"
  ) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("smoothed_energy_plot-1.png", plot = energy_plot, width = 10, height = 6, dpi = 300)
