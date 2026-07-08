data <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')

# Blockwise accuracy without HL or pav differentiation
block_rt <- data %>% 
  group_by(userId, block, stimulus) %>% 
  summarize(
    rt = mean(rt, na.rm = TRUE),
    .groups = 'drop'
  )
block_ci <- block_rt %>%
  mutate(stimulus = case_when(
    stimulus == "1" ~ "GW",
    stimulus == "2" ~ "NW",
    stimulus == "3" ~ "GAL",
    stimulus == "4" ~ "NAL",
    TRUE ~ as.character(stimulus)
  ))

block_vals <- sort(unique(data$block))
# Calculate mean and SE across users per block
block_ci <- block_ci %>% 
  group_by(block, stimulus) %>% 
  summarize(
    rt_mean = mean(rt, na.rm = TRUE),
    rt_sd = sd(rt, na.rm = TRUE),
    rt_se = rt_sd/sqrt(n()),
    .groups = 'drop'
  ) %>% 
  rename(proportion = rt_mean, 
         se = rt_se)

block_ci$stimulus
# Plot
ggplot(block_ci, aes(x = factor(block), y = proportion)) +
  geom_col(position = position_dodge(0.8), width = 0.7, fill = "red", alpha = 0.8) +
  geom_pointrange(aes(y = proportion,
                      ymin = proportion - se, 
                      ymax = proportion + se), 
                  position = position_dodge(0.8),
                  color = "black", alpha = 1, size = 0.01)+
  geom_text(aes(label = round(proportion, 2)),
            position = position_dodge(0.8),
            vjust = -0.5, size = 3) +
  scale_x_discrete(breaks = block_vals, labels = paste0("B", block_vals)) +
  labs(x = "Blocks", y = "Reaction Time (seconds)", title = "Blockwise Reaction-time") +
  facet_wrap(~stimulus) +
  coord_cartesian(ylim = c(0, 1.1)) +
  theme_classic(base_size = 10)

ggsave("1_blockwise-reaction-time.png", width = 6, height = 4, dpi = 1000)
