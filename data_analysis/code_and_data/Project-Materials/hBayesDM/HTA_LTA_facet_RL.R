# Add group labels
hb1_draws$group <- "HTA"; hb2_draws$group <- "HTA"
hb3v1_draws$group <- "HTA"; hb3v2_draws$group <- "HTA"
hb4_draws$group <- "HTA"

lb1_draws$group <- "LTA"; lb2_draws$group <- "LTA"
lb3v1_draws$group <- "LTA"; lb3v2_draws$group <- "LTA"
lb4_draws$group <- "LTA"

# Combine all
all_draws_combined <- bind_rows(
  hb1_draws, hb2_draws, hb3v1_draws, hb3v2_draws, hb4_draws,
  lb1_draws, lb2_draws, lb3v1_draws, lb3v2_draws, lb4_draws
)

# Faceted boxplot: grouped by HTA / LTA
plot_facet <- ggplot(all_draws_combined, aes(x = .variable, y = .value, fill = model)) +
  geom_boxplot(position = position_dodge(0.75), outlier.alpha = 0.2) +
  coord_flip() +
  facet_wrap(~ group) +
  labs(x = "Parameter", y = "Posterior Distribution", fill = "Model") +
  theme_classic(base_size = 13)

plot_facet
ggsave("RL3_HTA_LTA_facet.png", plot = plot_facet, width = 10, height = 6, dpi = 1000)
