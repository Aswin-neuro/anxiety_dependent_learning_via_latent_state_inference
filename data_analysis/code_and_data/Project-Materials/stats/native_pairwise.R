# Combine your two groups into a dataframe:
df_two_groups <- df_avo %>%
  filter(Group %in% c("Terrain 3_No conflict", "Terrain 4_No conflict")) %>%
  select(Per_avoidance, Group)

# Define the statistic function for boot()
mean_diff_stat <- function(data, indices) {
  d <- data[indices, ]
  mean(d$Per_avoidance[d$Group == "Terrain 3_No conflict"]) -
    mean(d$Per_avoidance[d$Group == "Terrain 4_No conflict"])
}

set.seed(123)  # for reproducibility

boot_results <- boot(data = df_two_groups, statistic = mean_diff_stat, R = 10000)
boot.ci(boot_results, type = "bca")

# Extract bootstrap statistics (replicated median differences)
boot_vals <- boot_results$t  # This is a matrix; if 1D, becomes a vector

# Observed difference
obs_diff <- boot_results$t0

# Two-tailed p-value: proportion of bootstrap replicates more extreme than observed
p_value <- mean(abs(boot_vals) >= abs(obs_diff))
p_value