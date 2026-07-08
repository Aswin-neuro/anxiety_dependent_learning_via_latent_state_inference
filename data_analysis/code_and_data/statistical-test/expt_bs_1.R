library(dplyr)
library(boot)

#--- Load and clean data ---#
df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')

df <- df %>%
  filter(
    !is.na(accuracy),
    !is.na(stimulus),
    !is.na(HL),
    !is.na(pav),
    !is.na(block),
    !is.na(rt),
    rt > 0
  ) %>%
  mutate(
    HL = factor(HL),
    stimulus = factor(stimulus),
    pav = factor(pav),
    block = factor(block),
    accuracy = as.numeric(accuracy),
    rt = as.numeric(rt)
  )

#--- Mean accuracy per user per block ---#
df_summary <- df %>%
  filter(HL %in% c("HTA", "LTA"), block == '1') %>%
  group_by(userId, block) %>%
  summarise(mean_acc = mean(accuracy), .groups = 'drop')

df_summary$mean_acc
#--- Bootstrap function ---#
mean_diff_stat <- function(data, indices) {
  d <- data[indices, ]
  mean(d$mean_acc[d$HL == "HTA"]) - mean(d$mean_acc[d$HL == "LTA"])
}

#--- Run bootstrap ---#
set.seed(123)
boot_results <- boot(data = df_summary, statistic = mean_diff_stat, R = 100)

#--- Confidence interval (use perc to avoid bca error) ---#
CI <- boot.ci(boot_results, type = "perc")$perc[4:5]

#--- p-value ---#
boot_vals <- boot_results$t
obs_diff <- boot_results$t0
p_value <- mean(abs(boot_vals) >= abs(obs_diff))

#--- Output ---#
cat("Mean Difference (HTA - LTA):", obs_diff, "\n")
cat("95% CI:", CI, "\n")
cat("p-value:", p_value, "\n")

anyNA(boot_results$t)

x <- df %>%
  group_by(userId) %>%
  summarise(n_HL = n_distinct(HL)) %>%
  filter(n_HL > 1)
x
