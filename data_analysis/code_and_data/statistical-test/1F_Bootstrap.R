df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')
library(car)
library(dplyr)
library(boot)
## change filter for LTA and HTA
df <- df %>% 
  select(userId,block,rt,accuracy,pav,HL) %>% 
  filter(HL == 'LTA')
df$block

# Combine your two groups into a dataframe:
df_two_groups <- df %>%
  filter(block %in% c("1", "2")) %>%
  mutate(block = as.factor(block)) %>% 
  select(block, accuracy)

# Define the statistic function for boot()
mean_diff <- function(data, indices) {
  df <- data[indices, ]
  mean(df$accuracy[df$block=='3v1'], na.rm = TRUE) -
    mean(df$accuracy[df$block=='3v2'], na.rm = TRUE)
}

# Bootstrapping
set.seed(132) 
boot_results <- boot(data = df_two_groups, statistic = mean_diff, R = 100)

# conf- interval
boot.ci(boot_results, type = "bca")

# Extract bootstrap statistics (replicated median differences)
boot_vals <- boot_results$t  # This is a matrix; if 1D, becomes a vector

# Observed difference
obs_diff <- boot_results$t0

# Two-tailed p-value: proportion of bootstrap replicates more extreme than observed
p_value <- mean(abs(boot_vals) >= abs(obs_diff))

p_value
