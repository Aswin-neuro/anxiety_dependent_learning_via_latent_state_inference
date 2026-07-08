# Load libraries
library(dplyr)
library(purrr)
library(tibble)

# Read data
df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')

# Define bootstrap function
get_boot_p <- function(hta, lta, R = 1000) {
  observed_diff <- mean(hta) - mean(lta)
  
  boot_diff <- replicate(R, {
    hta_sample <- sample(hta, replace = TRUE)
    lta_sample <- sample(lta, replace = TRUE)
    mean(hta_sample) - mean(lta_sample)
  })
  
  p_val <- mean(abs(boot_diff) >= abs(observed_diff))
  
  return(list(
    observed_diff = observed_diff,
    p_value = p_val,
    ci = quantile(boot_diff, c(0.025, 0.975))
  ))
}

# Apply bootstrap to each block
results <- df %>%
  group_split(block) %>%
  map_df(~{
    hta <- filter(.x, HL == "HTA")$accuracy
    lta <- filter(.x, HL == "LTA")$accuracy
    boot_out <- get_boot_p(hta, lta)
    
    tibble(
      block = unique(.x$block),
      mean_diff = boot_out$observed_diff,
      p_value = boot_out$p_value,
      CI_lower = boot_out$ci[1],
      CI_upper = boot_out$ci[2]
    )
  })

# View results
summary(results)
print(results)
