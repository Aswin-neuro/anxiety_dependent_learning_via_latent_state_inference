df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')

# Load required library
library(dplyr)
library(boot)
library(purrr)
#cleanup
## cleaning up NA
df <- df %>%
  filter(
    !is.na(accuracy),
    !is.na(stimulus),
    !is.na(HL),
    !is.na(pav),
    !is.na(block)
  )
# converting to factors
df$HL <- factor(df$HL)
df$stimulus <- factor(df$stimulus)
df$pav <- factor(df$pav)
df$block <- factor(df$block)
df$accuracy <- as.numeric(df$accuracy)
df$rt <- as.numeric(df$rt)
# keep +ve rt before test
data <- df %>% filter(rt > 0)



# Prepare blockwise accuracy per subject
block_data <- df %>%
  group_by(userId, HL, block) %>%
  summarize(acc = mean(accuracy), .groups = "drop")
# Define bootstrap ANOVA function
bootstrap_anova <- function(x, g, R = 1000) {
  model <- aov(x ~ g)
  obs_summary <- summary(model)
  obs_F <- obs_summary[[1]][["F value"]][1]
  
  boot_F <- replicate(R, {
    x_perm <- sample(x)
    summary(aov(x_perm ~ g))[[1]][["F value"]][1]
  })
  
  p_val <- mean(boot_F >= obs_F)
  
  return(list(
    aov_summary = obs_summary,
    F_statistic = obs_F,
    boot_p_value = p_val
  ))
}


# Example: block vs accuracy
factor <- df$accuracy
across <- df$block

bootstrap_anova(factor, across)

# Basic bootstrap t-test
bootstrap_ttest <- function(x1, x2, R = 1000) {
  obs_diff <- mean(x1) - mean(x2)
  boot_diff <- replicate(R, {
    x1_samp <- sample(x1, replace = TRUE)
    x2_samp <- sample(x2, replace = TRUE)
    mean(x1_samp) - mean(x2_samp)
  })
  p_val <- mean(abs(boot_diff) >= abs(obs_diff))
  list(p_value = p_val, diff = obs_diff, ci = quantile(boot_diff, c(0.025, 0.975)))
}

# Example: HTA vs LTA accuracy
hta <- df$accuracy[df$HL == "HTA"]
lta <- df$accuracy[df$HL == "LTA"]
bootstrap_ttest(hta, lta)
