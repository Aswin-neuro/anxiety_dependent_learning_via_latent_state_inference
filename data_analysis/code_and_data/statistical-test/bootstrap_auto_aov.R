# Load libraries
library(dplyr)
library(tibble)

# Optional: reproducibility
set.seed(42)

# Bootstrapped ANOVA function
bootstrap_anova <- function(data, group_col, value_col, n_reps = 1000) {
  data <- data[c(group_col, value_col)]
  
  formula <- as.formula(paste(value_col, "~", group_col))
  model <- aov(formula, data = data)
  observed_F <- summary(model)[[1]]$"F value"[1]
  
  boot_Fs <- replicate(n_reps, {
    shuffled_data <- data
    shuffled_data[[value_col]] <- sample(shuffled_data[[value_col]])
    shuffled_model <- aov(formula, data = shuffled_data)
    summary(shuffled_model)[[1]]$"F value"[1]
  })
  
  p_value <- mean(boot_Fs >= observed_F)
  
  return(list(
    p_value = p_value,
    F_stat = observed_F
  ))
}


# Read and clean the data
df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')

df <- df %>%
  filter(
    !is.na(accuracy), !is.na(rt),
    !is.na(block), !is.na(pav), !is.na(stimulus), !is.na(HL),
    rt > 0
  ) %>%
  mutate(
    accuracy = as.numeric(accuracy),
    rt = as.numeric(rt),
    block = factor(block),
    pav = factor(pav),
    stimulus = factor(stimulus),
    HL = factor(HL)
  )

# Compute bootstrapped ANOVA p-values
res_block_acc <- bootstrap_anova(df, "block", "accuracy")
res_block_rt  <- bootstrap_anova(df, "block", "rt")

res_pav_acc <- bootstrap_anova(df, "pav", "accuracy")
res_pav_rt  <- bootstrap_anova(df, "pav", "rt")

res_stim_acc <- bootstrap_anova(df, "stimulus", "accuracy")
res_stim_rt  <- bootstrap_anova(df, "stimulus", "rt")

# Result summary
bootstrap_results <- tibble::tibble(
  Factor = c("Block", "Block", "Pavlovian", "Pavlovian", "Stimulus", "Stimulus"),
  Variable = c("Accuracy", "RT", "Accuracy", "RT", "Accuracy", "RT"),
  F_statistic = c(res_block_acc$F_stat, res_block_rt$F_stat,
                  res_pav_acc$F_stat, res_pav_rt$F_stat,
                  res_stim_acc$F_stat, res_stim_rt$F_stat),
  P_Value = c(res_block_acc$p_value, res_block_rt$p_value,
              res_pav_acc$p_value, res_pav_rt$p_value,
              res_stim_acc$p_value, res_stim_rt$p_value),
  Significance = ifelse(P_Value < 0.05, "Yes", "No")
)



# Output
print(bootstrap_results)
