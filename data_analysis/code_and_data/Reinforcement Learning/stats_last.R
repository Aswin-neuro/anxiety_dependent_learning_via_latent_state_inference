# Pav in each blocks

rm(list = ls())
df <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')# View(data)
# View(data)
library(car)
library(dplyr)
library(rstatix)
library(knitr)

### hta vs lta blockwise
# Block-wise Wilcoxon test: HTA vs LTA
# Run Wilcoxon test for HTA vs LTA in all blocks (skip filtering)
df$accuracy <- as.numeric(df$accuracy)

acc_htalta <- df %>%
  group_by(block) %>%
  wilcox_test(accuracy ~ HL) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>%
  select(block, p, p.adj, p.adj.signif)

# Output


# Output results
kable(acc_htalta, caption = "Accuracy: HTA vs LTA in each Block", format = "markdown")




#################
df$accuracy <- as.numeric(df$accuracy)  # Already done, re-check

# If needed, ensure only PC and PI are used
df_sub <- df %>%
  filter(pav %in% c("PC", "PI")) %>%
  mutate(accuracy = as.numeric(accuracy))  # Force numeric again

# Run Wilcoxon per block
wilcox_results <- df_sub %>%
  group_by(block) %>%
  wilcox_test(accuracy ~ pav, paired = FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

# View
wilcox_results %>% kable()

df %>%
  filter(pav %in% c("PC", "PI")) %>%
  group_by(block) %>%
  wilcox_test(accuracy ~ pav, paired = FALSE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>%
  select(block, p, p.adj, p.adj.signif) %>%
  kable()


### Pav, HL , blockwise - compare between pc and pi
library(dplyr)
library(rstatix)

# Filter PC condition only
df_pc_filtered <- df %>%
  filter(pav == "PI") %>%
  mutate(
    accuracy = as.numeric(accuracy),
    rt = as.numeric(rt),
    HL = as.factor(HL)
  )

# Check HL levels
print(levels(df_pc_filtered$HL))  # Should be only "HTA" and "LTA"

# Remove blocks missing a group
View(valid_blocks)

# Now run Wilcoxon test block-wise
acc_htalta <- df_pc_filtered %>%
  group_by(block) %>%
  wilcox_test(accuracy ~ HL) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj") %>%
  select(block, p, p.adj, p.adj.signif)

# Display
library(knitr)
kable(acc_htalta, caption = "Accuracy: HTA vs LTA in PC (by block)", format = "markdown")
