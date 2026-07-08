library(dplyr)
df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')

# cleanup
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
# Split HTA and LTA
hta_df <- data %>% filter(HL == "HTA")
lta_df <- data %>% filter(HL == "LTA")

hta_df
# function for shapiro by grouping factor - stimulus, pav, block
# dv = dependent variable, change it to acc or rt
shapiro_grpwise <- function(data, group_var, dv = "rt") {
  split_data <- split(data[[dv]], data[[group_var]])
  lapply(split_data, shapiro.test)
}
#### Shapiro test ####
# 1. By block
shapiro_hta_block <- shapiro_grpwise(hta_df, "block")

# 2. By stimulus
shapiro_hta_stimulus <- shapiro_grpwise(hta_df, "stimulus")

# 3. By pav
shapiro_hta_pav <- shapiro_grpwise(hta_df, "pav")

# ---- LTA ----
# 1. By block
shapiro_lta_block <- shapiro_grpwise(lta_df, "block")

# 2. By stimulus
shapiro_lta_stimulus <- shapiro_grpwise(lta_df, "stimulus")

# 3. By pav
shapiro_lta_pav <- shapiro_grpwise(lta_df, "pav")

#Outputs
shapiro_hta_block  
shapiro_hta_stimulus  
shapiro_hta_pav  
shapiro_lta_block  
shapiro_lta_stimulus  
shapiro_lta_pav  

make_p_table <- function(lst, factor_name, hl_group) {
  data.frame(
    Level    = names(lst),
    P_Value  = sapply(lst, function(x) x$p.value),
    Factor   = factor_name,
    HL_Group = hl_group
  )
}

# Make long-format p-value tables
p_hta_block    <- make_p_table(shapiro_hta_block,    "Block",    "HTA")
p_hta_stimulus <- make_p_table(shapiro_hta_stimulus, "Stimulus", "HTA")
p_hta_pav      <- make_p_table(shapiro_hta_pav,      "Pav",      "HTA")
p_lta_block    <- make_p_table(shapiro_lta_block,    "Block",    "LTA")
p_lta_stimulus <- make_p_table(shapiro_lta_stimulus, "Stimulus", "LTA")
p_lta_pav      <- make_p_table(shapiro_lta_pav,      "Pav",      "LTA")

# Combine all into one long-format table
all_shapiro_pvals <- bind_rows(
  p_hta_block, p_hta_stimulus, p_hta_pav,
  p_lta_block, p_lta_stimulus, p_lta_pav
)

all_shapiro_pvals
all_shapiro_pvals$P_Value <- round(all_shapiro_pvals$P_Value, 5)
library(gridExtra)

pdf("shapiro_pvals_table.pdf", width = 4, height = 8)
grid.table(all_shapiro_pvals)
dev.off()


# Distribution visualizer
rt_values <- data %>%
  filter(HL == "LTA", block == 1) %>%
  pull(rt)
hist(rt_values, breaks = 250, main = "Histogram of RTs")
shapiro.test(rt_values)
