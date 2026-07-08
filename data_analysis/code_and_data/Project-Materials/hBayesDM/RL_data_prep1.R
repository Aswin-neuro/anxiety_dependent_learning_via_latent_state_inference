rm(list = ls())
library(dplyr)
df <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
# converting
df$HL <- factor(df$HL)
df$stimulus <- factor(df$stimulus)
df$pav <- factor(df$pav)
df$block <- factor(df$block)
df$accuracy <- as.logical(df$accuracy)
df$rt <- as.numeric(df$rt)

df <- df %>%
  filter(
    !is.na(accuracy),
    !is.na(stimulus),
    !is.na(HL),
    !is.na(pav),
    !is.na(block)
  )
# CHECKING OMITTED USERS
# length(unique(df$userId))
# nohl <- df %>% filter(is.na(HL))
# unique(nohl$userId)
# length(unique(nohl$userId))
# print(nohl)

## 1. Filter subset
data <- df %>% filter(HL == "HTA" & block == '1' & stimulus == '4')

data
# 2. Recode variables for gng_m1
data_ready <- data %>%
  mutate(
    subjID     = userId,
    cue        = as.integer(stimulus),
    keyPressed = as.integer(knocked),  # assumes knocked==1 means press
    outcome    = case_when(
      scoreChange == 50  ~  1L,
      scoreChange == -50 ~ -1L,
      TRUE               ~  0L
    )
  ) %>%
  select(subjID, cue, keyPressed, outcome)

data_ready
length(unique(data$userId))

# Integrity check
invalid_outcome <- !data_ready$outcome %in% c(0, 1, -1)
invalid_key <- !data_ready$keyPressed %in% c(0, 1)
invalid_cue <- !data_ready$cue %in% c(1, 2, 3, 4)
has_na <- any(is.na(data_ready))

# Report
if (any(invalid_outcome)) cat("❌ Invalid outcome values at rows:", which(invalid_outcome), "\n")
if (any(invalid_key))     cat("❌ Invalid keyPressed values at rows:", which(invalid_key), "\n")
if (any(invalid_cue))     cat("❌ Invalid cue values at rows:", which(invalid_cue), "\n")
if (has_na)               cat("❌ Missing (NA) values present in data\n")
if (!any(invalid_outcome | invalid_key | invalid_cue) & !has_na) {
  cat("✅ Data passed integrity check\n")
}

# 3. Export
write.table(data_ready,
            file = "s4_hta_gngm4.txt",
            sep = "\t", row.names = FALSE, quote = FALSE)

data_ready

