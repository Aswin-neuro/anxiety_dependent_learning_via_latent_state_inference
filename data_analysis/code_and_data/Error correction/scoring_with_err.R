# Load and select
data <- read.csv('/home/aswin/R/Projects/decision-game/anxiety-scores.csv')
data_ref <- read.csv('/home/aswin/R/Projects/decision-game/game_data.csv')

unique(data_ref$userId)
library(dplyr)
library(stringr)

rev_all <- function(x) 5 - x

rev_TA_items <- sprintf("Trait_Anxiety_%02d", c(1, 3, 6, 7, 10, 13, 14, 16, 19))
rev_SA_items <- sprintf("State_Anxiety_%02d", c(1, 2, 5, 8, 10, 11, 15, 16, 19, 20))

sel_col <- names(data)[
  names(data) == "Q3" |
    startsWith(names(data), "GAD") |
    startsWith(names(data), "PHQ") |
    startsWith(names(data), "Trait_Anxiety") |
    startsWith(names(data), "State_Anxiety")
]

data <- data[, sel_col] %>%
  rename(userId = Q3) %>%
  mutate(across(matches("^GAD|^PHQ|^Trait_Anxiety|^State_Anxiety"), ~ as.numeric(.))) %>%
  mutate(across(all_of(rev_TA_items), rev_all)) %>%
  mutate(across(all_of(rev_SA_items), rev_all))

length(unique(data$userId))

# Summarize per user
data_tot <- data %>%
  filter(str_detect(userId, "^[0-9]")) %>%
  group_by(userId) %>%
  summarise(
    GAD7_tot = rowSums(across(starts_with("GAD")), na.rm = TRUE),
    PHQ9_tot = rowSums(across(starts_with("PHQ")), na.rm = TRUE),
    TA_tot = rowSums(across(starts_with("Trait_Anxiety")), na.rm = TRUE),
    SA_tot = rowSums(across(starts_with("State_Anxiety")), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(userId %in% data_ref) %>%
  mutate(tag = ifelse(TA_tot >= 75, "HTA", "LTA"))

length(unique(data_tot$userId))

table(data_tot$tag)
summary(data_tot$TA_tot)


