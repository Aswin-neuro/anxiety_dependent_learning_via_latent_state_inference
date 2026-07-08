data <- read.csv('/home/aswin/R/Projects/decision-game/anxiety-scores.csv')
data_ref <- read.csv('/home/aswin/R/Projects/decision-game/game_data.csv')
data <- read.csv('/home/aswin/R/Projects/decision-game/latest_anxiety data.csv')

length(unique(latest$Q3))

data_ref <-  data_ref[ ,'userId']
View(data)
library(stringr)
library(dplyr)
library(ggplot2)

# rev fns
rev_all <- function(x) {return(5 - x)}

rev_TA_items <- sprintf("Trait_Anxiety_%02d", c(1, 3, 6, 7, 10, 13, 14, 16, 19))
rev_SA_items <- sprintf("State_Anxiety_%02d", c(1, 2, 5, 8, 10, 11, 15, 16, 19, 20))

sel_col <- colnames(data)[
  colnames(data) == "Q3" |
    startsWith(colnames(data), "GAD") |
    startsWith(colnames(data), "PHQ") |
    startsWith(colnames(data), "Trait_Anxiety")|
    startsWith(colnames(data), "State_Anxiety")
]

rev_SA_items
# Data structuring
data <- data[ , sel_col]%>%
  rename(userId = 'Q3') %>% 
  mutate(across(matches("^GAD|^PHQ|^Trait_Anxiety|^State_Anxiety"), ~as.numeric(.))) %>%
  mutate(across(all_of(rev_TA_items), rev_all)) %>% 
  mutate(across(all_of(rev_SA_items), rev_all))

data
data_tot <- data %>%  
  group_by(userId) %>%
  summarize(
    GAD7_tot = rowSums(across(starts_with('GAD')), na.rm = TRUE),
    PHQ9_tot = rowSums(across(starts_with('PHQ')), na.rm = TRUE),
    TA_tot = rowSums(across(starts_with('Trait_Anxiety')), na.rm = TRUE),
    # TA_tot = select(matches("^Trait_Anxiety_(01|03|06|07|10|13|14|16|19)$"), rev_TA),
    SA_tot = rowSums(across(starts_with("State_Anxiety")), na.rm = TRUE),
    # SA_tot = rowSums(matched("^State_Anxiety_(01|02|05|08|11|15|16|19|20)$"), rev_SA)
    ) %>%
  ungroup()


# Removing the user ids which are not in data_ref
data_tot <- data_tot %>%
  filter(str_detect(userId, "^[0-9]")) %>% 
  filter(userId %in% data_ref)


View(data_tot)  
data[, startsWith(colnames(data), "Trait_Anxiety")]

write.csv(data_tot,"ltot_data.csv", row.names =FALSE)


ggplot(data_tot) +
  geom_bar(aes(x = userId, y = TA_tot),
           stat = "identity", alpha = 0.5, fill = 'blue')+
  scale_fill_brewer(palette = "Set1")+
  # scale_x_continuous(breaks = ab_acc$block, labels = paste0("B", ab_acc$block)) +
  # coord_cartesian(ylim = c(0,1)) +
  labs(x = "User", y = "TA_scores", title = "User vs TA") +
  theme_classic(base_size = 9) +
  theme(legend.position = "none")


x <- data$userId
data$tag <- ifelse(is.integer(TA_tot) >= 75,data$tag == TRUE)
