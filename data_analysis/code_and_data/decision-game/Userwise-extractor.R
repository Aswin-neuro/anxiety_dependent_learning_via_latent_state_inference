data <- read.csv("/home/aswin/R/Projects/decision-game/game_data.csv")
data_tot
# extract the non NA values and remove the useless columns
data <- data[,-c(14:26)]
# Cleanup empty row
data$accuracy <- data$correct == "1" # will get TF for true or false
# pc vs pi
pav <- ifelse(data$stimulus == "1" | data$stimulus == "3", "PC", "PI")
data$pav <- pav 
# RT to seconds
rt <- data$reactionTime*0.001
data$rt <- rt

View(data)

library(ggplot2)
library(dplyr)

# Step 1: Compute accuracy per user and assign continuous user numbers
acc_per_user <- data %>%
  group_by(userId) %>% 
  summarise(acc = mean(accuracy), .groups = "drop") %>% 
  arrange(acc) %>%                                # Ensure consistent ordering
  mutate(user_num = paste0(row_number()))   # Assign User 1, 2, 3...

View(acc_per_user)


# Step 2: Plot
ggplot(acc_per_user, aes(x = factor(user_num, levels = user_num), y = acc)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "User-wise Accuracy",
       x = "User",
       y = "Accuracy") +
  theme_minimal()

# user_data[[2]]
