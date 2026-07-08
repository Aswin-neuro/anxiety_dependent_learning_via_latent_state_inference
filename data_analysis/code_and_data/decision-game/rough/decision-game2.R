data <- read.csv("/home/aswin/R/Projects/decision-game/game_data.csv")
data <- data[,-c(14:26)]
data$accuracy <- data$correct == "1" # will get TF for true or false
pav <- ifelse(data$stimulus == "1" | data$stimulus == "3", "PC", "PI")
data$pav <- pav 
rt <- data$reactionTime*0.001
data$rt <- rt
View(data)

### DATA& PLOT GEN
library(ggplot2)
library(dplyr)
## all users accuracy ##
a1_acc <- data %>%
  mutate(users = paste0("p", match(userId, unique(userId)))) %>% 
  select(c(users, accuracy)) %>% 
  group_by(users) %>% 
  summarize(acc_mean = mean(accuracy)) %>% 
  arrange(acc_mean)

View(a1_acc)
ggplot(a1_acc, aes(x = reorder(users, acc_mean), y = acc_mean)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "users", y = "mean accuracy", title = "Average Accuracy per User") +
  theme_minimal()

## all users rt ##
a1_rt <- data %>%
  mutate(users = paste0("p", match(userId, unique(userId)))) %>% 
  select(c(users,reactionTime)) %>% 
  group_by(users) %>% 
  summarize(rt_mean = mean(reactionTime)) %>% 
  arrange(rt_mean)

View(a1_rt)
ggplot(a1_rt, aes(x = reorder(users, rt_mean), y = rt_mean)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(x = "users", y = "mean accuracy", title = "Average Accuracy per User") +
  theme_minimal()

## all_u -> b{1[x],2[x],3[x],4[x]} -> av_ac -> plot = 4
## x = {RBI,PBS} = plots x2 x4 = 8
# plot_user_metrics(data, c("accuracy", "reactionTime"))

plot_user_metric <- function(data, metric, y_label, title) {
  data_summary <- data %>%
    mutate(users = paste0("p", match(userId, unique(userId)))) %>%
    select(users, var = all_of(metric)) %>%
    group_by(users) %>%
    summarize(mean_val = mean(var, na.rm = TRUE)) %>%
    arrange(mean_val)
  
  ggplot(data_summary, aes(x = reorder(users, mean_val), y = mean_val)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    labs(x = "users", y = y_label, title = title) +
    theme_minimal()
}

plot_user_metric(
  data = data,
  metric = "reactionTime",
  y_label = "Mean rt",
  title = "Average rt per User"
)
