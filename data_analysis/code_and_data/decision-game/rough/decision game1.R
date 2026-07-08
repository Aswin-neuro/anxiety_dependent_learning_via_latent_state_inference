data <- read.csv("/home/aswin/R/Projects/decision-game/game_data.csv")
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

## Data Extraction for anxiety

danx <- read.csv('~/R/Projects/decision-game/anxiety-scores.csv')
danx <- danx[,-c(1:18)]
View(danx)

GP_danx <- danx %>% 
  select(c(Q3, starts_with('GA'),Q2.1,starts_with('PH'), Q4.1))

View(GP_danx)
TA_danx <-danx %>% 
  select(c(starts_with('Trait')))

View(TA_danx)

SA_danx <- danx %>% 
  select(c(starts_with('State')))

View(SA_danx)


### DATA& PLOT GEN
# library(ggplot2)
# library(dplyr)
# 
# # 1. all users -> %acc -> plot = 1
# 
# a1 <- data %>% 
#   mutate(userId = paste0('p',row_number())) %>% 
#   select(c(userId, accuracy)) %>% 
#   group_by(userId) %>% # treat each user as a group
#   summarize(acc_mean = mean(accuracy),.groups = 'drop') %>% 
#   arrange(acc_mean)
#   # 1 to N numbering
# #avg for each grp & remove grp after summarizing
# 
# a1 <- data %>%
#   mutate(users = paste0("p", match(userId, unique(userId)))) %>% 
#   select(c(users, accuracy)) %>% 
#   group_by(users) %>% 
#   summarize(acc_mean = mean(accuracy)) %>% 
#   arrange(acc_mean)
# 
# View(a1)
# ggplot(a1, aes(x = factor(userId), y = acc_mean)) +
#   geom_bar(stat = "identity", fill = "darkblue") +
#   labs(x = "User", y = "Mean Accuracy", title = "Average Accuracy per User") +
#   theme_minimal()





