data <- read.csv("/home/aswin/R/Projects/decision-game/game_data.csv")
data <- data[,-c(14:26)]
data$accuracy <- data$correct == "1" # will get TF for true or false
pav <- ifelse(data$stimulus == "1" | data$stimulus == "3", "PC", "PI")
data$pav <- pav 
rt <- data$reactionTime*0.001
data$rt <- rt
View(data)

## Plotting function for userwise output
# plot_user_metric <- function(data, cols, y_label, title) {
#   data_summary <- data %>%
#     mutate(users = paste0("p", match(userId, unique(userId)))) %>%
#     select(users, var = all_of(cols)) %>%
#     group_by(users) %>%
#     summarize(mean_val = mean(var, na.rm = TRUE)) %>%
#     arrange(mean_val)
#   
#   ggplot(data_summary, aes(x = reorder(users, mean_val), y = mean_val)) +
#     geom_bar(stat = "identity", fill = "darkblue") +
#     labs(x = "users", y = y_label, title = title) +
#     theme_minimal()
# }
## usage ##
# plot_user_metric(
#   data = data,
#   cols = "reactionTime",
#   y_label = "Mean rt",
#   title = "Average rt per User"
# )

# 3. all_u -> b{1[x],2[x],3[x],4[x]} -> av_ac -> plot = 4
# x = {RBI,PBS} = plots x2 x4 = 8

# ab_acc
ab_acc <- data %>%
  # mutate(users = paste0("p", match(userId, unique(userId)))) %>%
  select(c(block, accuracy)) %>%
  group_by(block) %>%
  summarize(
    acc_mean = mean(accuracy, na.rm = TRUE),
    acc_sd = sd(accuracy, na.rm = TRUE),
    n = n(),
    acc_se = acc_sd/sqrt(n)
    )%>%
  arrange(block)

View(ab_acc)
ggplot(ab_acc) +
  geom_bar(aes(x = block, y = acc_mean, fill = factor(block)), 
           stat = "identity", alpha = 0.5) +
  scale_fill_brewer(palette = "Set1")+
  geom_pointrange(aes(x = block, y = acc_mean,
                      ymin = acc_mean - acc_se*10, 
                      ymax = acc_mean + acc_se*10), 
                  color = "orange", alpha = 1, size = 0.4) +
  scale_x_continuous(breaks = ab_acc$block, labels = paste0("B", ab_acc$block)) +
  coord_cartesian(ylim = c(0, 0.9)) +
  labs(x = "Blocks", y = "Mean-Accuracy", title = "Accuracy-Blockwise") +
  theme_minimal() +
  theme(legend.position = "none")

# ab_rt

ab_rt <- data %>%
  # mutate(users = paste0("p", match(userId, unique(userId)))) %>%
  select(c(block, reactionTime)) %>%
  group_by(block) %>%
  summarize(
    rt_mean = mean(reactionTime, na.rm = TRUE),
    rt_sd = sd(reactionTime, na.rm = TRUE),
    n = n(),
    acc_se = rt_sd/sqrt(n)
  )%>%
  arrange(block)

ggplot(ab_acc) +
  geom_pointrange(aes(x = block, y = acc_mean,
                      ymin = acc_mean - acc_se,
                      ymax = acc_mean + acc_se),
                  color = "orange", size = .2)


View(ab_acc)
ggplot(ab_acc) +
  geom_bar(aes(x = block, y = acc_mean, fill = block), 
           stat = "identity", alpha = 0.5) +
  geom_pointrange(aes(x = block, y = acc_mean,
                      ymin = acc_mean - acc_se*10, 
                      ymax = acc_mean + acc_se*10), 
                  color = "orange", alpha = 1, size = 0.4) +
  coord_cartesian(ylim = c(0, .9)) +
  labs(x = "Block", y = "Mean Accuracy", title = "Average Accuracy per Block") +
  theme_minimal() +
  theme(legend.position = "none")


abline(v = m, col = "blue", lwd = 2)
# ab_acc_s{1,2,3,4}
#Summarize accuracy by block and stimulus
ab1_acc <- data %>%
  select(block, accuracy, stimulus) %>%
  group_by(block, stimulus) %>%
  summarize(acc_mean = mean(accuracy), .groups = "drop") %>%
  arrange(stimulus, block)

#Plot separate bar using facet_wrap
ggplot(ab1_acc, aes(x = block, y = acc_mean, fill = block)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ stimulus) +
  labs(x = "Block", y = "Mean Accuracy", title = "Accuracy Across Blocks for Each Stimulus") +
  theme_minimal()
  # theme(legend.position = "none")

#4. all_u -> b{1[x],2[x],3[x],4[x]}
#x = {1,2,3,4} -> plot %acc = x4 x4 = 16

ab1_rt <- data %>%
  select(block, reactionTime, stimulus) %>%
  group_by(block, stimulus) %>%
  summarize(rt_mean = mean(reactionTime), .groups = "drop") %>%
  arrange(stimulus, block)

ggplot(ab1_rt, aes(x = block, y = rt_mean, fill = block)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ stimulus) +
  labs(x = "Block", y = "Mean RT", title = "Mean RT Across Blocks for Each Stimulus") +
  theme_minimal()

#5. all_u -> b{1[x],2[x],3[x],4[x]}
# x = {PC , PI} -> plot %acc -> plot x2 x4 = 8
# x = {PC[go], PI[go]} or stimulus {1, 2}-> plot rt x2 x4 = 8
ab1pc_rt <- data %>%
  select(block, reactionTime, pav) %>%
  group_by(block, pav) %>%
  summarize(rt_mean = mean(reactionTime), .groups = "drop") %>%
  arrange(stimulus, block)

ggplot(ab1_rt, aes(x = block, y = rt_mean, fill = block)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ stimulus) +
  labs(x = "Block", y = "Mean RT", title = "Mean RT Across Blocks for Each Stimulus") +
  theme_minimal()




