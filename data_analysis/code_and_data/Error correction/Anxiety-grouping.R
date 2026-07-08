data <- read.csv('/home/aswin/R/Projects/decision-game/game_data.csv')
data <- data[,-c(14:26)]
data$accuracy <- data$correct == "1" # will get TF for true or false
pav <- ifelse(data$stimulus == "1" | data$stimulus == "3", "PC", "PI")
data$pav <- pav 
rt <- data$reactionTime*0.001
data$rt <- rt
d2 <- read.csv('/home/aswin/R/Projects/decision-game/anxiety_scores_tot.csv')
View(d2)
mean(d2$TA_tot)
median(d2$TA_tot)
d2$HTA_LTA <- ifelse(d2$TA_tot <= 44, 'LTA',
                     ifelse(d2$TA_tot >= 45, 'HTA',
                            'none'))
data$userId
d2$userId
#Testing
sample <- table(d2$HTA_LTA)
sample
barplot(sample,        
        xlab ='Names',
        ylab = 'Count'
        )

# Mapping to the new dataframe
data_merged <- merge(data, d2[, c("userId", "HTA_LTA")], by = "userId", all.x = TRUE)

View(data_merged)

# Filtering per user
users = unique(data_merged$userId)
for (usr in users) {
  # Get the rows corresponding to this user
  user_rows <- d2[d2$userId == usr, ]
  # Print each relevant HTA_LTA value
  print(paste("User:", usr, "HTA_LTA:", paste(user_rows$HTA_LTA, collapse = ", ")))
}


write.csv(data_merged, "HTA-game-data.csv", row.names = FALSE)
