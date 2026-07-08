df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')
head(df)
colnames(df)

df <- df %>% filter(HL == "HTA")
data <-  df[, c("userId", "stimulus", "knocked", "scoreChange")]
df_hbayes <- 
print(data)
unique(data$scoreChange)

data$outcome <- ifelse(data$scoreChange == 50, 1,
                               ifelse(data$scoreChange == -50, -1, 0))

colnames(data) <- c("subjID", "cue", "keyPressed", "scoreChange", "outcome")
df_hbayes <- data[, c("subjID", "cue", "keyPressed", "outcome")]

write.table(df_hbayes, file = "gng4_ready_data.txt", sep = "\t", row.names = FALSE, quote = FALSE)
data
length(unique(df_hbayes$subjID))
