### Part 1 ###
data1<- read.csv("/home/aswin/R/Projects/final_game-data.csv")
data2 <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/gm_clean_HTA_LTA.csv')
data3 <- read.csv('/home/aswin/R/Projects/decision-game/anxiety-scores.csv')

game_only <- setdiff(data1$userId, data3$Q3) #  elements in x that are absent in y
anxiety_only <- setdiff(data3$Q3, data1$userId)

cat('users not in game data are:', anxiety_only)
cat('users not in anxiety data are the ones, HL values to be added:', game_only)
### Part 2 ####

#Before check
target <- as.character('240997')

df1 <- data1 %>% filter(userId == target)
df2 <- data2 %>% filter(userId == target)

df1
df2
## value HL finding

hl_value <- df2$HTA[df2$userId == target]
unique(hl_value)

# Enter manually
data1$HL[data1$userId == target] <- as.character('HTA')

#Afterstory
df11 <- data1 %>% filter(userId == target)

print(df11)


final_check1 <- data1 %>% filter(userId == '220317')
final_check2 <- data1 %>% filter(userId == '220333')
final_check3 <- data1 %>% filter(userId == '220613')
final_check4 <- data1 %>% filter(userId == '240997')
final_check1
final_check2
final_check3
final_check4

# final export
write.csv(data1, "myfinal_game-data.csv", row.names = FALSE)

result <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
res_len <- result %>% filter(!is.na(HL))
length(unique(res_len$userId))
# finally got 73 users clean!!