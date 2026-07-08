# Default anciety score file
Prev <- read.csv('/home/aswin/R/Projects/decision-game/anxiety-scores.csv')
# Default game data 
data <- read.csv('/home/aswin/R/Projects/decision-game/game_data.csv') 
############

library(dplyr)
game <- read.csv('/home/aswin/R/Projects/LMEs/game-data-vs - combined.csv')

ref1 <- read.csv('/home/aswin/R/Projects/LMEs/Class_Project_STAI_G1.csv')
ref2 <- read.csv('/home/aswin/R/Projects/LMEs/Class_Project_STAI.csv')
View(game)
View(ref1)
View(ref2)

length(unique(ref2$Q3))
# ref 2 has only 49 unique userid so ref2 is not the anxiety scores
n2 <- game %>% filter(userId == 220333)

n2# Comparing between ref1 and game
length(unique(ref1$Q3))
length(unique(game$userId))

n1 <- unique(game$userId) == '220333'
View(n1)
print(n1)
game

# Returns all elements in x that are not in y
setdiff(game$userId, ref1$Q3) # x,y
setdiff(ref1$Q3, game$userId)


## checking if the data was fine in the first place
anxiety <- data$Q3
game <- data_ref$userId

length(unique(anxiety))
length(unique(game))

anxiety_only <- setdiff(anxiety, game) #IDs in anxiety data but not in game data
game_only <- setdiff(game, anxiety) #IDs in game data but not in anxiety data

cat('users not in game data are:', anxiety_only)
cat('users not in anxiety data are:', game_only)

dup_values <- anxiety[duplicated(anxiety)]  # First occurrences
unique_dups <- unique(dup_values)    
length(dup_values)
unique_dups
