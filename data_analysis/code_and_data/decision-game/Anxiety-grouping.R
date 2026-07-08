library(dplyr)
library(stringr)

gm <- read.csv('/home/aswin/R/Projects/Error correction/game_0.csv')
gm <- gm[,-c(14:26)]
length(unique(gm$userId))
# 73 users
View(gm)

gm$accuracy <- gm$correct == "1" # will get TF for true or false
pav <- ifelse(gm$stimulus == "1" | gm$stimulus == "3", "PC", "PI")
gm$pav <- pav 
rt <- gm$reactionTime*0.001
gm$rt <- rt

anx <- read.csv('//home/aswin/R/Projects/Error correction/main_new data/ltot_data.csv')
View(anx)
colnames(anx)

ex <- anx %>% filter(userId == '220333')
ex

mean(anx$TA_tot)
median(anx$TA_tot)

anx$HTA_LTA <- ifelse(
  is.na(anx$TA_tot), 'none',
  ifelse(anx$TA_tot <= 44, 'LTA', 'HTA')
)
nrow(anx)
anx_unique <- anx[!duplicated(anx$userId), ]
nrow(anx_unique)
#test
ex <- anx_unique %>% filter(userId == '220333')
ex

# Mapping to the new dataframe
# data_merged <- merge(data, d2[, c("userId", "HTA_LTA")], by = "userId", all.x = TRUE)

#test
ex <- anx_unique %>% filter(userId == '220333')
ex

length(unique(data_merged$userId))



gm_clean <- gm[is.na(gm$HTA_LTA), ]
gm_clean

View(data_merged)

check_na_hl <- data_merged %>% filter(!is.na(HTA_LTA))
length(check_na_hl)

# Filtering per user
users = unique(data_merged$userId)
for (usr in users) {
  # Get the rows corresponding to this user
  user_rows <- d2[d2$userId == usr, ]
  # Print each relevant HTA_LTA value
  print(paste("User:", usr, "HTA_LTA:", paste(user_rows$HTA_LTA, collapse = ", ")))
}


write.csv(data_merged, "HL-game-data-latest.csv", row.names = FALSE)
