library(dplyr)
library(stringr)

# --- Load and prepare game data ---
gm <- read.csv('/home/aswin/R/Projects/Error correction/game_0.csv')
gm <- gm[ , -c(14:26)]
gm$accuracy <- gm$correct == "1"
gm$pav <- ifelse(gm$stimulus %in% c("1", "3"), "PC", "PI")
gm$rt <- gm$reactionTime * 0.001

# --- Load and tag anxiety data ---
anx <- read.csv('//home/aswin/R/Projects/Error correction/main_new data/ltot_data.csv')
anx$HTA_LTA <- ifelse(anx$TA_tot <= 44, 'LTA', 'HTA')

# --- Deduplicate user tagging ---
anx_unique <- anx[!duplicated(anx$userId), c("userId", "HTA_LTA")]
gm$userId <- as.character(gm$userId)
anx$userId <- as.character(anx$userId)

##################
# Unique users in gm
users <- unique(gm$userId)
length(users)
users
for (usr in users) {
  # Get HTA_LTA value(s) from anx
  user_rows <- anx[anx$userId == usr, ]
  if (nrow(user_rows) > 0) {
    # Assign the first HTA_LTA found
    gm$HTA_LTA[gm$userId == usr] <- user_rows$HTA_LTA[1]
    # Optional print
    print(paste("User:", usr, "HTA_LTA:", paste(user_rows$HTA_LTA, collapse = ", ")))
  }
}
# --- Filter out NA-tagged users ---
gm_clean <- gm[!is.na(gm$HTA_LTA), ]
length(unique(gm_clean$userId))

# --- Sanity check: per-user tag report ---
users <- unique(gm_clean$userId)

for (usr in users) {
  user_rows <- gm_clean[gm_clean$userId == usr, ]
  print(paste("User:", usr, "HTA_LTA:", paste(unique(user_rows$HTA_LTA), collapse = ", ")))
}

write.csv(gm_clean, "gm_clean_HTA_LTA.csv", row.names = FALSE)
