library(gtools)
data <- read.csv("/home/aswin/R/Projects/decision-game/HTA-game-data.csv")
colnames(data)[colnames(data) == 'HTA_LTA'] <- 'HL'
# Split block 3 into block3v1 and block3v2
data$block <- as.character(data$block)
data$block[data$block == "3" & data$week == 1] <- "3v1"
data$block[data$block == "3" & data$week == 2] <- "3v2"
data$block <- factor(data$block, levels = mixedsort(unique(data$block)))


write.csv(data, "final_game-data.csv", row.names = FALSE)
