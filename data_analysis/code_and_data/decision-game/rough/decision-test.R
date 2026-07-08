#stimulus type given by no. -> type of trial -> PC or PI  -> Go and no go in each ->  in each combination | find rt and % acc
#file.choose()
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
#print(unique(data$userId))

# Make a fn for inputting the static set and then use them in the function
# for checking the stable set for each of the users
# user_set <- function(block)
# for (i in seq_along(users_list)) {
#   (users_list[[i]])
# }
# Make another func for getting static block of
# HTA and LTA

### INDEX ###
# r - right
# t - tot

# Block - % acc and rt for each of this

## BLOCK 1 ##
b1_ar <- subset(data, block == "1" & accuracy)
print(b1_ar)
b1_at <- subset(data, block == "1")
b1_a <- nrow(b1_ar)/nrow(b1_at)
b1_rt <- subset(data, block == "1")
#b1c_
b1c_ar <- subset(data, block == "1" & pav == "PC" & accuracy )
b1c_at <- subset(data, block == "1" & pav == "PI")
b1c_a <- nrow(b1c_ar)/nrow(b1c_at)
b1c_rt <- subset(data, block == "1" & pav == "PC")
#b1i_
b1i_ar <- subset(data, block == "1" & pav == "PI" & accuracy)
b1i_at <- subset(data, block == "1" & pav == "PI")
b1i_a <- nrow(b1i_ar)/nrow(b1i_at)
b1i_rt <- subset(data, block == "1" & pav == "PI")

# stimulus #
# s1 # -> have rt
b1s1_ar <- subset(data, block == "1" & stimulus == "1" & accuracy)
b1s1_at <- subset(data, block == "1" & stimulus == "1")
b1s1_a <- nrow(b1s1_ar)/nrow(b1s1_at)
b1s1_rt <- subset(data, block == "1" & stimulus == "1")
# s2 # -> have rt
b1s2_ar <- subset(data, block == "1" & stimulus == "2" & accuracy)
b1s2_at <- subset(data, block == "1" & stimulus == "2")
b1s2_a <- nrow(b1s2_ar)/nrow(b1s2_at)
b1s2_rt <- subset(data, block == "1" & stimulus == "2")
# s3 #
b1s3_ar <- subset(data, block == "1" & stimulus == "3" & accuracy)
b1s3_at <- subset(data, block == "1" & stimulus == "3")
b1s3_a <- nrow(b1s3_ar)/nrow(b1s3_at)
# s4 #
b1s4_ar <- subset(data, block == "1" & stimulus == "4" & accuracy)
b1s4_at <- subset(data, block == "1" & stimulus == "4")
b1s4_a <- nrow(b1s4_ar)/nrow(b1s4_at)

## BLOCK 2 ##
b2_ar <- subset(data, block == "2" & accuracy)
b2_at <- subset(data, block == "2")
b2_a <- nrow(b2_ar)/nrow(b2_at)
b2_rt <- subset(data, block == "2")
#b1c_
b2c_ar <- subset(data, block == "2" & pav == "PC" & accuracy )
b2c_at <- subset(data, block == "2" & pav == "PI")
b2c_a <- nrow(b2c_ar)/nrow(b2c_at)
b2c_rt <- subset(data, block == "2" & pav == "PC")
#b1i_
b2i_ar <- subset(data, block == "2" & pav == "PI" & accuracy)
b2i_at <- subset(data, block == "2" & pav == "PI")
b2i_a <- nrow(b2i_ar)/nrow(b2i_at)
b2i_rt <- subset(data, block == "2" & pav == "PI")
# s1 # -> have rt
b2s1_ar <- subset(data, block == "2" & stimulus == "1" & accuracy)
b2s1_at <- subset(data, block == "2" & stimulus == "1")
b2s1_a <- nrow(b2s1_ar)/nrow(b2s1_at)
b2s1_rt <- subset(data, block == "2" & stimulus == "1")
# s2 # -> have rt
b2s2_ar <- subset(data, block == "2" & stimulus == "2" & accuracy)
b2s2_at <- subset(data, block == "2" & stimulus == "2")
b2s2_a <- nrow(b2s2_ar)/nrow(b2s2_at)
b2s2_rt <- subset(data, block == "2" & stimulus == "2")
# s3 #
b2s3_ar <- subset(data, block == "2" & stimulus == "3" & accuracy)
b2s3_ar <- subset(data, block == "2" & stimulus == "3")
b2s3_a <- nrow(b2s3_ar)/nrow(b2s3_at)
# s4 #
b2s4_ar <- subset(data, block == "2" & stimulus == "4" & accuracy)
b2s4_ar <- subset(data, block == "2" & stimulus == "4")
b2s4_a <- nrow(b2s4_ar)/nrow(b2s4_at)

## BLOCK 3 ##
b3_ar <- subset(data, block == "3" & accuracy)
b3_at <- subset(data, block == "3")
b3_a <- nrow(b3_ar)/nrow(b3_at)
b3_rt <- subset(data, block == "3")
#b3c_
b3c_ar <- subset(data, block == "3" & pav == "PC" & accuracy )
b3c_at <- subset(data, block == "3" & pav == "PI")
b3c_a <- nrow(b3c_ar)/nrow(b3c_at)
b3c_rt <- subset(data, block == "3" & pav == "PC")
#b3i_
b3i_ar <- subset(data, block == "3" & pav == "PI" & accuracy)
b3i_at <- subset(data, block == "3" & pav == "PI")
b3i_a <- nrow(b3i_ar)/nrow(b3i_at)
b3i_rt <- subset(data, block == "3" & pav == "PI")
# s1 # -> have rt
b3s1_ar <- subset(data, block == "3" & stimulus == "1" & accuracy)
b3s1_at <- subset(data, block == "3" & stimulus == "1")
b3s1_a <- nrow(b3s1_ar)/nrow(b3s1_at)
b3s1_rt <- subset(data, block == "3" & stimulus == "1")
# s2 # -> have rt
b3s2_ar <- subset(data, block == "3" & stimulus == "2" & accuracy)
b3s2_at <- subset(data, block == "3" & stimulus == "2")
b3s2_a <- nrow(b3s2_ar)/nrow(b3s2_at)
b3s2_rt <- subset(data, block == "3" & stimulus == "2")
# s3 #
b3s3_ar <- subset(data, block == "3" & stimulus == "3" & accuracy)
b3s3_ar <- subset(data, block == "3" & stimulus == "3")
b3s3_a <- nrow(b3s3_ar)/nrow(b3s3_at)
# s4 #
b3s4_ar <- subset(data, block == "3" & stimulus == "4" & accuracy)
b3s4_ar <- subset(data, block == "3" & stimulus == "4")
b3s4_a <- nrow(b3s4_ar)/nrow(b3s4_at)

## BLOCK 4 ##
b4_ar <- subset(data, block == "4" & accuracy)
b4_at <- subset(data, block == "4")
b4_a <- nrow(b4_ar)/nrow(b4_at)
b4_rt <- subset(data, block == "4")
#b3c_
b4c_ar <- subset(data, block == "4" & pav == "PC" & accuracy )
b4c_at <- subset(data, block == "4" & pav == "PI")
b4c_a <- nrow(b4c_ar)/nrow(b4c_at)
b4c_rt <- subset(data, block == "4" & pav == "PC")
#b3i_
b4i_ar <- subset(data, block == "4" & pav == "PI" & accuracy)
b4i_at <- subset(data, block == "4" & pav == "PI")
b4i_a <- nrow(b4i_ar)/nrow(b4i_at)
b4i_rt <- subset(data, block == "4" & pav == "PI")
# s1 # -> have rt
b4s1_ar <- subset(data, block == "4" & stimulus == "1" & accuracy)
b4s1_at <- subset(data, block == "4" & stimulus == "1")
b4s1_a <- nrow(b4s1_ar)/nrow(b4s1_at)
b4s1_rt <- subset(data, block == "4" & stimulus == "1")
# s2 # -> have rt
b4s2_ar <- subset(data, block == "4" & stimulus == "2" & accuracy)
b4s2_at <- subset(data, block == "4" & stimulus == "2")
b4s2_a <- nrow(b4s2_ar)/nrow(b4s2_at)
b4s2_rt <- subset(data, block == "4" & stimulus == "2")
# s3 #
b4s3_ar <- subset(data, block == "4" & stimulus == "3" & accuracy)
b4s3_ar <- subset(data, block == "4" & stimulus == "3")
b4s3_a <- nrow(b4s3_ar)/nrow(b4s3_at)
# s4 #
b4s4_ar <- subset(data, block == "4" & stimulus == "4" & accuracy)
b4s4_ar <- subset(data, block == "4" & stimulus == "4")
b4s4_a <- nrow(b4s4_ar)/nrow(b4s4_at)


x <- length(data$userId)
for (i in 0:x){
  ## BLOCK 1 ##
  b1_ar <- subset(data, block == "1" & accuracy & userId == i)
  print(b1_ar)
  print(paste0(b1_a, b1_rt, 'user is ', i, '\n'))
}
# cat("The following is the analysis for week 1 \n")
# cat("The percentage accuracy in block 4 is: ",block4_acc, "\n")
# cat("The percentage accuracy and mean rt for PC in block 4 is: ",block4_accpc,"and" ,block4rtc_mean ,"\n")
# cat("The percentage accuracy and mean rt for PI in block 4 is: ",block4_accpi,"and" ,block4rti_mean ,"\n")
# cat("The percentage accuracy for PC-GO in block 4 is: ",block4_accpcg, "\n")
# cat("The percentage accuracy for PC-NOGO in block 4 is: ",block4_accpcn, "\n")
# cat("The percentage accuracy for PI-GO in block 4 is: ",block4_accpig, "\n")
# cat("The percentage accuracy for PI-NOGO in block 4 is: ",block4_accpin, "\n")
# cat("The RBI for block 4 is =", RBI, "\n")
# cat("The PBS for block 4 is =", PBS, "\n")
# cat("The Pavlovian Performance index is =", PPI)