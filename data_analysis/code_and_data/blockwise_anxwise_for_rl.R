## Blockwise_Anxietywise_splitted data generated
library(dplyr)
library(ggplot2)
df <- read.csv('/home/aswin/rl_projects/rl_algos/gng_data/main_data/loseless_game-data.csv')
View(df)
dim(df)
nrow(df)
ncol(df)
samples <- unique(df[,1])
length(samples)

print(df[1,])
print(df[,1])

clean_df <- df[c("userId", "trialNumber", "stimulus", "knocked", "scoreChange")]

sum(is.na(clean_df)) # no NA values

## Creating different csv files
## HTA-LTA|blockwise (inc 3v1,2)

anxiety <- unique(clean_df$HL)
block <- unique(clean_df$block)
for (anx in anxiety){
  for (blk in block){
    split_df <- clean_df %>% 
      filter(
        HL==anx,
        block==blk
      )
    write.csv(
      split_df,
      paste0(anx,"_",blk,".csv"),
    )
  }
} 
