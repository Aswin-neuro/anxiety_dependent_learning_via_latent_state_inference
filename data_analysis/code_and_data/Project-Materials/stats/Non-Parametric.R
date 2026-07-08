df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')
# View(df)
library(tidyverse)
library(ggpubr)
library(rstatix)

## krusker wallis
set.seed(1234)
df_acc <- df %>%
  select(userId, block, accuracy, rt, pav, HL) %>%    
  filter(HL == 'HTA') %>%                             
  group_by(block, userId) %>%                     
  summarize(
    acc = mean(accuracy),
    .groups = "drop"         
  )
#Blockwise mean acc of each usr
df_acc %>% 
  group_by(block) %>% 
  get_summary_stats(acc, type='common')
ggboxplot(df_acc, x = "block", y = "acc")+
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.7, color = "darkblue")
#________________________________________________
df$block
# HLTA

#kruskal-Wallis test
df_acc$acc <- as.numeric(df_acc$acc)
kruskal.test(acc ~ block, data = df_acc)
## To find which groups are giving issues
pw <- pairwise.wilcox.test(df_acc$acc, df_acc$block, p.adjust.method = "BH")
pw

