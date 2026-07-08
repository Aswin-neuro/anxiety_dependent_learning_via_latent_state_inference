df <- read.csv('/home/aswin/rl_projects/rl_algos/gng_data/final_game-data.csv')
# View(df)
library(tidyverse)
library(ggpubr)
library(rstatix)
colnames()
## Data prep
set.seed(1234)
df <- df %>%
  select(userId, block, accuracy, rt, pav, HL) %>%    
  filter(HL == 'LTA') %>%                             
  group_by(block, userId) %>%                     
  summarize(
    acc = mean(accuracy),
    rt = mean(rt),
    .groups = "drop"         
  )
df
#Blockwise mean acc usr-wise
df_acc <- df%>% 
  group_by(block) %>% 
  get_summary_stats(acc, type='common')
ggboxplot(df, x = "block", y = "acc")+
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.7, color = "darkblue")

#Blockwise mean rt usr-wise
View(df)
df_rt <- df %>% 
  group_by(block) %>% 
  get_summary_stats(rt, type='common')
ggboxplot(df_rt, x = "block", y = "rt")+
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.7, color = "darkblue")

ggboxplot(df, x = "block", y = "rt", color = "block", palette = "jco") +
  geom_jitter(mapping = NULL, 
              data = df, 
              width = 0.15, size = 1.5, alpha = 0.7,
              color = "darkblue",
              inherit.aes = FALSE,
              x = df$block, y = df$rt)
