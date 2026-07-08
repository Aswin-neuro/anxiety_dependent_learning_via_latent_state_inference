rm(list = ls())
df <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')# View(data)
library(car)
library(dplyr)
library(rstatix)
library(knitr)

df <- df %>%
  filter(
    !is.na(accuracy),
    !is.na(stimulus),
    !is.na(HL),
    !is.na(pav),
    !is.na(block)
  )

df$HL <- factor(df$HL)
df$stimulus <- factor(df$stimulus)
df$pav <- factor(df$pav)
df$block <- factor(df$block)
df$accuracy <- as.numeric(df$accuracy)
df$rt <- as.numeric(df$rt)

length(unique(df$userId)) # again 69
#### HLTA #####
df <- df %>% 
  select(userId, block, rt, accuracy, pav, HL, stimulus) %>% 
  filter(block == '1')


###########################################
#BLOCKWISE
##### acc & rt summarize userwise for krusker
ublock_avg <- df %>%
  group_by(userId, stimulus) %>%
  summarize(
    acc = mean(accuracy, na.rm = TRUE),
    rt = mean(rt, na.rm = TRUE),
    .groups = 'drop'
  )

## Krusker test
kruskal.test(acc ~ stimulus, data = ublock_avg)
kruskal.test(rt ~ stimulus, data = ublock_avg)

### Pairwise Wilcovson's t-test ###
wt_acc <- pairwise_wilcox_test(
  data = ublock_avg,
  formula = acc ~ stimulus,
  p.adjust.method = "bonferroni"
)

wt_rt <- pairwise_wilcox_test(
  data = ublock_avg,
  formula = rt ~ stimulus,
  p.adjust.method = "bonferroni"
)
### output ###
wt_acc <- wt_acc[,c('group1',"group2","p.adj","p.adj.signif")]
kable(wt_acc, format = "markdown", digits = 3)

wt_rt <- wt_rt[,c('group1',"group2","p.adj","p.adj.signif")]
kable(wt_rt, format = "markdown", digits = 3)

