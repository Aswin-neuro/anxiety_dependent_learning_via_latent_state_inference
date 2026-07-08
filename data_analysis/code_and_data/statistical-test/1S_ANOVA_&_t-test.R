data <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')
# View(data)
library(car)
library(dplyr)

## change filter for LTA and HTA
data_str <- data %>% 
  select(userId,block,rt,accuracy,pav,HL) %>% 
  filter(HL == 'LTA')
data_str 

## ANOVA ##
anova_model <- aov(accuracy ~block, data = df)

anova_model

summary(anova_model)

## t-test ##

t_test <- t.test(accuracy ~ block, data = df, var.equal = TRUE)
