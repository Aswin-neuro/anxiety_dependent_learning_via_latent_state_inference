df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')
View(data)
library(car)
library(dplyr)

#a. Normality
HTA_Bacc_Nor <- data_str %>%
  group_by(block, userId) %>%
  summarize(
    acc = mean(rt),
    .groups = "drop"
  ) %>% 
  group_by(block) %>% 
  summarize(
    p_value = shapiro.test(acc)$p.value,
    normal = ifelse(p_value > 0.05, 'Yes', 'No'),
    .groups = 'drop')

print(HTA_Bacc_Nor)
#b. Homogeneity of variance
HTA_Bacc <- data_str %>% 
  group_by(block, userId) %>% 
  summarize(acc = mean(rt),
            .groups ='drop')

L <- leveneTest(acc ~ as.factor(block), data = HTA_Bacc)
L

2.


#3. If assumptions are met -  2 way anova
aov_model <- aov(accuracy ~ HL, data = df)
summary(aov_model)


