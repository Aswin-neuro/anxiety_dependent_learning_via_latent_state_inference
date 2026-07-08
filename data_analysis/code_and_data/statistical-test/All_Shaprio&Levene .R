df <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
head(df)
# View(df)
library(car)
library(dplyr)
library(knitr)
# cleanup
## cleaning up NA
df <- df %>%
  filter(
    
    !is.na(accuracy),
    !is.na(stimulus),
    !is.na(HL),
    !is.na(pav),
    !is.na(block)
  )
# converting to factors
df$HL <- factor(df$HL)
df$stimulus <- factor(df$stimulus)
df$pav <- factor(df$pav)
df$block <- factor(df$block)
df$accuracy <- as.numeric(df$accuracy)
df$rt <- as.numeric(df$rt)
# keep +ve rt before test
data <- df %>% filter(rt > 0)


## change filter for LTA and HTA
df <- df %>% 
  select(userId, block, rt, accuracy, pav, HL, stimulus) %>% 
  filter(HL == 'HTA')
df


##############################
#####     SHAPIRO       ######
##############################

### blockwise test
S_block <- df %>%
  group_by(block, userId) %>%
  summarize(
    acc = mean(accuracy),
    rt  = mean(rt),
    .groups = "drop"
  ) %>%
  group_by(block) %>%
  summarize(
    acc_p = shapiro.test(acc)$p.value,
    acc_normal = ifelse(acc_p > 0.05, 'Yes', 'No'),
    rt_p = shapiro.test(rt)$p.value,
    rt_normal = ifelse(rt_p > 0.05, 'Yes', 'No'),
    .groups = 'drop'
  )
S_block %>%
  kable(format = "markdown", digits = 4,
        col.names = c("Block", "Accuracy p", "Accuracy Normal", "RT p", "RT Normal"))


### Pav-wise test
S_pav <- df %>%
  group_by(pav, userId) %>%
  summarize(
    acc = mean(accuracy),
    rt  = mean(rt),
    .groups = "drop"
  ) %>%
  group_by(pav) %>%
  summarize(
    acc_p = shapiro.test(acc)$p.value,
    acc_normal = ifelse(acc_p > 0.05, 'Yes', 'No'),
    rt_p = shapiro.test(rt)$p.value,
    rt_normal = ifelse(rt_p > 0.05, 'Yes', 'No'),
    .groups = 'drop'
  )

tibble(S_pav)
S_pav%>%
  kable(format = "markdown", digits = 4,
        col.names = c("Block", "Accuracy p", "Accuracy Normal", "RT p", "RT Normal"))

### stimulus-wise
S_stimulus <- df %>%
  group_by(stimulus, userId) %>%
  summarize(
    acc = mean(accuracy),
    rt  = mean(rt),
    .groups = "drop"
  ) %>%
  group_by(stimulus) %>%
  summarize(
    acc_p = shapiro.test(acc)$p.value,
    acc_normal = ifelse(acc_p > 0.05, 'Yes', 'No'),
    rt_p = shapiro.test(rt)$p.value,
    rt_normal = ifelse(rt_p > 0.05, 'Yes', 'No'),
    .groups = 'drop'
  )
S_stimulus %>%
  kable(format = "markdown", digits = 4,
        col.names = c("Block", "Accuracy p", "Accuracy Normal", "RT p", "RT Normal"))



#####################################################################
#### LEVINE'S TEST - HOV b/w conditions - blocks, pav, stimulus #####
#####################################################################
library(car)
library(dplyr)

### BLOCK-wise
L_block <- df %>%
  group_by(block, userId) %>%
  summarize(
    acc = mean(accuracy),
    rt = mean(rt),
    .groups = "drop"
  )

leveneTest(acc ~ block, data = L_block)
leveneTest(rt ~ block, data = L_block)


### PAV-wise
L_pav <- df %>%
  group_by(pav, userId) %>%
  summarize(
    acc = mean(accuracy),
    rt = mean(rt),
    .groups = "drop"
  )

leveneTest(acc ~ pav, data = L_pav)
leveneTest(rt ~ pav, data = L_pav)


### STIMULUS-wise
L_stim <- df %>%
  group_by(stimulus, userId) %>%
  summarize(
    acc = mean(accuracy),
    rt = mean(rt),
    .groups = "drop"
  )

leveneTest(acc ~ stimulus, data = L_stim)
leveneTest(rt ~ stimulus, data = L_stim)

#Extract p-values
p_vals <- c(
  leveneTest(acc ~ block, data = L_block)$`Pr(>F)`[1],
  leveneTest(rt ~ block, data = L_block)$`Pr(>F)`[1],
  leveneTest(acc ~ pav, data = L_pav)$`Pr(>F)`[1],
  leveneTest(rt ~ pav, data = L_pav)$`Pr(>F)`[1],
  leveneTest(acc ~ stimulus, data = L_stim)$`Pr(>F)`[1],
  leveneTest(rt ~ stimulus, data = L_stim)$`Pr(>F)`[1]
)
levene_summary <- tibble(
  Factor     = rep(c("Block", "Pavlovian", "Stimulus"), each = 2),
  Variable   = rep(c("Accuracy", "RT"), times = 3),
  P_Value    = round(p_vals, 5),
  Equal_Var  = ifelse(p_vals > 0.05, "Yes", "No")
)

print(levene_summary)
kable(levene_summary, format = "markdown",
      col.names = c("Factor", "Variable", "Levene's p", "Equal Variance"))

