df <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')

library(ggplot2)
library(lme4)
library(tidyr)
library(dplyr)
library(emmeans)
library(knitr)

colnames(df)
# Data preprocessing - clean, convert to factors, acc to numeric
## cleaning up NA
df <- df %>%
  filter(
    !is.na(accuracy),
    !is.na(stimulus),
    !is.na(HL),
    !is.na(pav),
    !is.na(block)
  )
# converting
df$HL <- factor(df$HL)
df$stimulus <- factor(df$stimulus)
df$pav <- factor(df$pav)
df$block <- factor(df$block)
df$accuracy <- as.numeric(df$accuracy)
df$rt <- as.numeric(df$rt)
# 1360 rows removed due to the na values
#checking similarity of trial count to check if uniform
table(df$HL, df$pav)
table(df$block)
## failed b3v2 has 2480/6680 no of trials, while b3v1 has 4200/6680
length(unique(df$userId))

acc <- df %>%
  group_by(HL, pav) %>%
  summarise(mean_acc = mean(accuracy), mean_rt = mean(rt), .groups = 'drop')

#########################################
#   INTERACTION ONLY ##
#### Checking each interactions with main_model

# Interaction_model - reference
model_no_interact <- lmer(rt ~ stimulus + HL + pav + block +
                             stimulus:HL + (1 | userId),
                           data = df)

interact <- summary(model_interact)

### stimulus:HL:pav is 3 way interaction###
## add stim:HL
model_stimHL<- lmer(rt ~ stimulus + HL + pav + block +
                       stimulus:HL + (1 | userId),
                     data = df)
## add stim:pav 
model_stimPav <- lmer(rt ~ stimulus + HL + pav + block +
                         stimulus:pav + (1 | userId),
                       data = df)
## add stim:block
model_stimBlock <- lmer(rt ~ stimulus + HL + pav + block +
                           stimulus:block + (1 | userId),
                         data = df)
## add all stim interactions - block, pav, HL
model_allstim <- lmer(rt ~ stimulus + HL + pav + block +
                       stimulus:HL + stimulus:block + stimulus:pav
                       + (1|userId), data = df)

### HL with pav:block
## add HL:pav
model_hlPav <- lmer(rt ~ stimulus + HL + pav + block 
                     + HL:pav
                     + (1 | userId), data = df)

## add HL:block
model_hlBlock <- lmer(rt ~ stimulus + HL + pav + block 
                       + HL:block
                       + (1 | userId), data = df)

## add all HL interactions = HL:block, HL:pav
model_allHL <- lmer(rt ~ stimulus + HL + pav + block
                     + HL:block + HL:pav
                     + (1|userId), data = df)

### add pav:block
model_pavBlock <- lmer(rt ~ stimulus + HL + pav + block +
                          pav:block + (1 | userId),
                        data = df)


# list of infos
models <- list(
  Full_model = model_no_interact,
  stimHL = model_stimHL,
  stimPav = model_stimPav,
  stimBlock = model_stimBlock,
  allstim = model_allstim,
  HLpav = model_hlPav,
  HLblock = model_hlBlock,
  allHL = model_allHL,
  pavBlock = model_pavBlock
)
# Extract AIC/BIC values
model_stats <- data.frame(
  model = names(models),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC)
)

# Long format
model_stats_long <- model_stats %>%
  pivot_longer(cols = c(AIC, BIC), names_to = "Metric", values_to = "Value")
# AIC & BIC plot
# Option 1: No fixed limits (recommended)
plot <- ggplot(model_stats_long, aes(x = reorder(model, Value), y = Value, fill = Metric)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = 'RT inv interaction models', x = 'Model', y = 'Value') +
  theme_classic() +
  coord_cartesian(ylim = c(59000, 61000)) +
  
  scale_fill_manual(values = c('AIC' = 'blue', 'BIC' = 'red'))
plot
ggsave("inv_rt_interact_model.png", plot = plot, width = 10, height = 6, dpi = 1000)
