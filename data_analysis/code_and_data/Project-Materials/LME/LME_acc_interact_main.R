rm(list = ls())
df <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
library(ggplot2)
library(lme4)
library(tidyr)
library(dplyr)
library(emmeans)
library(knitr)

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
model_interact <- glmer(accuracy ~ stimulus + HL + pav + block +
                          stimulus:HL + stimulus:pav + stimulus:block +
                          HL:pav + HL:block + pav:block +
                          (1 | userId),
                        data = df, family = binomial)
interact <- summary(model_interact)

### stimulus:HL:pav is 3 way interaction###
## remove stim:HL
model_nostimHL<- glmer(accuracy ~ stimulus + HL + pav + block
                      + stimulus:pav + stimulus:block
                      + HL:pav + HL:block + pav:block 
                      + (1 | userId), data = df, family = binomial)
## remove stim:pav 
model_nostimPav <- glmer(accuracy ~ stimulus + HL + pav + block
                         + stimulus:HL + stimulus:block
                         + HL:pav + HL:block + pav:block
                         + (1|userId), data = df, family = binomial)
## remove stim:block
model_nostimBlock <- glmer(accuracy ~ stimulus + HL + pav + block 
                           + stimulus:HL + stimulus:pav
                           + HL:pav + HL:block + pav:block
                           + (1|userId), data = df, family = binomial)
## remove all stim interactions - block, pav, HL
model_nostim <- glmer(accuracy ~ stimulus + HL + pav + block 
                           + HL:pav + HL:block + pav:block
                           + (1|userId), data = df, family = binomial)

### HL with pav:block

## remove HL:pav
model_nohlPav <- glmer(accuracy ~ stimulus + HL + pav + block 
                        + stimulus:HL + stimulus:pav + stimulus:block
                        + HL:block + pav:block 
                        + (1 | userId), data = df, family = binomial)

## remove HL:block
model_nohlBlock <- glmer(accuracy ~ stimulus + HL + pav + block 
                       + stimulus:HL + stimulus:pav + stimulus:block
                       + HL:pav + pav:block 
                       + (1 | userId), data = df, family = binomial)

## remove all HL
model_noHL <- glmer(accuracy ~ stimulus + HL + pav + block
                     + HL:pav
                     + (1|userId), data = df, family = binomial)

### remove pav:block
model_nopavBlock <- glmer(accuracy ~ stimulus + HL + pav + block +
                            stimulus:HL + stimulus:pav + stimulus:block +
                            HL:pav + HL:block +
                            (1 | userId),
                          data = df, family = binomial)
# list of infos
models <- list(
  Full_model = model_interact,
  No_stimHL = model_nostimHL,
  No_stimPav = model_nostimPav,
  No_stimBlock = model_nostimBlock,
  No_stim = model_nostim,
  No_HLpav = model_nohlPav,
  No_HLblock = model_nohlBlock,
  No_HL = model_noHL,
  No_pavBlock = model_nopavBlock
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
  labs(title = 'Accuracy interaction models', x = 'Removed Interactions', y = 'Value') +
  theme_classic() +
  coord_cartesian(ylim = c(21000, 22500)) +
  
  scale_fill_manual(values = c('AIC' = 'blue', 'BIC' = 'red'))
plot
ggsave("acc_interact_model.png", plot = plot, width = 10, height = 6, dpi = 1000)
