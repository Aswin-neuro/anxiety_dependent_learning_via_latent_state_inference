df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')
library(ggplot2)
library(lme4)
library(dplyr)
library(emmeans)
library(knitr)
n_before <- nrow(df)

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

View(df)
# Checking the removal
n_after <- nrow(df)
n_removed <- n_before - n_after
n_before
n_after
n_removed
sum(df$accuracy) # it is giving number, so didt get true wiped
View(df)
# 1360 rows removed due to the na values

#checking similarity of trial count to check if uniform
table(df$HL, df$pav)
table(df$block)
## failed b3v2 has 2480/6680 no of trials, while b3v1 has 4200/6680

acc <- df %>%
  group_by(HL, pav) %>%
  summarise(mean_acc = mean(accuracy), mean_rt = mean(rt))

# null model - no interactions
# model_null <- glmer(accuracy ~ 1 + (1 | userId), data = df, family = binomial)

# main model with no interactions
model_main <- glmer(accuracy ~ stimulus + HL + pav + block + (1 | userId), 
                    data = df, family = binomial)
summary(model_main)
# comparing main to null
compare_1 <- anova(model_null, model_main)
compare_1

emm <- emmeans(model_main, ~ stimulus + HL + pav + block)
summary(emm)
# Pairwise comparisons for stimulus
stim <- pairs(emmeans(model_main, ~ stimulus), adjust = "tukey")
hl <- pairs(emmeans(model_main, ~ HL), adjust = "tukey")
pav <- pairs(emmeans(model_main, ~ pav), adjust = "tukey")
blk <- pairs(emmeans(model_main, ~ block), adjust = "tukey")

stim
# Combine
all_contrasts <- rbind(
  as.data.frame(stim),
  as.data.frame(hl),
  as.data.frame(pav),
  as.data.frame(blk)
)
all_contrasts

#export
library(gridExtra)
write.csv(all_contrasts, "pairwise_contrasts.csv", row.names = FALSE)
pdf("pairwise_contrasts.pdf", width = 11, height = 8)
grid.table(all_contrasts)
dev.off()

#########################################

#### Checking each interactions with main_model
# Interaction_model - reference
model_interact <- glmer(accuracy ~ stimulus + HL + pav + block +
                          stimulus:HL + stimulus:pav + stimulus:block +
                          HL:pav + HL:block + pav:block +
                          (1 | userId),
                        data = df, family = binomial)
interact <- summary(model_interact)

#1. removing 3 interaction with stimulus & replace with no interaction

### stimulus:HL:pav is 3 way interaction###
## remove stim:HL
model_nostimHL<- glmer(accuracy ~ stimulus + HL + pav + block
                       + stimulus:pav + stimulus:block
                       + HL:pav + HL:block + pav:block 
                       + (1 | userId), data = df, family = binomial)
## remove stim:pav 
model_nostimPav <- glmer(accuracy ~ stimulus + HL + pav + block
                         + stimulus:HL + stimulus:block
                         ---                         + HL:pav + HL:block + pav:block
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

### remove pav:block
model_nopavBlock <- glmer(accuracy ~ stimulus + HL + pav + block +
                            stimulus:HL + stimulus:pav + stimulus:block +
                            HL:pav + HL:block +
                            (1 | userId),
                          data = df, family = binomial)
# list of infos
models <- list(
  full = model_interact,
  No_stimHL = model_nostimHL,
  No_stimPav = model_nostimPav,
  No_stimBlock = model_nostimBlock,
  No_HLpav = model_nohlPav,
  No_HLblock = model_nohlBlock,
  No_pavBlock = model_nopavBlock
)
# Extract AIC/BIC values
model_stats <- data.frame(
  model = names(models),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC)
)

# Long format
model_stats <- model_stats %>%
  pivot_longer(cols = c(AIC, BIC), names_to = "Metric", values_to = "Value")
# AIC & BIC plot
ggplot(model_stats, aes(x= reorder(Model, AIC), y = BIC)) +
  geom_col(position = 'dodge')+
  coord_flip()+
  labs(title = 'Model comparison(AIC)', 
       x = 'Model', 
       y ='Value') +
  theme_classic() +
  scale_fill_manual(values = c('AIC'='blue', 'BIC' = 'red'))



# Summaries
library(broom.mixed)
interact_df <- tidy(model_interact, effects = "fixed")
pdf("pairwise_contrasts_interactions.pdf", width = 11, height = 8)
grid.table(interact_df)
dev.off()
