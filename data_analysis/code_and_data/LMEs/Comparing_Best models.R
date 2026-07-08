df <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
library(ggplot2)
library(lme4)
library(tidyr)
library(dplyr)
library(emmeans)
library(knitr)
library(sjPlot)
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

###
# No _HL block int model

model_nohlBlock <- glmer(accuracy ~ stimulus + HL + pav + block 
                         + stimulus:HL + stimulus:pav + stimulus:block
                         + HL:pav + pav:block 
                         + (1 | userId), data = df, family = binomial)

nohlBlock <- glmer(accuracy ~ stimulus + pav + block 
                         + stimulus:pav + stimulus:block
                        + pav:block 
                         + (1 | userId), data = df, family = binomial)
# mult models
model_multiplication <- glmer(accuracy ~ stimulus * HL * pav * block + (1 | userId), data = df, family = binomial)

model_nopav<- glmer(accuracy ~ pav + stimulus * HL * block + (1 | userId),
                    data = df, family = binomial)

plot_model(model_multiplication, show.values = TRUE, value.offset = .3)
plot_model(model_nohlBlock, show.values = TRUE, value.offset = .3)
plot_model(model_nopav, show.values = TRUE, value.offset = .3)

plot_model(nohlBlock, show.values = TRUE, value.offset = .3)

names(fixef(model_nohlBlock))
p <- plot_model(model_nohlBlock, show.values = TRUE, value.offset = 0.3)
ggsave("model_nohlblock_int.png", plot = p, width = 6, height = 6, dpi = 2000)
