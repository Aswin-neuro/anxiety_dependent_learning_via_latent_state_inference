df <- read.csv('/home/aswin/R/Projects/Reinforcement Learning/loseless_game-data.csv')
library(ggplot2)
library(lme4)
library(dplyr)
library(tidyr)
library(emmeans)
library(knitr)

# Data preprocessing - clean, convert to factors, acc to numeric
## cleaning up NA
df <- df %>%
  filter(
    !is.na(rt),
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
df$rt <- as.numeric(df$rt)
# 1360 rows removed due to the na values
#checking similarity of trial count to check if uniform
table(df$HL, df$pav)
table(df$block)
## failed b3v2 has 2480/6680 no of trials, while b3v1 has 4200/6680

#########################################

# Addition_model - reference
model_addition <- lmer(rt ~ stimulus + HL + pav + block + (1 | userId),
                        data = df)
addition <- summary(model_addition)
addition
model_nostim<- lmer(rt ~ HL + pav + block + (1 | userId),
                     data = df)
model_noHL<- lmer(rt ~ stimulus + pav + block + (1 | userId),
                   data = df)
model_nopav<- lmer(rt ~ stimulus + HL + block + (1 | userId),
                    data = df)
model_noblock<- lmer(rt ~ stimulus + HL + pav + (1 | userId),
                      data = df)

# list of infos
models <- list(
  Full_model = model_addition,
  No_Stimulus = model_nostim,
  No_HL = model_noHL,
  No_pav = model_nopav,
  No_block = model_noblock
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
plot <- ggplot(model_stats_long, aes(x = reorder(model, Value), y = Value, fill = Metric)) +
  geom_col(position = 'dodge') +
  geom_text(aes(label = round(Value, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  labs(title = 'RT Addition model comparison', x = 'Model', y = 'Value') +
  theme_classic() +
  coord_cartesian(ylim = c(58000, 65000)) +
  scale_fill_manual(values = c('AIC' = 'blue', 'BIC' = 'red'))

plot
ggsave("rt_addition_model_comparison.png", plot = plot, width = 10, height = 6, dpi = 1000)
