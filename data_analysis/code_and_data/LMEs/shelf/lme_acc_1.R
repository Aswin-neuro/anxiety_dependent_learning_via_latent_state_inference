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

###################################
## Accuracy ###
#############################

# null model - no interactions
model_null <- glmer(accuracy ~ 1 + (1 | userId), data = df, family = binomial)
# main model with no interactions
model_main <- glmer(accuracy ~ stimulus + HL + pav + block + (1 | userId), 
                    data = df, family = binomial)
summary(model_main)
# comparing main to null
compare_1 <- anova(model_null, model_main)
compare_1

#Pairwise comparisons for model main.
library(emmeans)

# Compute estimated marginal means
emm <- emmeans(model_main, ~ stimulus + HL + pav + block)

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

x <- summary(all_contrasts)
y <- as.data.frame(x)
View(y)
z <- kable(all_contrasts, digits = 3)
print(z)
# saviall_contrasts# saving model summary
sink("kable_model_acc vs all params_pairwise_test.txt")   # Start redirecting output
summary(z)         # Replace with your model object
sink()                      # Stop redirecting


model0 <- glmer(accuracy ~ 1 + (1 | userId), data = df, family = binomial)
model1 <- glmer(accuracy ~ stimulus + HL + pav + block + (1 | userId), data = df, family = binomial)

#plot
library(sjPlot)
plot_model(compare_1, type = "est", show.values = TRUE, value.offset = 0.3)

