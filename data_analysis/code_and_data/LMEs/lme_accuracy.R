# Load data
df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')

# Load necessary libraries
library(ggplot2)
library(lme4)

# Inspect structure
head(df)
colnames(df)
summary(df)

# Check for missing values in 'accuracy'
which(is.na(df$accuracy))
which(!complete.cases(df$accuracy))

# Boxplot: accuracy by block and stimulus
df$accuracy_bin <- as.numeric(df$accuracy)  # convert TRUE/FALSE to 1/0

model <- glmer(
  accuracy_bin ~ stimulus + block + HL + pav + (1 | userId),
  data = df,
  family = binomial
)

model1 <- glmer(
  accuracy_bin ~ stimulus * HL * pav + block + (1 | userId),
  data = df,
  family = binomial
)

summary(model)
summary(model1)
