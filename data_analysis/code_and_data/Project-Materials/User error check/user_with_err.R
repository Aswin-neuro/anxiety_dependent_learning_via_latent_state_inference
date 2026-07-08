df <- read.csv('/home/aswin/R/Projects/statistical-test/final_game-data.csv')
df1 <- read.csv('/home/aswin/R/Projects/decision-game/anxiety-scores.csv')

data <- data %>% filter(!userId %in% c('220317','220333','220613','240997'))
View(data)

View(df1)
length(unique(df$userId))
df <- df %>%
  filter(
    !is.na(accuracy),
    !is.na(stimulus),
    !is.na(HL),
    !is.na(pav),
    !is.na(block)
  )

lol<- df %>% 
  filter(is.na(HL))
head(lol)
unique(lol$userId)

length(unique(df$userId))

df_filtered <- df1 %>% filter(userId %in% c(220317,220333,220613,240997))
View(df_filtered)
