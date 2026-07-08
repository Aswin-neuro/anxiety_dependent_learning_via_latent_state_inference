load("candy.RData")
ggplot(
  candy, # THis is the data frame to be plotted
  aes(x= price_percentile, y=sugar_percentile)
)+
  geom_jitter( # used to add random noise to points
    #in a scatter plot to reduce overplotting
    color = "darkorchid",
    fill = "orchid",
    size = 1.5,
    shape = 21
  )+
  labs(
    x = "Price",
    y = "Sugar",
    title = "Price and Sugar"
  )+
  theme_classic()


