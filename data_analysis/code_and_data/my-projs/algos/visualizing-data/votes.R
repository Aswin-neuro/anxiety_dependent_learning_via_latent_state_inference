votes <- read.csv("votes.csv")
# giving it some data, then some geometry
# we are adding a new layer(using +) to the blank plot
p <- ggplot(votes, aes(x = candidate, y = votes))+
  geom_col(
    aes(fill = candidate), # to fill the plots with color
    show.legend = FALSE # to remove the color labels
  )+
  scale_fill_viridis_d()+ #Color blindness specific colors
  scale_y_continuous(limits = c(0, 250))+ # We are changing the range of the y axis here
  labs(
      x = "Candidate",
      y = "Votes",
      title = "Election results"
    )+
  theme_classic()

# p + to add more functions
ggsave( # While saving we need to specify all these to save it properly
  "votes.png",
  plot = p,
  width = 1200,
  height = 900,
  units = "px"
)
  
  