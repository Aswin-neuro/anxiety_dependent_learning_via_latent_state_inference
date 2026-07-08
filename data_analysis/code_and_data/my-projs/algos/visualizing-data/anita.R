load("anita.RData")
ggplot(anita, aes(x=timestamp, y = wind))+
  geom_line( #added over the line in which the points are plotted
    linetype = 1,
    linewidth = 1
  )+
  geom_point(color = "darkblue", # The data points of the graph
             size =2
             )+
  geom_hline( # For setting up the horizontal line
    linetype =3,
    yintercept = 65
  )+
  labs( # Giving labels
    x="Date",
    y = "Wind Speed(knots)",
    title = "Hurricane Anita"
    
  )
