hurricanes <- read.csv("hurricanes.csv")
View(hurricanes)

hurricanes |> 
  group_by(year)|> 
  # arrange(desc(wind)) |>
  # slice_max(order_by = wind)
  # summarize(mean(windspeed))
  summarize(result = n())
# result is the heading for the output column
# n is a function which finds in every groups
# how many rows there are.
  # ungroup() will remove the temporary group created

# Principle of tidyr 
# Each observation is a row; each row is an observation

