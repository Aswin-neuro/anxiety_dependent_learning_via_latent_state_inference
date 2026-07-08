# cols want c() remove !c() or -c()
#select - contains, ends_with, starts_with
# dplyr:: filter(
#   new_ds <- dplyr::select(storms, 
#                           !c(lat, long, pressure, tropicalstorm_force_diameter,))
#   , status == 'hurricane'
# )

hurricanes <- storms |>
  select(!c(lat, long, pressure, ends_with("diameter"))) |>
  filter(status == 'hurricane') |>
  arrange(desc(wind), name) |>
  distinct(name, year, .keep_all = TRUE)

hurricanes |>
  select(c(year, name, wind)) |>
  write.csv('hurricanes.csv', row.names = FALSE)


#groups subfunc
#slice_head - first value in a given group
#slice_tail - gives last value in a given frp
#slice_max - gives max value in a given group
#slice_min - gives the min value in a given grp
 
# Find the strongest storm in each year
 hurricanes |> group_by(year)
 # it finds all the group of rows for each value of year
 hurricanes |> 
   group_by(year)|> 
   arrange(desc(wind)) |>
   slice_max(order_by = wind)
 #filter(year >= 1980 & year <= 1990)
 