## change filter for LTA and HTA
df <- df %>%
  select(userId, block, accuracy, rt, pav, HL) %>%    # Select only relevant columns
  filter(HL == 'LTA') %>%                             # Filter data to only include 'LTA' entries
  group_by(block, userId) %>%                         # Group data by block and userId
  summarize(
    acc_usr = mean(accuracy)                          # Calculate the mean accuracy for each user in each block
  )
df