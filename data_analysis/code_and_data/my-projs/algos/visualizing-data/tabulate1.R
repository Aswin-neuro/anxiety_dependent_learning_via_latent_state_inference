votes <- read.csv('votes.csv')
# we want a vector of output for each candidates
View(votes)
votes[,1]
for (i in 1:nrow(votes)){
  print(votes[i, ])
}
