votes <- read.csv('votes.csv')
View(votes)
total_votes <- c()
rownames(votes)
for (cand in rownames(votes)){
  #total_votes[cand] <- sum(votes[cand, ])
  total_votes <- c(total_votes, sum(votes[cand, ]))
  }
# total_votes['mario'] <- 100
or

total_votes <- c()
for (method in colnames(votes)) {
  total_votes[method] <- sum(votes[,method])
}

apply(votes, MARGIN = , FUN = sum)