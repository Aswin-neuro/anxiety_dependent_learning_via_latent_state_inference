# votes <- read.table(
#   "votes.csv",
#   sep=",",
#   header=TRUE
# )
votes <- read.csv("votes.csv")
View(votes)
nrow(votes)
ncol(votes)
unique(votes$candidate)
x<- c(1,2,3,4,5,55,5,55)
unique(x)
val = c("yes", "NO", "unknown")
exclude = c(-1)

val
