get_votes <- function(prompt = 'Enter votes: '){
  repeat{
    votes <- as.integer(readline(prompt))
    if(!is.na(votes)) {
      break
    } else {
      
    }
  }
  ifelse(is.na(votes), 0, votes)
}
# Even without explicitly writing vote,R will automatically
# return the votes
# override already set params but arg = ''
mik <- get_votes('mik ')
yots <- get_votes('yots ')
nino <- get_votes('nino ')

total <- sum(mik,nino,yots)
cat("total:", total)

# defining fn


# Loops
i <- 3
  repeat {
    cat('quack! \n')
    i <- i -1
    if (i == 0){
      break
    } else {
      next # go and continue the next iteration
    }
  }
i <- 3

while (i != 0) {
  cat('quack \n')
  i <- i -1
}

#for loop for a vector of elements
for (i in c(1:3)){
  cat("quack\n")
}
