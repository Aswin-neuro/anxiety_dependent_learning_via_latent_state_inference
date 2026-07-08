get_votes <- function(prompt = "Enter votes:"){
  repeat{
    votes <- suppressWarnings(as.integer(readline(prompt)))
    if (!is.na(votes)) {
      return(votes) 
    }
  }
  #return(votes)
}

sum <- 0
for (i in c("mik", 'yots', 'ichik')){
  votes <- get_votes(paste0(i, ": "))
  sum <- sum + votes
}

cat(sum)
