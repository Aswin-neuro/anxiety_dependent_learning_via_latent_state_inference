
# We should not ignore the NA values
# Subsetting the dataframe only to the rows in which the chick ate casein
#df[row, col] - subsetting the data
#a_vector_of_indices <- (c(1,2,3))
#casein_c <- chicks[a_vector_of_indices,]
#mean(casein_c$weight)

chicks <- read.csv("chicks.csv")
mean(chicks$weight, na.rm = TRUE)
torem<-(chicks$weight == NA)
filter <- (chicks$feed == "casein")
casein_chicks <- chicks[filter, ]
mean(casein_chicks$weight)
f2 <- !(is.na(chicks$weight))
which(f2)
