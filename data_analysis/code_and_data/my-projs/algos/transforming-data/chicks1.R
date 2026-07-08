chicks <- read.csv("chicks.csv")
#chicks <- chicks[!is.na(chicks$weight),]
chicks <- subset(chicks, !is.na(weight))

View(chicks)
soybean <- subset(chicks, feed == "soybean")
soybean

#since we have removed a lot of the rows with na value
#there will be a gap in the ordering.
rownames(chicks) <- NULL
# setting the rowname vector null will remove the existing values
#R will then rebuild the values

rownames(chicks)

x <- is.na(chicks$weight)
sum(x) # gives the num of values in the vectors
View(chicks)

# How do we check, how many NAs we had in total
# This can be done by counting the number of is.na TRUE sum

## PART2 ##
# We are going to output the required data per the requirements of the user



