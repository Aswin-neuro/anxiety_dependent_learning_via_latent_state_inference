load("temps.RData")
mean(temps)
temps[2]
temps[4]
temps[7]
# remove these elements from the list
# Reassigning temps
temps <- temps[-c(2,4,7)]
temps
# make a vector of those outliers to work with
# we will index into the position of the vector
# we will give a vector of indexes

#1. go through the data and give us the outliers's positions
# We have " is it an outlier or not -> using logical expression 
# Yes or no que and yes or no answers, comparison operators
any(temps < 0 | temps >60)
all(temps<0 | temps>60)

filter <- !(temps > 60 | temps < 0)
temps[filter]

save(filter, file = "no_outliers.RData")
# gives the indice of the values for the if conditions
# Similar to the list.index(value)





