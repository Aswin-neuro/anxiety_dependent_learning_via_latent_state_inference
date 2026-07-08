#Getting input from use to choose a csv
path <- readline(prompt="Enter PATH to .csv")
data <- read.csv(path)
View(data)

# output total number of pitstops = no. of rows
# shortest pitstop duration
# longest pitstop duration
# total pitstop across the pit stop across all racers
print(paste0("Total number of pitstops is :", nrow(data)))
print(paste0("Shortest pitstop duration is :", min(data$time)))
print(paste0("longest pitstop duration is :", max(data$time)))      
print(paste0("total pitstop duration is : ", sum(data$time)))
