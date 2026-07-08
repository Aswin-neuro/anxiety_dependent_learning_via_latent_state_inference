chicks <- read.csv("chicks.csv")
chicks <- subset(chicks, !is.na(weight))
# To determine the feed options
feed_opts<- unique(chicks$feed) # unique will not repeat the options
print(feed_opts) # gives different feed in order
#Prompt user with options - like it will index into the 

#cat("1.", feed_opts[1], "\n")

# Not the best way
# cat can take a vector of inputs
# Format options
# formatted_opts = num "," "name"
# THis vector being the result of 3 values - SI no. , dot, feed option 
# then concatenate these three

length(feed_opts)

formatted_opts <- paste0(1:length(feed_opts), ". ", feed_opts)
print(formatted_opts)

cat(formatted_opts, sep = "\n")

feed_choice <- as.integer((readline("feed type: ")))
#handling invalid choice
if (feed_choice <1 || feed_choice >6)  {
  cat("Invalid choice!")

}  else {
  selected_feed <- feed_opts[feed_choice]
  print(subset(chicks, feed == selected_feed))
}
# Making it mutually exclusive will helps in not getting output
# while the input being invalid
# } else if(feed_choice >=1 && feed_choice <= 6) {
#   
# Used when multiple questions or conditions are needed  
# }