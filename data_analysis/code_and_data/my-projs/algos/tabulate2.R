df <- data.frame(
  name = c("Alice", "Bob", "Charlie", "miku","nino", "vivy"),
  age = c(23, 25, 22, 25, 22, 23),
  score = c(90, 85, 95,123, 323, 22 ),
  stringsAsFactors = FALSE
)
View(df)

df_out <- do.call(rbind, lapply(names(df), function(name){
  cbind(name = name, as.data.frame(df[[name]], stringsAsFactors = FALSE))
  
}))
df_out <- as.data.frame(df_out, stringsAsFactors = FALSE)
write.csv(df_out, "people_data.csv", row.names = FALSE)

read.csv("people_data.csv")
