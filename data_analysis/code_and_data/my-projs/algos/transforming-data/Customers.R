# When we are having to combine different datasets from
# different tables into one
Q1<- read.csv("Q1.csv")
#Adding a new column
Q1$quarter <- "q1"

Q2<- read.csv("Q2.csv")
Q2$quarter <- "q2"
Q3<- read.csv("Q3.csv")
Q3$quarter <- "q3"
Q4<- read.csv("Q4.csv")
Q4$quarter <- "q4"
# They have the same structure -> same no. of columns but diff no rows
# Such dfs can be combined using rbind
sales <- rbind(Q1,Q2,Q3,Q4)
View(sales)

#Logex for categorizing based on the values
x<-ifelse(sales$sale_amount > 100, "High value", "Regualar")
# Now adding this as a column
sales$value <- x

View(sales)

# Adding a new column to check the sale value
# rbind binds the dataframes to bottom of the first one
