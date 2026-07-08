library(dplyr)

df = read.csv("Sample_Superstore.csv")
View(df)

df_grp_reg_cat = df %>% group_by(Region, Category) %>%
  summarise(mean_Sales = mean(Sales), 
            mean_Profit = mean(Profit)
  )
            # .groups = 'drop')

View(df_grp_reg_cat)

# group_by(department) → groups the data by department
# summarize(avg_salary = mean(salary)) → calculates average salary for each group
# %>% It passes the result of one function as the input to the next function, making code easier to read.
# summarize(group_by(df, Region), avg_profit = mean(mean_Sales))
