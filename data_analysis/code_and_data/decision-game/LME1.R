age = c(14,23,35,48,52,67)
pitch = c(252,244,240,233,212,204)
my.df = data.frame(age,pitch)
xmdl = lm(pitch ~ age, my.df)
summary(xmdl)
my.df$age.c = my.df$age - mean(my.df$age)
xmdl = lm(pitch ~ age.c, my.df)
summary(xmdl)
plot(fitted(xmdl),residuals(xmdl))
hist(residuals(xmdl))
qqnorm(residuals(xmdl))
which(is.na(politeness$frequency))
which(!complete.cases(politeness))
boxplot(frequency ~ attitude*gender,
        col=c("white","lightgray"),politeness)
lmer(frequency ~ attitude, data=politeness)



politeness=
  read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")