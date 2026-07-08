library(lme4)
pol = read.csv('/home/aswin/R/Projects/LMEs/politeness_data.csv')
head(pol)
tail(pol)
summary(pol)
str(pol)
colnames(pol)
which(is.na(politeness$frequency))
which(!complete.cases(politeness))
boxplot(frequency ~ attitude*gender,
        col=c("white","lightgray"),politeness)
lmer(frequency ~ attitude, data=politeness)
# error - no random effect terms specified in formula
# because this model needs a random effect
# adding random intercepts for subjects and items - here items are called scenarios

politeness_model = lmer(frequency ~ attitude +
                          (1|subject) + (1|scenario), data=politeness)
## comparing null and main model
politeness_model =  lmer(frequency~ attitude +gender+(1|subject)+(1|scenario),
                         data=politeness, REML=FALSE)
politeness_null = lmer(frequency ~ gender +
                         (1+attitude|subject) + (1+attitude|scenario),
                       data=politeness, REML=FALSE)
anova(politeness_null,politeness_model)
summary(politeness_model)

## checking assumptions
all_res = numeric(nrow(dataframe))
for (i in 1:nrow(mydataframe)){
  mymodel= lmer(response~predictor+
                  (1+predictor|randomeffect),POP[-i,])
  all_res[i]=fixef(myfullmodel)[some number]
}