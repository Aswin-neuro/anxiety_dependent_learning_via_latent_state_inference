library(lme4)
politeness = read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
View(politeness)
which(is.na(politeness$frequency))
which(!complete.cases(politeness))
boxplot(frequency ~ attitude*gender,
        col=c("white","lightgray"),politeness)
lmer(frequency ~ attitude, data=politeness)
# got error that no random effect terms specified in formula
politeness.model = lmer(frequency ~ attitude + (1|subject)+(1|scenario), data=politeness)
summary(politeness.model)
coef(politeness.model)
politeness.model = lmer(frequency ~ attitude +
                          gender + (1+attitude|subject) +
                          (1+attitude|scenario),
                        data=politeness,
                        REML=FALSE)
coef(politeness.model)
politeness.null =lmer(frequency ~ gender + (1+attitude|subject) + (1+attitude|scenario),
                      data=politeness, REML=FALSE)
anova(politeness.null, politeness.model)

politeness.model = lmer(frequency ~ attitude +
                          gender + (1+attitude|subject) +
                          (1+attitude|scenario),
                        data=politeness,
                        REML=FALSE)

# Comparing the politeness.model to a new null model in a likelihood ratio test.
politeness.null = lmer(frequency ~ gender +
                         (1+attitude|subject) + (1+attitude|scenario),
                       data=politeness, REML=FALSE)
# the null model needs to have the same random effects structure. If full model is a random slope model, null model also
# needs to be a random slope model.
anova(politeness.null,politeness.model)

#Assumptions
all.res=numeric(nrow(mydataframe))

for(i in 1:nrow(mydataframe)){
  myfullmodel=lmer(response~predictor+
                     (1+predictor|randomeffect),POP[-i,])
  all.res[i]=fixef(myfullmodel)[some number]
}

citation("lme4")
