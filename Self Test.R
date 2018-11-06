head(multivar)

Q1<- lm(div.data~Microsite, data=multivar) ##glm for shrubbery vs diversity

summary(Q1)

Q2<- glm(div.data~Microsite, data=multivar) ##glm for drugs potential to curse
summary(Q2)

anova(Q2, test="Chisq") ## chisq test on predictor from model

lsmeans(Q2, pairwise~Microsite, adjust="tukey") ##conduct pairwise comparisons on predictor
