library(LearnCommAnalysis)

#Runner distance
duration <- c(30,60,45,44,36,58,57,22,25)
distance <- c(2.5,5.1,3.9,3.9,3,4.9,4.8,1.7,2.1)

plot(duration,distance)

m1 <- lm(distance~duration) ## linear model with response ~ predictor
summary(m1) ## see output of model

abline(m1, col="red") ## adds line from model
#Here the parameter coefficient is under Estimate as 0.087 kilometres per minute or 5.22 kph.
#We also notice that the p value is extremely small and the R squared close to 1. 
#These inform us that the model fits accurately. 
#We can plot this relationship using the abline function. 
#The distance between the line and each point is the residuals 

#GLMs
head(drug.trial)
drug.trial <- drug.trial

m2<- glm(response~predictor, data=drug.trial, family="binomial") ##glm for drugs potential to curse
summary(m2) ##output of model
#Residual deviance should be a 1:5 ratio or smaller, otherwise its not fitting well. 
#the deviance should not be more than double the degrees of freedom,
#Remedy by using quasi dispersion relationship
#So add quasi

anova(m2, test="Chisq") ## chisq test on predictor from model (POST HOC TEST)

#2.2 Pairwise comparisons and packages
#To see which drug is most effective, we need to conduct pairwise comparisons.
library(lsmeans)

lsmeans(m2, pairwise~predictor, adjust="tukey") ##conduct pairwise comparisons on predictor 
##Tilda tells it which colum to run on

#Sig difference of drug 3 than control. And drug 3 significantly diff than drug 1.

#2.3 Different types of GLMs

#Try self
bird.data <- bird.data
str(bird.data)
m3 <- glm(species.rich~habitat, data = bird.data, family = "poisson") #Actually needed poisson family
summary(m3)
?glm()
anova(m3, test="Chisq") ## chisq test on predictor from model
lsmeans(m3, pairwise~habitat, adjust="tukey") ##conduct pairwise comparisons on predicto

#2.5 Generalized Linear Mixed Model
#Random effects are variables that are included in the model to account for common variance among your replicates
##Example, drug testing but everyones blood pressure is likley different at the start of the trial
###Year affecting your model
ranch <- ranch
head(ranch)#Biomass of plants per year, looking for cattles effect on biomass
plot(ranch$cattle, ranch$biomass, ylab="Biomass (kg/m)", xlab="Cattle (per ha)", pch=19)

library(lme4) #To conduct a mixed model requires the package lme4 and the random factor is fitted using the operator (1 | randomeff)

m1 <- lmer(biomass ~ cattle + (1|year), data=ranch)
summary(m1)#Notice no p value because df can only be estimated

m1 <- lmer(biomass ~ cattle + (1|year), data=ranch)
m0 <- lmer(biomass ~ 1 + (1|year), data=ranch)
#Compare 2 models, one null, to see if fixed effect HAS an effect on biomass
#If you had multiple response variable or fixed effect, you have to compare EVERy one
#Cant get significance from random effects
anova(m1,m0, test="Chisq")

## refitting model(s) with ML (instead of REML)
library(lmerTest)

m1 <- lmer(biomass ~ cattle + (1|year), data=ranch)
anova(m1, test="Chisq")
#Can do generalized l models by adding g
m4 <- glmer(biomass ~ cattle + (1|year), data=ranch, family = "gamma")
#2.6 Generalized Additive Models 
#Usually used for time series
#GAMs borrow all the flexibility of GLMs with the exception of being able to account for non-linearity.
library(mgcv)

head(gam.data)
gam.data <- gam.data

## Plot the data
ggplot(gam.data, aes(x=year, y=ndvi)) + geom_point()
## Run a linear model
m1 <- lm(ndvi~year, data=gam.data)
summary(m1)
## add linear model to plot
ggplot(gam.data, aes(x=year, y=ndvi)) + geom_point()+  geom_smooth(method = "lm", formula = y ~x)
## check the fit
par(mfrow = c(2,2))
plot(m1)

## Fit GAM
m2 <- gam(ndvi~s(year), data=gam.data, method="REML")#Restricted max liklihood
summary(m2)

#According to output, capturing and explaining >80% of the deviance
## check diagnostics
par(mfrow = c(2,2))
gam.check(m2)

#closer edf is to k, the worse it is. 

## see final plot
ggplot(gam.data, aes(x=year, y=ndvi)) + geom_point() +  geom_smooth(method = "gam", formula = y ~s(x))

## Change back plot pattern
par(mfrow = c(1,1))
