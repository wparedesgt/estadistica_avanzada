satis <- read.csv("satisfaction.csv")

View(satis)

#########
### ordinal logistic regression - goodness-of-fit measures
#########

### run the model again

require(MASS)

## set the baselines (reference categories)

satis$imprice <- relevel(factor(satis$imprice), ref="3")

satis$type <- relevel(satis$type, ref="Business traveler")

model <- polr(factor(satisfaction)~type+age+imprice, data = satis, method = "logistic")

### we will compute the goodness-of-fit indicators manually
### based on the log-likelihoods

### first we fit the null model (without independent variables)

model0 <- polr(factor(satisfaction)~1, data = satis, method = "logistic")

### now we compute the log-likelihood of both null and proposed model

LL0 <- logLik(model0)
LL1 <- logLik(model)

###### compute the pseudo R squares

## McFadden pseudo R square

mcfadden <- 1 - (LL1 / LL0)

print(mcfadden)

## Cox-Snell pseudo R square

n <- nrow(satis)

coxsnell <- 1 - exp((2/n) * (LL0 - LL1))

print(coxsnell)

### Nagelkerke pseudo R square

nagel <- (1 - exp((2/n) * (LL0 - LL1))) / (1 - exp(LL0)^(2/n))

print(nagel)

#############

### compute the deviance

deviance(model)

## get the deviance table of the model 

require(car)

Anova(model)

### this table displays the statistical significance of each independent variable



