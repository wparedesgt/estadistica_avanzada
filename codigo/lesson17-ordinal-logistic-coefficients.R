satis <- read.csv("satisfaction.csv")

View(satis)

#########
### ordinal logistic regression - interpreting the antilogarithms (odds)
#########

### let's run the model again

require(MASS)

## set the baselines (reference categories)

satis$imprice <- relevel(factor(satis$imprice), ref="3")

satis$type <- relevel(satis$type, ref="Business traveler")

model <- polr(factor(satisfaction)~type+age+imprice, data = satis, method = "logistic")

### compute the odds (antilogarithms of the coefficients)

odds <- exp(coef(model))

print(odds)

### get the confidence interval for the odds

ci <- exp(confint(model))

print(ci)


