satis <- read.csv("satisfaction.csv")

View(satis)

#########
### how to perform the ordinal logistic regression
#########

#########
### Basic assumptions:

# the dependent variables do not present outliers
# there is no important multicollinearity
# the condition of proportional odds is met*

### we will only check the assumptions marked with an asterisk (*)
##########

### we will determine whether the satisfaction level depends on
### the other variables

### dependent variable: satisfaction with the hotel services
### 1 - not at all satisfied, 4 - very satisfied

### the explainers are the following: 
### customer age 
### customer type: pleasure traveller or business traveller
### importance of price: 1 - not important, 2 - somewhat important, 3 - very important

### N.B. the ordinal variables must be coded numerically
### the nominal variables can be string

### load the package

require(MASS)

## set the baselines (reference categories) for the categorical explainers

satis$imprice <- relevel(factor(satis$imprice), ref="3")

satis$type <- relevel(satis$type, ref="Business traveler")

model <- polr(factor(satisfaction)~type+age+imprice, data = satis, method = "logistic")

summary(model)

### compute the p values for the coefficients

cft <- coef(summary(model))

print(cft)

pv <- pnorm(abs(cft[,"t value"]), lower.tail = F) * 2

print(pv)

### add the p values to the coefficients table

cft <- cbind(cft, "p value" = pv)

print(cft)



