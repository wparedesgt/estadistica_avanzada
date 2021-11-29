news <- read.csv("newspapers.csv")

View(news)

#########
### how to perform the multinomial logistic regression
#########

#########
### Basic assumptions:

# the dependent variables do not present outliers
# there is no important multicollinearity
##########

### we will determine whether the preferred newspaper is influenced
### by age and political orientation

### dependent variable: preferred newspaper, with 3 categories
### Daily News, National Politics, Free Tribune

### explainers: age and political opinion (political)
### political is a categorical variable with three categories:
### Left-wing, Right-wing, Center

### before running the regression, we must set the reference category (baseline)
### for the categorical variables in the model

## set the baseline

news$newspaper <- relevel(news$newspaper, ref="Free Tribune")

news$political <- relevel(news$political, ref="Center")

### run the multinomial regression
### using the nnet package, multinom function

require(nnet)

model <- multinom(newspaper~age+political, data = news)

summ <- summary(model)

print(summ)

### the multinom function only computes the coefficients and their standard errors
### it does not compute the p values

### we must compute the p values manually

### we compute the z scores first

z <- summ$coefficients/summ$standard.errors

### we generate the p values of the z scores (two-tailed)

pv <- pnorm(abs(z), lower.tail = F) * 2

print(pv)


