mobi <- read.csv("mobilenet.csv")

View(mobi)

#########
### how to perform the binomial logistic regression
#########

#########
### Basic assumptions:

# the independent variables do not present outliers
# there is no important multicollinearity
##########

### we will predict the chance that a subject uses mobile Internet
### based on the other three variables

### dependent variable: has/does not have mobile internet (1/0)
### explainers: income, hours spent on the Internet, where they use the Internet

### where is a dichotomous variable: at home (0) and at the office (1)

### very important: the dependent variable must be coded numerically

### to run the regression, we use the glm function

model <- glm(mobile~income+hours+where, data=mobi, family=binomial())

### for the categorical independent variable where, 
### home (0) is the reference category

summary(model)

### Null deviance - the difference between the LL of the saturated model 
### and the LL of the null model (with intercept only)

### Residual deviance - difference between the LL of the 
### saturated model and the LL of the proposed model

### the saturated model is the model where each case has its own parameter
### the proposed model is better than the saturated model
### because it has a lower LL

######### compute the antilogarithms of the coefficients
######### these antilogs actually represent the chance that a subject uses mobile Internet

expb <- exp(coef(model))

print(expb)

### compute the confidence interval of the antilogarithms

intexp <- exp(confint(model))

print(intexp)


