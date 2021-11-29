news <- read.csv("newspapers.csv")

View(news)

#########
### multinomial logistic regression - compute and interpret the odds (antilogs of coefficients)
#########

require(nnet)

### set the reference categories and execute the multinom function

news$newspaper <- relevel(news$newspaper, ref="Free Tribune")

news$political <- relevel(news$political, ref="Center")

model <- multinom(newspaper~age+political, data = news)

### compute the antilogarithms of the coefficients

expb <- exp(coef(model))

print(expb)

### compute the confidence intervals for the coefficients

ci <- confint(model, level = 0.95)

print(ci)

### compute the confidence intervals for the antilogarithms

expci <- exp(ci)

print(expci)

### compute the predicted probabilities

pred <- fitted(model)

View(pred)


