mobi = read.csv("mobilenet.csv")

View(mobi)

#########
### the binomial logistic regression - goodness-of-fit indicators
#########

### run the regression model again

model <- glm(mobile~income+hours+where, data=mobi, family=binomial())

### compute the Hosmer-Lemeshow statistic

require(ResourceSelection)

hoslem.test(mobi$mobile, fitted(model))

### compute the Nagelkerke pseudo R square

require(fmsb)

NagelkerkeR2(model)

#### compute all the pseudo R square indicators

require(BaylorEdPsych)

PseudoR2(model)


