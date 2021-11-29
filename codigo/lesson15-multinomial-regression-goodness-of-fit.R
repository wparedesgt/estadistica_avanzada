news <- read.csv("newspapers.csv")

View(news)

#########
### multinomial logistic regression - goodness-of-fit measures
#########

require(nnet)

### set the reference categories

news$newspaper <- relevel(news$newspaper, ref="Free Tribune")

news$political <- relevel(news$political, ref="Center")


### create the null model (without explainers)

model0 <- multinom(newspaper~1, data = news)

### create our proposed model

model <- multinom(newspaper~age+political, data = news)

### compute the log-likelihoods for both models

LL1 <- logLik(model)
LL0 <- logLik(model0)

### McFadden pseudo R square

mcfadden <- 1 - (LL1 / LL0)

print(mcfadden)

## Cox-Snell pseudo R square

n <- nrow(news)

coxsnell <- 1 - exp((2/n) * (LL0 - LL1))

print(coxsnell)

## Nagelkerke pseudo R square

nagel <- (1 - exp((2/n) * (LL0 - LL1))) / (1 - exp(LL0)^(2/n))

print(nagel)

### get the deviance

deviance(model)


