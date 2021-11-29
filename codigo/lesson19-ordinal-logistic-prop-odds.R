satis <- read.csv("satisfaction.csv")

View(satis)

#########
### ordinal logistic regression - checking the assumption of proportional odds
#########

### we will use the clm function in the package ordinal

require(ordinal)

model <- clm(factor(satisfaction)~type+age+imprice, data = satis)

nominal_test(model)

###  nominal_test provides likelihood ratio tests of the proportional odds assumption


