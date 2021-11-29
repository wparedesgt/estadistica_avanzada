vit = read.csv("vitamin-a.csv")

View(vit)

#########
### the analysis of covariance - checking the assumptions
#########

#########
### Basic assumptions:

# the response variable do not present outliers
# the relationship between the dependent variable and the covariate is linear
# there is no relationship between the covariate and the factor*
# the residuals of the response variable are normally distributed*
# there is homogeneity of variances*
# there is homoskedasticity*

### we will check only the assumptions marked with an asterisk (*)
##########

##########  check for the homogeneity of regression slopes
########## i.e. independence between factor and covariate

### we compute the interaction between age and dose of vitamin

model <- aov(effort~age*dose, data=vit)
av <- Anova(model, type="III")
print(av)

############ check for the normality of residuals

### get the residuals and standardize them

res <- residuals(model)

zres <- scale(res)

shapiro.test(zres)

########## check for the homogeneity of variances

require(car)

leveneTest(vit$effort, vit$dose)

########## check for homoskedasticity

### get the predicted values of the dependent variable

pred <- predict(model)

### build the scatterplot (predicted vs. residuals)

require(ggplot2)

ggplot()+geom_point(aes(x=pred, y=zres))



