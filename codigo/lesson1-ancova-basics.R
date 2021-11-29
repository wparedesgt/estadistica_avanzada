vit = read.csv("data/vitamin-a.csv")

View(vit)

#########
### how to perform the one-way analysis of covariance (ANCOVA)
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

### dependent variable: effort resistance
### factor: dose of vitamin
### covariate: employees' age

####################

### how to run the ANCOVA, get the adjusted means
### and do multiple comparisons of the adjusted means

### load the car package
### so we can compute the type III sum of squares

require(car)

### run the ANCOVA

model <- aov(effort~dose+age, data=vit)
ancova <- Anova(model, type="III")
print(ancova)

### compute the adjusted means

require(effects)

effect("dose", model)

#### perform multiple comparisons between adjusted means

require(multcomp)

mcomp <- glht(model, linfct=mcp(dose="Tukey"))

### linfct (linear function) specifies the hypotheses to be tested 
### here we use the mcp (multiple comparisons) specifying the option Tukey

### get the differences and their statistical significance

summary(mcomp)

### get the confidence interval for the differences

confint(mcomp)

