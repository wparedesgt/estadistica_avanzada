diet <- read.csv("diet2.csv")

View(diet)

#########
### the within-within-subjects analysis of variance
#########

#########
### Basic assumptions:

# the variables are approximately normally distributed
# the variables do not present significant outliers
# there is sphericity*

### we will check only the assumptions marked with an asterisk (*)
##########

### dependent variable: weight
### factors: time (beginning, middle, end) and physical exercises (with and without exercises)

### first you must prepare a data frame with the combined factor levels
### like this one:

fact <- read.csv("factors-within-within.csv")

View(fact)

### create a matrix with all the dependent variables

weight <- cbind(diet$weight_beg, diet$weight_mid, diet$weight_end, 
             diet$weight_beg_ex, diet$weight_mid_ex, diet$weight_end_ex)

### get the means of the dependent variables

model <- lm(weight~1)
summary(model)

### run the ANOVA

require(car)

model2 <- Anova(model, idata=fact, idesign=~Exercise*Time, type="III")

summary(model2, multivariate=F)   ## we do not need the MANOVA results

### Exercise and Time are the variable names in the fact data frame

### since the interaction effect is significant, we must compute
### the simple main effects of the factors time and exercise


