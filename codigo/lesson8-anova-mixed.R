diet <- read.csv("diet3.csv")

View(diet)

#########
### the mixed analysis of variance
#########

#########
### Basic assumptions:

# the dependent variables are normally distributed
# the dependent variables do not present outliers
# there is homogeneity of variances (for the between-subjects factor)*
# there is homogeneity of covariances (for the between-subjects factor)*
# there is sphericity (for the within-subjects factor)*

### we will check only the assumptions marked with an asterisk (*)
##########

### we will determine whether there is a significant difference in average weight
### between the three moments of the diet, for both male and female subjects

### within-subjects factor: time (beginning, middle, end)
### between-subjects factor: gender (male, female)

###############

###### check the assumption of equal variances (for each dependent variable)

require(car)

leveneTest(diet$weight_beg, diet$gender)

leveneTest(diet$weight_mid, diet$gender)

leveneTest(diet$weight_end, diet$gender)

####### check the assumption of equal covariances (Box's M test)

require(biotools)

### from the diet data frame, extract the dependent variables

diet2 <- diet[c(2,3,4)]

View(diet2)

boxM(diet2, diet$gender)

################# get to the ANOVA

### prepare and load a new data frame, with all the combinations of the factors

fact <- read.csv("factors-mixed.csv")

View(fact)

### create a new data frame with the male subjects only 

dietm <- diet[diet$gender=="male",]

View(dietm)

### rename the columns conveniently

colnames(dietm) <- c("gender", "weight_beg_male", "weight_mid_male", "weight_end_male")

### create a new data frame with the female subjects only

dietf <- diet[diet$gender=="female",]

View(dietf)

### rename the columns

colnames(dietf) <- c("gender", "weight_beg_female", "weight_mid_female", "weight_end_female")

### create a matrix with all the dependent variables

weight <- cbind(dietm$weight_beg_male, dietm$weight_mid_male, 
              dietm$weight_end_male, dietf$weight_beg_female, dietf
              $weight_mid_female, dietf$weight_end_female)

View(weight)

### get the means of all the dependent variables

model <- lm(weight~1)

summary(model)

### finally, create the ANOVA model

model2 <- Anova(model, idata=fact, idesign=~time+gender*time, type="III")

summary(model2, multivariate=F)    ## we don't want the MANOVA results

### gender and time are the variables of the data frame fact

### since the interaction effect is statistically significant,
### we are going to compute the simple main effects of the factors



