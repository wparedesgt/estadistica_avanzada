comp <- read.csv("company.csv")

View(comp)

#########
### how to perform a simple discriminant analysis
#########

### we will determine whether the employee gender can be predicted
### based on other variables

### dependent variable: gender
### independent variables: education level (educ), salary,
### months at the current job (jobtime), previous experience in months (prevexp)

### run the DA using the lda function

require(MASS)

### get the prior probabilities, the group means and
### the coefficients of the discriminant function (CV = FALSE)

lmod1 <- lda(gender~educ+salary+jobtime+prevexp, data=comp, CV = F, method="mle")

print(lmod1)

### get the predicted group membership and the posterior probabilities (CV = TRUE)

lmod2 <- lda(gender~educ+salary+jobtime+prevexp, data=comp, CV = T, method="mle")

print(lmod2)

### to compute the discriminant function scores

pred <- predict(lmod1)

### get the scores

pred$x

########## compute the Wilks' lambda

### create a matrix with the independent variables
### remove columns 1 and 3 (gender and job) 

matr <- as.matrix(comp[c(-1, -3)])

View(matr)

### apply manova to the new matrix
### requiring the Wilks test

summary(manova(matr~comp$gender), test = "Wilks")

####### create the classification table (to assess the prediction accuracy)

### create a new data set with the real and expected groups

comp2 <- cbind(comp$gender, lmod2$class)

comp2 <- data.frame(comp2)

View(comp2)

### give names to the variables

colnames(comp2) <- c("original", "predicted")

### build the cross table

require(gmodels)

CrossTable(comp2$original, comp2$predicted, digits = 2, expected=FALSE, prop.r=TRUE, prop.c=FALSE,
           prop.t=FALSE, prop.chisq=FALSE, chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
           missing.include=FALSE)

