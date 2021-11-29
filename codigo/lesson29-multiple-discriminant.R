comp <- read.csv("company.csv")

View(comp)

#########
### how to perform a multiple discriminant analysis
#########

### dependent variable: job category (1 - employees, 2 - middle managers, 3 - top managers)
### independent variables: education level (educ), salary,
### months at the current job (jobtime), previous experience in months (prevexp)

### run the DA using the lda function

require(MASS)

### get the prior probabilities, the group means and
### the coefficients of the discriminant function (CV = FALSE)

lmod1 <- lda(job~educ+salary+jobtime+prevexp, data=comp, CV = F, method="mle")

print(lmod1)

### get the classes and the posterior probabilities (CV = TRUE)

lmod2 <-lda(job~educ+salary+jobtime+prevexp, data=comp, CV = T, method="mle")

print(lmod2)

### compute the Wilks lambda

matr <- as.matrix(comp[c(-1, -3)])

View(matr)

summary(manova(matr~comp$job), test = "Wilks")

### get the discriminant function scores

pred <- predict(lmod1)

pred$x

### build the classification table

comp2 <- cbind(comp$job, lmod2$class)

comp2 <- data.frame(comp2)

View(comp2)

colnames(comp2) <- c("original", "predicted")

require(gmodels)

CrossTable(comp2$original, comp2$predicted, digits = 2, expected=FALSE, prop.r=TRUE, prop.c=FALSE,
           prop.t=FALSE, prop.chisq=FALSE, chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
           missing.include=FALSE)



