diet = read.csv("diet2.csv")

View(diet)

#########
### the simple main effects of the factor exercise
#########

### the simple main effects of the factor exercise represent the effects
### of this factor at every level of the factor time, i.e
### at every moment of the diet: beginning, middle, end

### concretely, they consist of three differences

### weight with physical exercises - weight without physical exercises, at the beginning of the diet
### weight with physical exercises - weight without physical exercises, in the middle of the diet
### weight with physical exercises - weight without physical exercises, at the end of the diet

### we will evaluate the first difference only (at the beginning of the diet)

### from the diet data frame, we extract the columns we need

diet2 <- diet[,c("weight_beg", "weight_beg_ex")]

View(diet2)

### create the dataframe with the levels of the factor exercise

xr <- c("no", "yes")

xr_frm <- data.frame(xr)

View(xr_frm)

### create a matrix with the columns of the data frame diet2

xr_mat <- cbind(diet2$weight_beg, diet2$weight_beg_ex)

### create the linear model to get the means of the dependent variables

model <- lm(xr_mat~1)

#### create the within-subjects model

require(car)

model2 <- Anova(model, idata=xr_frm, idesign=~xr, type="III")

summary(model2, multivariate=F)


######## get the simple Tukey comparisons
######## to find out how big the difference is

### reshape the initial data set

require(reshape2)

dietm <- melt(diet2)

View(dietm)

### give the columns some suggestive names

colnames(dietm) <- c("group", "weight")

### build an ANOVA model

model3 <- aov(weight~group, data=dietm)

### compute the paired comparison tests

TukeyHSD(model3)

### the same procedure is to be applied for the other two differences


