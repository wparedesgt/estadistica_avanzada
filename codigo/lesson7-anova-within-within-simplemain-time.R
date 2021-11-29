diet = read.csv("diet2.csv")

View(diet)

#########
### the simple main effects of the factor time
#########

### the simple main effects of the factor time represent the effects
### of this factor at every level of the factor exercise, i.e
### with and without physical exercises

### concretely, they consist of two differences

### the difference between the weight at the beginning, in the middle and at the end of the diet, WITHOUT exercises
### the difference between the weight at the beginning, in the middle and at the end of the diet, WITH exercises

### we already evaluated the first set of differences, in the lecture about within-subjects ANOVA

### now we will evaluate the second set

### build a dataframe with the levels of the factor time

moments <- c("beginning", "middle", "end")

moments_frm <- data.frame(moments)

View(moments_frm)

### build a matrix with the values of the measure (weight)

moments_mat <- cbind(diet$weight_beg_ex, diet$weight_mid_ex, diet$weight_end_ex)

#### get the means of the dependent variables

model <- lm(moments_mat~1)

### now do the within-subjects analysis

require(car)

model2 <- Anova(model, idata = moments_frm, idesign = ~moments, type="III")

summary(model2, multivariate=F)


############# get the Tukey pairwise comparisons
############# to see how big the differences are

### from the data frame diet, extract the columns we need

diet2 <- diet[,c("weight_beg_ex", "weight_mid_ex", "weight_end_ex")]

### reshape the new data frame

dietm <- melt(diet2)

View(dietm)

### give the columns some suggestive names

colnames(dietm) <- c("group", "weight")

### build an ANOVA model

model3 <- aov(weight~group, data=dietm)

### compute the paired comparison tests

TukeyHSD(model3)


