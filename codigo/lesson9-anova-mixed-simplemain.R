diet = read.csv("diet3.csv")

View(diet)

#########
### the mixed analysis of variance - simple main effects
#########

### the simple main effects of the variable time represent
### the mean differences of weight between the three moments of the diet
### for each gender separately

### to compute them, we will run a within-subjects ANOVA for each gender category

######## for the male subjects

### create a new data frame with the male subjects only 

dietm <- diet[diet$gender=="male",]

### build a dataframe with the levels of the factor time

moments <- c("beginning", "middle", "end")

moments_frm <- data.frame(moments)

View(moments_frm)

### build a matrix with the values of the measure (weight)

weight_male <- cbind(dietm$weight_beg, dietm$weight_mid, dietm$weight_end)

#### get the means of the dependent variables

model <- lm(weight_male~1)

### now do the within-subjects analysis

model2 <- Anova(model, idata = moments_frm, idesign = ~moments, type="III")

summary(model2, multivariate=F)

### the same procedure will be used for the female subjects

#######################

### the simple main effects of the variable gender represent
### the mean differences of weight between the male and female subjects
### for each moment of the diet: beginning, middle, end

### they consist of three pairs of differences

### average male weight - average female weight, at the beginning of the diet
### average male weight - average female weight, in the middle of the diet
### average male weight - average female weight, at the end of the diet

### we will evaluate these differences using the independent sample t test

### the first difference (beginning of the diet)

t.test(diet$weight_beg~diet$gender, var.equal=T)

### the second difference (middle of the diet)

t.test(diet$weight_mid~diet$gender, var.equal=T)

### the third difference (end of the diet)

t.test(diet$weight_end~diet$gender, var.equal=T)


