diet <- read.csv("diet1.csv")

View(diet)

#########
### the within-subjects (repeated measures) analysis of variance
#########

#########
### Basic assumptions:

# the variables are approximately normally distributed
# the variables do not present significant outliers
# there is sphericity*

### we will check only the assumptions marked with an asterisk (*)
##########

### we will determine whether there is a significant difference
### between the average subjects' weights at the three diet moments:
### beginning, middle, end

### dependent variable: weight (measured three times)
### factor: time

###### running the within-subjects ANOVA supposes several steps

### build a matrix and a dataframe with the factor levels

moments_mat <- c("beginning", "middle", "end")

print(moments_mat)

moments_frm <- data.frame(moments_mat)

View(moments_frm)

### build a matrix with the values of the measure (weight)

weight_mat <- cbind(diet$weight_beg, diet$weight_mid, diet$weight_end)

print(weight_mat)

#### get the means of the groups (these will be compared through the ANOVA)

model <- lm(weight_mat~1)

summary(model)

### now do the within-subjects analysis

require(car)

model2 <- Anova(model, idata = moments_frm, idesign = ~moments_mat, type="III")

### the options idata and idesign are used to define the factor levels
### in the repeated-measure analyses

summary(model2, multivariate=F)

### the option multivariate = F prevents the display of the MANOVA results


