diet <- read.csv("diet1.csv")

View(diet)

#########
### the within-subjects analysis of variance - multiple comparisons
#########

### to perform the multiple (paired) comparisons
### we must reshape the data frame first
### (put it in the "long data" format)

require(reshape2)

dietm <- melt(diet)

View(dietm)

### give the columns some suggestive names

colnames(dietm) <- c("group", "weight")

### build an ANOVA model

model <- aov(weight~group, data=dietm)

### perform the Tukey test

TukeyHSD(model)

### perform the Bonferroni paired comparisons

pairwise.t.test(dietm$weight, dietm$group, p.adjust.method = "bonferroni")




