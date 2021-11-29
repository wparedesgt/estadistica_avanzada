diet = read.csv("diet1.csv")

View(diet)

#########
### the Friedman test
#########

### we will compare the median weights at the three moments of the diet
### using the Friedman test

### create a matrix from the dataframe

weight <- cbind(diet$weight_beg, diet$weight_mid, diet$weight_end)

#### apply the friedman.test function to the matrix

friedman.test(weight)


