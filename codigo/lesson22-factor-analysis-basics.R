brd <- read.csv("brandsurvey.csv")

View(brd)

#########
### how to perform an exploratory factor analysis
#########

###### run a principal component analysis first 
###### to find the best number of factors

pcamodel <- princomp(brd, cor=T)

### Benzecri criterion
### proportion of explained variance should be at least 70%

summary(pcamodel)

### Kaiser criterion
### the eigenvalue should be higher than one

### compute the eigenvalues

eigenv <- pcamodel$sdev^2

print(eigenv)

### Evrard criterion
### visual inspection of the scree plot

### get the scree plot

screeplot(pcamodel, type="line")

### based on the pca results we decide to retain three factors

######## run the factor analysis with the varimax rotation
######## and print the factor matrix, with a 0.3 cutoff

model <- factanal(brd, factors = 3, rotation = "varimax")

print(model, digits=2, cutoff=.3, sort=TRUE)

### compute the communalities

comm <- 1 - model$uniquenesses

print(comm)

