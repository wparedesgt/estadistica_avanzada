brd <- read.csv("brandsurvey.csv")

View(brd)

#########
### exploratory factor analysis - adequacy tests
#########

### how to compute the Kaiser-Meier-Olkin measure 
### and the Bartlett's sphericity test

### get the correlation matrix for our variables

corm <- cor(brd)

View(corm)

#### compute the KMO indicator

require(psych)

KMO(corm)

### alternatively

KMO(brd)

### compute the Bartlett's test

cortest.bartlett(corm, 106)   

### 106 is the sample size (number of respondents)
### 100 is the default
### the sample size must be specified only when the argument is a matrix

### alternatively

cortest.bartlett(brd)

