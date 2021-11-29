toyo <- read.csv("toyota.csv")

View(toyo)

#########
### how to perform a simple correspondence analysis
#########

### our goal is to determine the models that sell better on each continent

### we will create a perceptual map with the profiles (categories) of the two variables:
### car model and continent

### create a contingency table with our variables

tt <- xtabs(~model+continent, data=toyo)

print(tt)

###### run the analysis and display the results

require(ca)

model <- ca(tt)

summary(model)

### the eigenvalues show the proportion of the total variance explained by each axis (attribute)

### mass - proportion of each row/column in the total

### inr (inertia) shows the amount of variance each row/column 
### accounts for the total inertia value

### mass, inr (inertia) and qlt (quality) are multiplied by 1000
### so are cor (squared correlations)

### k=1 and k=2: coordinates of each profile on each dimension
### quality = cor(k=1) + cor(k=2)


############# prepare the data in order to plot the profiles

### get the row and column coordinates

rco <- ca(tt)$rowcoord   ## coordinates of the car models

cco <- ca(tt)$colcoord   ## coordinates of the continents

print(rco)

print(cco)

### put the coordinates in dataframes

rcodata <- data.frame(rco)

ccodata <- data.frame(cco)

View(rcodata)

View(ccodata)


### plot the coordinates with ggplot2

require(ggplot2)

ggplot()+
  geom_point(data=rcodata, aes(x=Dim1, y=Dim2), size=3, color="red", shape=16)+
  geom_point(data=ccodata, aes(x=Dim1, y=Dim2), size=3, color="blue", shape=16)+
  geom_text(data=rcodata, aes(x=Dim1, y=Dim2-0.07, label=rownames(rcodata), size=16))+
  geom_text(data=ccodata, aes(x=Dim1, y=Dim2-0.07, label=rownames(ccodata), size=16))+
  geom_hline(yintercept = 0, colour = "black")+
  geom_vline(xintercept = 0, colour = "black")+
  theme(legend.position="none")






