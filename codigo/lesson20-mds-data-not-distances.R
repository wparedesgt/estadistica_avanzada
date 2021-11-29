j <- read.csv("juices.csv")

View(j)

#########
### how to perform the multidimensional scaling procedure
### when the data are NOT distances between objects
#########

### we will generate two "hidden attributes" for our brands,
### compute the brands coordinates on each attribute
### and build a perceptual map

### transpose the data set to compute the distance between columns (brands)

j1 <- t(j)

### compute the distances between brands

prox <- dist(j1)

print(prox)

### we apply the cmdscale function to the new matrix prox
### requiring the program to generate two dimensions

model <- cmdscale(prox, k=2)

### this model contains the coordinates of each brand on the two dimensions

print(model)

######### preparing the data in order to plot the brands
######## in a two-dimensional space (i.e. build a perceptual map)

### make a data frame with the coordinates

coord <- as.data.frame(model)

View(coord)

### add a new column to the data frame coord, containing the brand names

coord <- cbind(coord, c("b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11"))

### rename the columns

colnames(coord) <- c("attribute1", "attribute2", "brand")

### plot the points (with labels)

require(ggplot2)

ggplot()+geom_point(data=coord, aes(x=attribute1, y=attribute2), color="gray")+
      geom_text(data=coord, aes(x=attribute1, y=attribute2, label=brand, size=1))


