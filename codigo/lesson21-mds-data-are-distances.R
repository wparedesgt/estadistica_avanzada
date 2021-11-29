dest <- read.csv("destinations.csv")

View(dest)

#########
### how to perform the multidimensional scaling procedure
### when the data ARE distances between objects
#########

### we will generate two "hidden attributes" for our destinations,
### compute the destinations coordinates on each attribute
### and draw a perceptual map

### the dest data frame is already a matrix
### so we can use it as an argument in the cmdscale function

model <- cmdscale(dest, k=2)

print(model)

### store the coordinates as a data frame

coord <- as.data.frame(model)

View(coord)

### add the destination names and name the data frame columns

coord <- cbind(coord, c("A", "B", "C", "D", "E", "F", "G"))

colnames(coord) <- c("attribute1", "attribute2", "destination")

### plot the map with labels

require(ggplot2)

ggplot()+geom_point(data=coord, aes(x=attribute1, y=attribute2), color="gray")+
  geom_text(data=coord, aes(x=attribute1, y=attribute2, label=destination, size=10))





