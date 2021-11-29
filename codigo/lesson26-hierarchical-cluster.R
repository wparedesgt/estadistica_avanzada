car <- read.csv("cars.csv")

View(car)

#########
### how to perform a hierarchical cluster analysis
#########

### we will cluster the car models by their characteristics:
### price, engine displacement, power, fuel consumption, maximum speed

### create a new data set with the clustering variables

car2 <- cbind(car$price, car$engine, car$power, car$fuelcons, car$speed)

View(car2)

### add column names and (important!) row names

colnames(car2) <- c("price", "engine", "power", "fuelcons", "speed")

rownames(car2) <- car$carmodel

### compute the distance matrix

dm <- dist(car2, method = "euclidean")

### create the clustering model

model <- hclust(dm, method = "ward.D")

### plot the model (as a dendrogram)

plot(model, labels=rownames(car2))

### get cluster membership

cutree(model, k=2:4)

### visualize clusters on the dendrogram

rect.hclust(model, k=5, border="red")


