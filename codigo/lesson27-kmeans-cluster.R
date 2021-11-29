ctr <- read.csv("countries.csv")

View(ctr)

#########
### how to perform a k-means cluster analysis
#########

### we will cluster the countries by their
### demographic and economic characteristics

####### data preparation

### remove the missing values

ctr <- na.omit(ctr)

### create a matrix with all the clustering variables

ctr2 <- cbind(ctr$urban, ctr$flexp, ctr$mlexp, ctr$literacy, ctr$infmort, ctr$gdp, ctr$density, ctr$popincr)

View(ctr2)

### standardize the clustering variables (recommended)

ctr2 <- scale(ctr2)

### name the rows and the columns

rownames(ctr2) <- ctr$country

colnames(ctr2) <- c("urban", "flexp", "mlexp", "literacy", "infmort", "gdp", "density", "popincr")

######### run the k-means algorithm, with three clusters

model <- kmeans(ctr2, 3)

print(model)

###### get some relevant information

### clustering vector

model$cluster

### size of clusters

model$size

### cluster centers

model$centers

####### sums of squares

### total sum of squares

model$totss

### within-cluster sum of squares

model$withinss

### between-cluster sum of squares, i.e. totss-tot.withinss

model$betweenss

### total within-cluster sum of squares, i.e. sum(withinss)

model$tot.withinss

##########################

###### plot the clusters and their centers (scatterplot chart)
###### in a two dimensional space
###### the axes will be literacy and gross domestic product (gdp)

### put the cluster centers in a data frame

centers <- data.frame(model$centers)

View(centers)

### convert the ctr2 matrix into a data frame

ctr3 <- data.frame(ctr2)

View(ctr3)

### build the scatterplot

require(ggplot2)

ggplot()+geom_point(data=ctr3, aes(x=literacy, y=gdp, color = model$cluster))+
  geom_point(data=centers, aes(x=literacy[1], y=gdp[1], size=10), color="red", shape=8)+
  geom_point(data=centers, aes(x=literacy[2], y=gdp[2], size=10), color="green", shape=8)+
  geom_point(data=centers, aes(x=literacy[3], y=gdp[3], size=10), color="magenta", shape=8)+
  theme(legend.position="none")

################ we can put labels to the centers

ggplot()+geom_point(data=ctr3, aes(x=literacy, y=gdp, color = model$cluster))+
  geom_point(data=centers, aes(x=literacy[1], y=gdp[1], size=10), color="red", shape=8)+
  geom_text(data=centers, aes(x=literacy[1], y=gdp[1]-0.09, label="Cluster 1"))+
  geom_point(data=centers, aes(x=literacy[2], y=gdp[2], size=10), color="green", shape=8)+
  geom_text(data=centers, aes(x=literacy[2], y=gdp[2]-0.09, label="Cluster 2"))+
  geom_point(data=centers, aes(x=literacy[3], y=gdp[3], size=10), color="magenta", shape=8)+
  geom_text(data=centers, aes(x=literacy[3], y=gdp[3]-0.09, label="Cluster 3"))+
  theme(legend.position="none")

