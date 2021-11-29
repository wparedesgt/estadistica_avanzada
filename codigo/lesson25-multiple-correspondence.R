retail <- read.csv("retail.csv")

View(retail)

#########
### how to perform a multiple correspondence analysis
#########

### our goal is to determine whther there is an association
### between juice type, packaging and retail channel

### we will create a perceptual map with the profiles of the three variables

### run the analysis (with two dimensions)

require(MASS)

model <- mca(retail, nf=2)

print(model)

########## prepare the data to plot the profiles

### create the table with the column coordinates

mtable <- model$cs

print(mtable)

### convert the table into a data frame

ccodata <- data.frame(mtable)

### rename the rows and columns

colnames(ccodata) <- c("Dim1", "Dim2")

rownames(ccodata) <- c("Apple Juice", "Strawberry Juice", "Bottle", "Tetra Pak", "Convenience Store", "Supermarket")

View(ccodata)

### create the plot

require(ggplot2)

ggplot()+
  geom_point(data=ccodata, aes(x=Dim1, y=Dim2), size=3, color="red", shape=16)+
  geom_text(data=ccodata, aes(x=Dim1, y=Dim2, label=rownames(ccodata), size=16))+
  geom_hline(yintercept = 0, colour = "black")+
  geom_vline(xintercept = 0, colour = "black")+
  theme(legend.position="none")


