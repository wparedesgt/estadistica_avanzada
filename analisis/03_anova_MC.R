#Anova con multiples componentes

library(tidyverse)
library(car)
library(reshape2)
library(effects)
library(multcomp)

diet <- read_csv("data/diet1.csv")

#analisis de la varianza con multiples comparaciones


diet_m <- melt(diet)  #Cambiando a datos largos o (pivoteando)

#dando a las columnas nombres sugestivos

colnames(diet_m) <- c("group", "weight")

#Construyendo un modelo ANOVA

model_aov <- aov(weight~group, data = diet_m)

# rendimiento test Tukey

TukeyHSD(model_aov)

#Rendimiento Bonferronu comparacion pareja

pairwise.t.test(diet_m$weight, diet_m$group, p.adjust.method = "bonferroni")
