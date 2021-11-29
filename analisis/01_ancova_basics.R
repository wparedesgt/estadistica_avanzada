#Analisis de una via de la covarianza

library(tidyverse)
library(car)
library(effects)
library(multcomp)


vit <- read.csv("data/vitamin-a.csv")

view(vit)


# Asumciones Basicas

  #La variable de respuesta no presenta valores atipicos.
  #La relacion entre la variable dependiente y la covariable es linear.
  #No existe relacion entre la covarianza y el factor.  *
  #Los residuales de la variable de respeuesta son de distribucion normal. *
  #Existe homogenidad en la varianzas. *
  #Existe homoskedasticity

#Alas asumciones estan marcadas con asterisco.

#Variable dependiente: effort resistance
#Factor: Dose of vitamin
#Covarianza: employees 'age


#Run ONCOVA

model <- aov(effort ~ dose + age, data = vit)

ancova <- Anova(model, type = "III")
ancova


#Calculando la media ajustada

effect("dose", model)

#Multiples comparaciones entre medias ajustadas

model$model$dose <- as.factor(model$model$dose)  #Arreglando el modelo con un factor
mcomp <- glht(model, linfct=mcp(dose = "Tukey"))

#Sumando as diferencias estadisticamente significativas


summary(mcomp)

#obteniendo el intervalo de confidencia

confint(mcomp)


#Se calcula la interaccion entre edad y dosis de vitaminas

model <- aov(effort~age*dose, data = vit)
av <- Anova(model, type = "III")
av


