#Within -withing Analisis de la varianza

#Asunsciones basicas:

#las variables son aproximadamente normalmente distribuidas
#las variables no presentan datos atipicos significativos
#tienen esfericidad

#Variable dependiente: weight
#Factores: time (biginning, middle, end) y ejercicio fisico (con y sin ejercicio)

#Preparando el dataframe con los niveles de los factores combinados


library(tidyverse)
library(car)
library(reshape2)

fact <- read.csv("data/factors-within-within.csv")
diet <- read.csv("data/diet2.csv")

fact$Exercise <- as.factor(fact$Exercise)
fact$Time <- as.factor(fact$Time)



str(fact)


###Crear una matriz con todas las variables dependientes

weight_01 <- cbind(diet$weight_beg, diet$weight_mid, diet$weight_end, 
                   diet$weight_beg_ex, diet$weight_mid_ex, diet$weight_end_ex)

#Obteniendo los promedios de las variables dependientes

model <- lm(weight_01~1)
summary(model)

#Corriendo el ANOVA


model_02 <- Anova(model, idata = fact, idesign =~Exercise*Time, type = "III")

summary(model_02, multivariate = FALSE) # Le pusimos falso porque no necesitamos los resultados MANOVA


#evaluando peso con ejercicio fisico - peso sin ejercicio fisico en cada uno de los estados de la dieta.

diet2 <- diet[,c("weight_beg", "weight_beg_ex")]

view(diet2)

#Creamos el dataframe con los niveles del ejercicio de factores.

xr <- c("no", "yes")

xr_frm <- data.frame(xr)
View(xr_frm)


##Crear la matriz con elas columnas del dataframe diet2

xr_mat <- cbind(diet2$weight_beg, diet2$weight_beg_ex)

#Creamos el modelo linear para obtener el promedio de las variables dependientes.

model03 <- lm(xr_mat~1)

#Creamos el modelo entre los sujetos

xr_frm$xr <- as.factor(xr_frm$xr) #Convertimos en factor para que no de error


model04 <- Anova(model03, idata = xr_frm, idesign = ~xr, type = "III")

summary(model04, multivariate = FALSE) #Esta en falso porque no necesitalos los resultados de MANOVA


#Obteniendo una comparacion simple de Tukey para encontrar cuan grandes son las diferencias.


dietm <- melt(diet2)

#Dando a las columnas nombres sugestivos

colnames(dietm) <- c("group", "weight")

#Construyendo el modelo ANOVA

model05 <- aov(weight~group, data = dietm)

#Calculando el emparejamiento en los test de comparación

TukeyHSD(model05)

#El mismo procedimiento puede ser aplicado a las otras dos diferencias

#Caclulando las direcencias entre el peso al inicio y a mediados de la dieta con y sin ejercicio.



