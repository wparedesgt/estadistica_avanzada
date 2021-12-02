#Within -withing Analisis de la varianza

#Asunsciones basicas:

#las variables son aproximadamente normalmente distribuidas
#las variables no presentan datos atipicos significativos
#tienen esfericidad

#Variable dependiente: weight
#Factores: time (biginning, middle, end) y ejercicio fisico (con y sin ejercicio)

#Preparando el dataframe con los niveles de los factores combinados


library(tidyverse)

diet <- read_csv("data/diet2.csv")

fact <- read_csv("data/factors-within-within.csv")


###Crear una matriz con todas las variables dependientes


weight_01 <- cbind(diet$weight_beg, diet$weight_mid, diet$weight_end, 
                   diet$weight_beg_ex, diet$weight_mid_ex, diet$weight_end_ex)

#Obteniendo los promedios de las variables dependientes

