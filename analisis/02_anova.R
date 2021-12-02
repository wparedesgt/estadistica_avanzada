#Anova 02

library(tidyverse)
library(car)


diet <- read_csv("data/diet1.csv")

#Conviertiendo en factor

#diet$weight_beg <- as.factor(diet$weight_beg)
#diet$weight_mid <- as.factor(diet$weight_mid)
#diet$weight_end <- as.factor(diet$weight_end)

#Asunciones Basicas

# Las variables son aprixmadamente de distribucion normal
# las Variables no presentan datos atipicos significantes
#Tiene spericidad


view(diet)


#Determinar si hay diferencias en el peso.
#entre los sujetos en los tres momentos de la dieta
#Inicio a mediados y al finalizar



#Variable dependiente: Peso medido en tres etapas
#Factor variable independiente:  Tiempo  (inicio medio y final)

#Construyendo una matriz y dataframe con factores en los tres momentos.


moments_mat <- as.factor(c("Inicio", "Intermedio", "Fin")) #Convertir estos en factores

moments_frm <- data.frame(moments_mat)

#Construir la matris con los valores a medida (peso)

weight_mat <- cbind(diet$weight_beg, diet$weight_mid, diet$weight_end)

head(weight_mat)

#Obteniendo los promedios de los grupos (este se comprara con el modelo ANOVA)

model_lm <- lm(weight_mat~1)

summary(model_lm)

#Comenzar con el analisis

#model_lm  

modelo_02 <- Anova(model_lm, idata = moments_frm, idesign = ~moments_mat, type = "III")

#Las opciones idata y idesigno son usadas para definir los niveles de los factores.
#en el repeated-measure analysis 

summary(modelo_02, multivariate = FALSE)




