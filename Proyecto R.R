#Proyecto Daniel Lobo. Modulo II

#Cargamos librerías
library (dplyr)
library (ggplot2)

#Fijamos el directorio de trabajo
setwd("/Users/danieloboc/Library/Mobile Documents/com~apple~CloudDocs/DATA ANALYTICS/MODULO 2/Proyecto R")

#Leemos el dataset que vamos a utilizar y lo guardamos como "Alturas" y comprobamos estructura
Heights = read.csv("Height of Male and Female by Country 2022.csv") 
str(Heights)

#Seleccionamos solo las columnas que vamos a utilizar
Heights = select(Heights, Country.Name:Female.Height.in.Cm)

#Obtenemos summary de cada columna
summary(Heights)

#Le cambiamos el nombre a las columnas
Heights <- rename(Heights, Country = Country.Name, MaleHeight = Male.Height.in.Cm, FemaleHeight = Female.Height.in.Cm)
names(Heights)

#Obtenemos los 6 países rankeados con mayor altura
head(Heights)

#Obtenemos los 6 países rankeados con menor altura
tail(Heights)

#Obtenemos las estaturas de Mexico
filter(Heights, Country == "Mexico") # Seleccionamos sólo a México

#Obtenemos histograma de estaturas de hombres
Heights %>%
  ggplot() + 
  aes(MaleHeight) +
  geom_histogram(binwidth = 3, col="black", fill = "gray") +
  ggtitle("Histograma de Estatura Promedio de Hombres") +
  ylab("Paises") +
  xlab("Estatura Hombres en CM") + 
  geom_density(aes(y = 3*..count..))+
  geom_vline(xintercept = mean(Heights$MaleHeight), linetype="dashed", color = "black") + 
  theme_gray()

#Obtenemos histograma de estaturas de mujeres
Heights %>%
  ggplot() + 
  aes(FemaleHeight) +
  geom_histogram(binwidth = 3, col="black", fill = "gray") +
  ggtitle("Histograma de Estatura Promedio de Mujeres") +
  ylab("Paises") +
  xlab("Estatura Mujeres en CM") + 
  geom_density(aes(y = 3*..count..))+
  geom_vline(xintercept = mean(Heights$FemaleHeight), linetype="dashed", color = "black") + 
  theme_gray()

#Metemos las columnas de alturas de hombres y mujeres como variables HM y HF
MH = Heights$MaleHeight
FH = Heights$FemaleHeight

#Obtenemos varianza, desviacion estandar, media y mediana y las metemos en variables
varMH = var(MH);varFH = var(FH)
sdMH = sd(MH);sdFH = sd(FH)
meanMH = mean(MH);meanFH = mean(FH)
medianMH = median(MH);medianFH = median(FH)

#Definimos intervalos de ejes X
x1 = seq(160, 184,1) #Hombres
x2 = seq(150,171,1) #Mujeres

#Obtenemos densidad de probabilidad normal para estatura en hombres
y1 = dnorm(x1, mean = meanMH , sd = sdMH)
plot(x1, y1, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 173, " y ", sigma == 5)))
abline(v = 173, lwd = 2, lty = 2) # La media es 173

#Probabilidad de que un hombre mida 178 o menos
pnorm(q = 178, mean = 173, sd = 5)
polygon(c(min(x1), x1[x1<=178], 178), c(0, y1[x1<=178], 0), col="blue")
abline(v = 173, lwd = 2, lty = 2) # La media es 173

#Obtenemos densidad de probabilidad normal para estatura en mujeres
y2 = dnorm(x2, mean = meanFH , sd = sdFH)
plot(x2, y2, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 161, " y ", sigma == 4)))
abline(v = 161, lwd = 2, lty = 2) # La media es 161

#Probabilidad de que una mujer mida 165 o mas
pnorm(q = 165, mean = 161, sd = 4, lower.tail = FALSE)
polygon(c(165, x2[x2>=165], max(x2)), c(0, y2[x2>=165], 0), col="pink")


