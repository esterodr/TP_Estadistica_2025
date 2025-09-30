########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################


########################################################

##' Fundamentos de la inferencia estadística: Distribuciones Muestrales

##' Este ejercicio es una adaptación del incluido en el libro OpenIntro Statistics.
##' La versión original y en inglés del ejercicio se puede consultar en el siguiente vínculo:
##' http://htmlpreview.github.io/?https://github.com/andrewpbray/oiLabs-base-R/blob/master/sampling_distributions/sampling_distributions.html
##' 
##' El objetivo es analizar cómo los estadísticos de una muestra aleatoria pueden servir
##' como estimaciones puntuales de parámetros poblacionales. En particular, interesa formular
##' una *distribución muestral* de la estimación para aprender más acerca de las propiedades
##' de la misma.
 
##' Se utilizarán datos del mercado inmobiliario de la ciudad de Ames, Iowa, EEUU. En particular,
##' nos enfocaremos en todas las ventas de hogares ocurridas en esa ciudad entre 2006 y 2010.
##' Este conjunto de datos representa nuestra *población* de interés. Mediante pequeñas muestras
##' de esta población, intentaremos aprender más sobre estas transacciones inmobiliarias.

##' Lectura y carga de los datos:


###### Ejecutar desde aqui #################################
library("tidyverse")
load("./archivos/ames.RData")
###### Hasta aqui ##########################################


##' La base de datos contiene una gran cantidad de variables (puede explorar tipeando head(ames) en 
##' la consola). Para empezar, sólo nos centraremos en una de ellas: el área del hogar en pies cuadrados.
##' Guardamos esta variable en un objeto con nombre adecuado y convertimos el área del hogar
##' a metros cuadrados para facilitar su interpretación.


##' Daremos una mirada rápida a la distribución del área de los hogares calculando los principales
##' indicadores y construyendo el histograma

###### Ejecutar desde aqui #################################
area <- ames$Gr.Liv.Area*0.0929
summary(area)
###### Hasta aqui ##########################################


###### Ejecutar desde aqui #################################
data.frame(Area=area) %>%
  ggplot(aes(x=Area)) +
  geom_histogram(binwidth = 10, fill="cyan", color="black") +
  xlab("Área en metros cuadrados") +
  scale_y_continuous(breaks = seq(0,250,50)) +
  scale_x_continuous(breaks = seq(0,500,50)) +
  theme_classic() +
  ggtitle("Histograma del área de los hogares") +
  ylab("Frecuencia Absoluta") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


##' En este ejercicio tenemos acceso a la población completa, pero esto rara vez sucede en 
##' la vida real. Recolectar información sobre el total de la población suele ser extremadamente
##' costoso o incluso imposible. Por este motivo, se suele tomar una muestra de la población y
##' utilizarla para entender las propiedades de la misma.
##' 
##' Si estamos interesados en estimar el área promedio de los hogares en Ames, podemos empezar
##' tomando una muestra aleatoria de 50 hogares.
##' 
##' En base a esta muestra, nuestra mejor estimación puntual sobre el área promedio de los
##' hogares es el promedio de la muestra de 50 hogares:
##' 
##'Tome una muestra de 50 hogares y calcule el área promedio de esa muestra


###### Ejecutar desde aqui #################################
muestra1 <- sample(area, 50)
mean(muestra1)
###### Hasta aqui ##########################################


##' Dependiendo de cuáles fueron los 50 hogares incluidos en la muestra, el promedio estimado
##' estará un poco por encima o por debajo del verdadero promedio de la población . Sin embargo, 
##' la media muestral suele resultar un buen estimador del tamaño promedio de los hogares, dando 
##' un resultado no muy diferente al real y requiriendo para su cálculo menos del 3% de los 
##' hogares que conforman la población.

##' Tomemos otra muestra aleatoria, también de 50 hogares. ¿El promedio da muy distinto al de la primer muestra?


###### Ejecutar desde aqui #################################
muestra2 <- sample(area, 50)
mean(muestra2)
###### Hasta aqui ##########################################


##' Supongamos que tomamos dos muestras más, una de tamaño 100 y otra de 1000. ¿Cuál cree que
##' dará una estimación más precisa de la media poblacional? Justifique.

##' No debería sorprender que, cada vez que se toma una muestra aleatoria, se obtiene un área
##' promedio diferente. Es útil tomar dimensión de cuánta variabilidad debemos esperar cuando
##' estimamos la media poblacional mediante medias muestrales. La distribución de los promedios
##' muestrales, denominada *distribución muestral*, nos puede ayudar a entender esta variabilidad.
##' En este ejemplo, como tenemos acceso al total de la población, podemos construir la
##' distribución muestral repitiendo los pasos anteriores muchas veces. En particular,
##' vamos a generar 5000 muestras de 50 hogares y calcular la media para cada una de ellas.


###### Ejecutar desde aqui #################################
medias_muestrales_50 <- rep(NA, 5000)
for(i in 1:5000){
  muestra <- sample(area, 50)
  medias_muestrales_50[i] <- mean(muestra)
}
###### Hasta aqui ##########################################


##' Veamos el histograma de las 5000 medias muestrales


###### Ejecutar desde aqui #################################
data.frame(Media=medias_muestrales_50) %>%
  ggplot(aes(x=Media)) +
  geom_histogram(binwidth = 1, fill="cyan", color="black") +
  xlab("Área promedio (m2) de la muestra") +
  scale_y_continuous(breaks = seq(0,300,50)) +
  scale_x_continuous(breaks = seq(120,170,5)) +
  theme_classic() +
  ggtitle("Distribución muestral de la media del área de los hogares") +
  ylab("Frecuencia Absoluta") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


##' Aproximadamente, ¿cuál es el centro de esta distribución?

##' ¿Qué esperaría que ocurra si, en lugar de tomar 5000 muestras, toma 50000? Para 
##' comprobarlo, tome 50000 muestras, calcule la media muestral de cada una de ellas 
##' y presente el histograma de esta las mismas.


###### Ejecutar desde aqui #################################
##' Tomamos 50000 muestras de 50 hogares y calculamos la media
medias_muestrales_50_2 <- rep(NA, 50000)
for(i in 1:50000){
  muestra <- sample(area, 50)
  medias_muestrales_50_2[i] <- mean(muestra)
}
###### Hasta aqui ##########################################


###### Ejecutar desde aqui #################################
##' Veamos el histograma de las 50000 medias muestrales
data.frame(Media=medias_muestrales_50_2) %>%
  ggplot(aes(x=Media)) +
  geom_histogram(binwidth = 1, fill="cyan", color="black") +
  xlab("Área promedio (m2) de la muestra") +
  scale_y_continuous(breaks = seq(0,3000,500)) +
  scale_x_continuous(breaks = seq(120,170,5)) +
  theme_classic() +
  ggtitle("Distribución muestral de la media del área de los hogares") +
  ylab("Frecuencia Absoluta") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


##' ¿Se modificó algo en el gráfico?

##' La distribución muestral que acabamos de computar nos dice mucho acerca de la estimación
##' del área promedio de los hogares en Ames.
##' Como *la media muestral es un estimador insesgado*, la distribución muestral está centrada
##' en el verdadero promedio de la población, mientras que la dispersión de la distribución
##' indica cuánta variabilidad debemos esperar al tomar muestras de sólo 50 hogares.

##' Para tomar conciencia de lo anterior, construiremos dos distribuciones muestrales adicionales,
##' una basada en muestras de 10 hogares y la otra basada en muestras de 100 hogares.


###### Ejecutar desde aqui #################################
medias_muestrales_10 <- rep(NA, 5000)
medias_muestrales_100 <- rep(NA, 5000)
for(i in 1:5000){
  muestra <- sample(area, 10)
  medias_muestrales_10[i] <- mean(muestra)
  muestra <- sample(area, 100)
  medias_muestrales_100[i] <- mean(muestra)
} 
###### Hasta aqui ##########################################


###### Ejecutar desde aqui #################################
##' Grafiquemos las 3 distribuciones juntas para ver las diferencias
data.frame(Media=c(medias_muestrales_10, medias_muestrales_50, medias_muestrales_100),
           N = c(rep("1. Distribución de medias muestrales de tamaño 10",5000),
                 rep("2. Distribución de medias muestrales de tamaño 50",5000),
                 rep("3. Distribución de medias muestrales de tamaño 100",5000))) %>%
  ggplot(aes(x=Media)) +
  geom_histogram(binwidth = 1, fill="cyan", color="black") +
  xlab("Área promedio (m2) de la muestra") +
  scale_x_continuous(breaks = seq(90,210,10)) +
  theme_classic() +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE) +
  facet_wrap(.~N, nrow = 3)
###### Hasta aqui ##########################################


##' Cuando aumenta la cantidad de hogares, ¿qué sucede con el centro de la distribución? ¿y con la dispersión?


##' Repitamos el ejercicio pero ahora considerando el precio de las viviendas, expresado en dólares.


###### Ejecutar desde aqui #################################
##' Veamos indicadores e histograma de los precios
precio <- ames$SalePrice
summary(precio)
###### Hasta aqui ##########################################


###### Ejecutar desde aqui #################################
data.frame(Precio=precio) %>%
  ggplot(aes(x=Precio)) +
  geom_histogram(binwidth = 5000, fill="cyan", color="black") +
  xlab("Precio en dólares") +
  scale_y_continuous(breaks = seq(0,150,25)) +
  scale_x_continuous(breaks = seq(0,700000,100000)) +
  theme_classic() +
  ggtitle("Histograma del precio de los hogares") +
  ylab("Frecuencia Absoluta") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


##' Tomamos una muestra de 50 hogares y calculamos el precio promedio


###### Ejecutar desde aqui #################################
muestra1 <- sample(precio, 50)
mean(muestra1)
###### Hasta aqui ##########################################


##' Tomemos otra muestra aleatoria, también de 50 hogares. ¿Da muy distinto a la primer muestra?


###### Ejecutar desde aqui #################################
muestra2 <- sample(precio, 50)
mean(muestra2)
###### Hasta aqui ##########################################


##' vamos a generar 5000 muestras de 50 hogares y calcular el precio promedio para cada una de ellas.


###### Ejecutar desde aqui #################################
medias_muestrales_50 <- rep(NA, 5000)
for(i in 1:5000){
  muestra <- sample(precio, 50)
  medias_muestrales_50[i] <- mean(muestra)
}
###### Hasta aqui ##########################################


###### Ejecutar desde aqui #################################
##' Veamos el histograma de las 5000 medias muestrales
data.frame(Media=medias_muestrales_50) %>%
  ggplot(aes(x=Media)) +
  geom_histogram(binwidth = 2500, fill="cyan", color="black") +
  xlab("Precio en dólares") +
  scale_x_continuous(breaks = seq(0,225000,12500)) +
  theme_classic() +
  ggtitle("Distribución muestral de la media del precio de los hogares") +
  ylab("Frecuencia Absoluta") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


##' Aproximadamente, ¿cuál es el centro de esta distribución?

##' Construiremos dos distribuciones muestrales adicionales,
##' una basada en muestras de 10 hogares y la otra basada en muestras de 100 hogares.


###### Ejecutar desde aqui #################################
medias_muestrales_10 <- rep(NA, 5000)
medias_muestrales_100 <- rep(NA, 5000)
for(i in 1:5000){
  muestra <- sample(precio, 10)
  medias_muestrales_10[i] <- mean(muestra)
  muestra <- sample(precio, 100)
  medias_muestrales_100[i] <- mean(muestra)
} 
###### Hasta aqui ##########################################


###### Ejecutar desde aqui #################################
##' Grafiquemos las 3 distribuciones juntas para ver las diferencias
data.frame(Media=c(medias_muestrales_10, medias_muestrales_50, medias_muestrales_100),
           N = c(rep("1. Distribución de medias muestrales de tamaño 10",5000),
                 rep("2. Distribución de medias muestrales de tamaño 50",5000),
                 rep("3. Distribución de medias muestrales de tamaño 100",5000))) %>%
  ggplot(aes(x=Media)) +
  geom_histogram(binwidth = 2500, fill="cyan", color="black") +
  theme_classic() +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE) +
  facet_wrap(.~N, nrow = 3)
###### Hasta aqui ##########################################


##' Cuando aumenta la cantidad de hogares, ¿qué sucede con el centro de la distribución? ¿y con la dispersión?

