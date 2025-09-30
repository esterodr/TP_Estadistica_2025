########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################



########################################################

##' La distribución Normal

##' Este ejercicio es una adaptación del incluido en el libro OpenIntro Statistics.
##' La versión original y en inglés del ejercicio se puede consultar en el siguiente vínculo:
##' http://htmlpreview.github.io/?https://github.com/andrewpbray/oiLabs-base-R/blob/master/normal_distribution/normal_distribution.html

##' En este ejercicio se analiza la distribución de probabilidad más relevante en Estadística:
##' *la distribución Normal*. 
##' Utilizaremos datos de dimensiones del cuerpo humano. Se utilizará un set de datos que contiene
##' información de 247 hombres y 260 mujeres, todos considerados adultos saludables.

##' La base tiene 25 variables (puede verse tipeando head(bdims)), sobre las que se puede encontrar
##' una descripción en el siguiente link: https://www.openintro.org/stat/data/bdims.php
##' Aquí sólo nos centraremos en la altura en centímetros y el sexo

## Cargamos y realizamos una preparación previa de los datos


###### Ejecutar desde aqui #################################
library("tidyverse")
load("./archivos/bdims.RData") # Cargamos los datos
#Cambiemos el nombre de las variables para facilitar la interpretación
bdims <- bdims %>% select(sex, hgt, wgt)
names(bdims) <- c("Sexo", "Altura", "Peso")
# Recodificamos la variable sexo con los valores hombre y mujer en lugar de 0 y 1
bdims$Sexo <- as.character(bdims$Sexo)
bdims$Sexo[bdims$Sexo=="0"] <- "Mujer"
bdims$Sexo[bdims$Sexo=="1"] <- "Hombre"
###### Hasta aqui ##########################################


##' Comencemos construyendo un histograma de la altura de hombres y otro para las mujeres


###### Ejecutar desde aqui #################################
bdims %>% ggplot() +
  geom_histogram(aes(x=Altura),binwidth = 5, fill="cyan", color="black") +
  ylab("Cantidad de Personas") +
  xlab("Altura en centímetros") +
  facet_wrap(.~Sexo, nrow = 2) +
  theme_classic() +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted"),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


##' ¿Qué diferencias observa entre hombres y mujeres?

##' Notar que ambas distribuciones tienen forma de campana y son aproximadamente simétricas.
##' ¿Podemos decir que siguen distribuciones aproximadamente normales?
##' Para una primera aproximación, graficaremos encima de cada histograma una distribución Normal.
##' Esta distribución Normal deberá tener la misma media y desvío standard que los datos originales.
##' Empecemos con el caso de las mujeres:


###### Ejecutar desde aqui #################################
mujeres <- bdims %>% filter(Sexo=="Mujer")
altura_media_mujeres <-  mean(mujeres$Altura)
sd_altura_mujeres <- sd(mujeres$Altura)
data.frame(Altura=mujeres$Altura,
           y=dnorm(x = seq(140,190,length.out = nrow(mujeres)),
                   mean = altura_media_mujeres, sd = sd_altura_mujeres)) %>% 
  ggplot() +
  geom_histogram(aes(x=Altura, y=..density..), bins = 8, fill="cyan", color="black") +
  geom_line(aes(x=seq(140,190,length.out = nrow(mujeres)), y=y), color="darkgreen", size=2) +
  theme_classic() +
  ggtitle("Histograma de la altura de las mujeres") +
  ylab("%") +
  theme(panel.grid.major.y = element_line(linetype = "dotted"),
        panel.background = element_blank(),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


##' Basado en este gráfico, ¿considera que la altura de las mujeres sigue una distribución aproximadamente
##' Normal?

##' Veamos el caso de los hombres


###### Ejecutar desde aqui #################################
hombres <- bdims %>% filter(Sexo=="Hombre")
altura_media_hombres <-  mean(hombres$Altura)
sd_altura_hombres <- sd(hombres$Altura)

data.frame(Altura=hombres$Altura,
           y=dnorm(x = seq(150,200,length.out = nrow(hombres)),
                   mean = altura_media_hombres, sd = sd_altura_hombres)) %>% 
  ggplot() +
  geom_histogram(aes(x=Altura, y=..density..), bins = 8, fill="cyan", color="black") +
  geom_line(aes(x=seq(150,200,length.out = nrow(hombres)), y=y), color="darkgreen", size=2) +
  theme_classic() +
  ggtitle("Histograma de la altura de los hombres") +
  ylab("%") +
  theme(panel.grid.major.y = element_line(linetype = "dotted"),
        panel.background = element_blank(),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


##' Basado en este gráfico, ¿considera que la altura de los hombres sigue una distribución aproximadamente
##' Normal?

##' Evaluar a ojo la forma del histograma para ver si los datos se distribuyen de manera Normal
##' puede ser útil, pero no siempre es posible determinar qué tan buena es la aproximación.
##' Un camino alternativo consiste en construir un gráfico "QQ", nombre que proviene de Quantile-Quantile,
##' (Quantile significa cuantil o percentil). El gráfico consiste en comparar si los percentiles de la
##' distribución que estamos analizando se corresponden con los de la distribución Normal.

##' A modo de ejemplo, el primer cuartil (percentil 25) de las alturas de las mujeres de la base de datos es:


###### Ejecutar desde aqui #################################
quantile(mujeres$Altura,0.25)
###### Hasta aqui ##########################################


##' Por su parte, si la población fuera exactamente normal, con media y desvíos iguales a los de nuestra base
##' de datos, el primer cuartil (percentil 25) debería ser:


###### Ejecutar desde aqui #################################
qnorm(0.25,mean=altura_media_mujeres,sd=sd_altura_mujeres)
###### Hasta aqui ##########################################


##' Notará que son valores bastante parecidos. El gráfico QQ consiste en realizar esta comparación para todos
##' los percentiles.

##' Veamos el gráfico para la altura de las mujeres


###### Ejecutar desde aqui #################################
qqnorm(mujeres$Altura)
qqline(mujeres$Altura)
###### Hasta aqui ##########################################


##' Si la distribución fuera exactamente Normal, cada circulito aparecería exactamente sobre la
##' línea recta. Si bien en este caso la correspondencia no es exacta, podemos ver que es una muy buena
##' aproximación, salvo en las colas de la distribución.

##' Veamos el gráfico QQ para la altura de los hombres


###### Ejecutar desde aqui #################################
qqnorm(hombres$Altura)
qqline(hombres$Altura)
###### Hasta aqui ##########################################


##' Al igual que para el caso de las mujeres, podemos concluir que la distribución Normal es una buena 
##' aproximación de la distribución de la altura de los hombres.
##' De esta forma, podemos utilizar todo lo que sabemos de la distribución Normal para responder preguntas
##' sobre la altura de las personas.

##' A partir de ahora nos enfocaremos en valores específicos de nuestro conjunto de datos y veremos
##' si la distribución normal produce estimaciones razonables de las probabilidades.

##' Suponiendo una distribución Normal, ¿Cuál es la probabilidad de que una mujer adulta seleccionada 
##' al azar mida más de 170 cm.?


# REALICE EL CÁLCULO ANALÍTICAMENTE, UTILICE LUEGO R PARA COMPROBAR LA RESPUESTA.
# Suponga una media de 164,87 cm. y un desvío de 6,5446 cm.


###### Ejecutar desde aqui #################################
1 - pnorm(q = 170, mean = altura_media_mujeres, sd = sd_altura_mujeres)
###### Hasta aqui ##########################################


##' En la base de datos utilizada, ¿cuál es la proporción de mujeres que mide más de 170 cm.?


###### Ejecutar desde aqui #################################
sum(mujeres$Altura > 170) / length(mujeres$Altura)
###### Hasta aqui ##########################################


##' ¿La proporción es cercana a la estimada mediante la distribución Normal? Interprete.

##' Suponiendo una distribución Normal, ¿Cuál es la probabilidad de que un hombre adulto seleccionado 
##' al azar mida más de 170 cm.?


# REALICE EL CÁLCULO ANALÍTICAMENTE, UTILICE LUEGO R PARA COMPROBAR LA RESPUESTA.
# Suponga una media de 177,75 cm. y un desvío de 7,1836 cm.


###### Ejecutar desde aqui #################################
1 - pnorm(q = 170, mean = altura_media_hombres, sd = sd_altura_hombres)
###### Hasta aqui ##########################################


##' En la base de datos utilizada, ¿cuál es la proporción de hombres que mide más de 170 cm.?


###### Ejecutar desde aqui #################################
sum(hombres$Altura > 170) / length(hombres$Altura)
###### Hasta aqui ##########################################


##' ¿La proporción es cercana a la estimada mediante la distribución Normal? Interprete.

