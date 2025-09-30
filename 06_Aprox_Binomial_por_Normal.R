########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################

########################################################


## Aproximación de la Binomial por la Normal


#' En la primera parte del TP se lanzó una moneda equilibrada 1 millón de veces, obteniéndose 
#' proporciones de caras y cecas cercanas al 50%-50%. En este ejercicio analizaremos qué tan 
#' probable es obtener resultados distintos a estos.
#' En principio, la cantidad de resultados "cara" en 1 millón de lanzamientos puede ser cualquier
#' número entre 0 y 1 millón. Dado que la probabilidad de que se obtenga "cara" en un lanzamiento 
#' en particular es la misma en todos los lanzamientos, la distribución de probabilidades de la 
#' cantidad de caras obtenidas se puede representar como una variable Binomial con parámetros 
#' n=1000000 y p=0.5, ya que la moneda es equilibrada. Sin embargo, dado el alto valor de 'n', 
#' puede ser dificultoso calcular la distribución binomial. Como se vio en clase, la Distribución 
#' Normal puede dar una buena aproximación de la distribución binomial cuando se cumple que np>10 
#' y nq>10. 

#' ¿Se cumplen los supuestos para el caso de moneda equilibrada?


###### Ejecutar desde aqui #################################
library(tidyverse)
n <- 1000000
p_eq <- 0.5
paste("np = ",n*p_eq)
paste("nq = ",n*(1-p_eq))
###### Hasta aqui ##########################################


#' ¿Cuál es la media y el desvío de la distribución normal utilizada para
#' aproximar la distribución de la cantidad de caras obtenidas con la moneda equilibrada?


###### Ejecutar desde aqui #################################
paste('La media es:', n*p_eq)
paste('El desvío es:', sqrt(n*p_eq*(1-p_eq)))
###### Hasta aqui ##########################################


#' Realizar el gráfico de esta distribución


###### Ejecutar desde aqui #################################
data.frame(caras=c(497500:502500), 
           Probabilidad=dnorm(c(497500:502500),mean=n*p_eq, sd=sqrt(n*p_eq*(1-p_eq)))) %>%
  ggplot(aes(x=caras, y=Probabilidad)) +
  geom_line() +
  theme_classic() +
  ggtitle("Distribución de probabilidad para la moneda equilibrada") +
  theme(panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.major.x = element_line(linetype = "dotted"),
        panel.background = element_blank(),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' ¿Cuál es la probabilidad de que se obtenga más de un 50.3% de caras?
#' Es decir, más de 503000 caras del millón de lanzamientos
#' Utilice la distribución Normal para aproximar la binomial e interprete los resultados.


###### Ejecutar desde aqui #################################
p_aproximada <- 1-pnorm(503000,mean=n*p_eq, sd=sqrt(n*p_eq*(1-p_eq)))
p_aproximada
###### Hasta aqui ##########################################


#' Si hubiese podido calcular la distribución Binomial, por ejemplo, usando R,
#' ¿Cuál sería la probabilidad exacta de que salgan más de 503000 caras del millón de lanzamientos


###### Ejecutar desde aqui #################################
p_exacta <- 1 - pbinom(503000, 1000000, prob=0.5)
p_exacta
###### Hasta aqui ##########################################


#' ¿Cuál es la diferencia entre la probabilidad aproximada por la normal y la exacta?


###### Ejecutar desde aqui #################################
p_exacta - p_aproximada
###### Hasta aqui ##########################################


#' ¿Considera que la aproximación mediante la Normal es razonable?

#' Sin embargo, recuerde que cuando se aproxima una variable discreta (binomial) mediante
#' una variable continua (normal) es recomendable aplicar un "factor de corrección".
#' En este caso, al hacer la aproximación con la normal tendríamos que haber calculado la
#' probabilidad de que la variable sea mayor a 503000 + 0.5.
#' ¿Cuál es esta probabilidad?


###### Ejecutar desde aqui #################################
p_aproximada_factor_correccion <- 1-pnorm(503000+0.5,mean=n*p_eq, sd=sqrt(n*p_eq*(1-p_eq)))
p_aproximada_factor_correccion
###### Hasta aqui ##########################################


#' Una vez aplicado el factor de corrección ¿Cuál es la diferencia entre la probabilidad 
#' aproximada por la normal y la exacta?


###### Ejecutar desde aqui #################################
p_exacta - p_aproximada_factor_correccion
###### Hasta aqui ##########################################


#' La aplicación del factor de corrección, ¿mejoró o empeoró la aproximación?


#' ¿Cuál es la probabilidad de que se obtenga menos de un 50.1% de caras?
#' Es decir, menos de 501000 caras del millón de lanzamientos
#' Utilice la distribución Normal para aproximar la binomial, probando tanto con o sin el 
#' factor de corrección para comparar los resultados. Interprete los resultados.

# Sin factor de corrección
###### Ejecutar desde aqui #################################
p_aproximada <- pnorm(501000,mean=n*p_eq, sd=sqrt(n*p_eq*(1-p_eq)))
p_aproximada
###### Hasta aqui ##########################################


# Con factor de corrección
###### Ejecutar desde aqui #################################
p_aproximada_factor_correccion <- pnorm(501000-0.5,mean=n*p_eq, sd=sqrt(n*p_eq*(1-p_eq)))
p_aproximada_factor_correccion
###### Hasta aqui ##########################################


#' Si hubiese podido calcular la distribución Binomial, por ejemplo, usando R, 
#' ¿Cuál sería la probabilidad exacta de que salgan menos de 501000 caras del millón de lanzamientos


###### Ejecutar desde aqui #################################
p_exacta <- pbinom(501000-1, 1000000, prob=0.5)
p_exacta
###### Hasta aqui ##########################################

# ¿Cuál es la diferencia entre la probabilidad exacta y ambas aproximaciones?


###### Ejecutar desde aqui #################################
p_exacta - p_aproximada
p_exacta - p_aproximada_factor_correccion
###### Hasta aqui ##########################################


#' La aplicación del factor de corrección, ¿mejoró o empeoró la aproximación?


#' ¿Cuál es la probabilidad de que se obtenga entre 49,99% y 50,01% de caras?
#' Es decir, entre 499900 y 500100 caras en el millón de lanzamientos.
#' Utilice la distribución Normal para aproximar la binomial, 
#' probando tanto con o sin el factor de corrección para comparar los resultados.
#' Interprete los resultados.


###### Ejecutar desde aqui #################################
p_aproximada <- pnorm(500100,mean=n*p_eq, sd=sqrt(n*p_eq*(1-p_eq))) - pnorm(499900,mean=n*p_eq, sd=sqrt(n*p_eq*(1-p_eq))) 
p_aproximada_factor_correccion <-  pnorm(500100+0.5,mean=n*p_eq, sd=sqrt(n*p_eq*(1-p_eq))) - pnorm(499900-0.5,mean=n*p_eq, sd=sqrt(n*p_eq*(1-p_eq)))
p_aproximada
p_aproximada_factor_correccion
###### Hasta aqui ##########################################


#' Si hubiese podido calcular la distribución Binomial, por ejemplo, usando R, 
#' ¿Cuál sería la probabilidad exacta de que salgan entre 499900 y 500100 caras del millón de lanzamientos


###### Ejecutar desde aqui #################################
p_exacta <-  pbinom(500100, 1000000, prob=0.5) - pbinom(499899, 1000000, prob=0.5)
p_exacta
###### Hasta aqui ##########################################


# ¿Cuál es la diferencia entre la probabilidad exacta y ambas aproximaciones?


###### Ejecutar desde aqui #################################
p_exacta - p_aproximada
p_exacta - p_aproximada_factor_correccion
###### Hasta aqui ##########################################


#' La aplicación del factor de corrección, ¿mejoró o empeoró la aproximación?

