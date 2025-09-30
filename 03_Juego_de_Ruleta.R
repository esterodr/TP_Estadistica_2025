########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################




#' Simularemos una ruleta con números del 0 al 36. 
#' Todos los números tienen la misma probabilidad de salir.

#' Pruebe de jugar 10 veces seguidas y muestre los números que salieron:


###### Ejecutar desde aqui #################################
library(tidyverse)
ruleta <- c(0:36)
sample(ruleta,10,replace=TRUE)
###### Hasta aqui ##########################################

#' De ahora en más, apostará siempre al mismo número. 
#' Elija el número al que quiere apostar (del 0 al 36):
 

###### Ejecutar desde aqui #################################
numero_elegido <- 3     ### Poner cualquier número de 0 a 36
###### Hasta aqui ##########################################


##### Parte a)

#' Cada vez que juega tiene que pagar **1 peso** como entrada. En caso de ganar, recibe 
#' un premio de **36 pesos**. Es decir, en caso de ganar, obtendrá una ganancia 
#' neta de 36-1 = 35 pesos.

#' Jugaremos un millón de veces al número elegido

#' Calculamos los resultados (ganancia/pérdida) acumulados. ¿Cuál fue el resultado final?


###### Ejecutar desde aqui #################################
entrada <- 1
premio <- 36
repeticiones <- 1000000
resultados <- replicate(repeticiones, {
  x <- sample(ruleta,1,replace=TRUE)
  if (x==numero_elegido) {
    resultados <- premio-entrada
  } else {
    resultados <- entrada*(-1)
  }
})
resultados_acumulados_36 <- cumsum(resultados)
paste("El resultado final fue: ",resultados_acumulados_36[repeticiones])
###### Hasta aqui ##########################################

 
#' Graficamos (puede demorar unos segundos en aparecer el gráfico)


###### Ejecutar desde aqui #################################
data.frame(tirada=seq(1,repeticiones,25), resultado_acumulado=resultados_acumulados_36[seq(1,repeticiones,25)]) %>%
  ggplot() +
  geom_line(aes(x=tirada, y=resultado_acumulado)) +
  theme_classic() +
  ylab("Resultado Acumulado") +
  xlab("Número de juego") +
  ggtitle("Resultado acumulado de jugar\n1 millón de veces a la ruleta\n con premio=36") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Viendo los resultados, ¿Parece ser un juego equilibrado? ¿Conviene jugar o ser el Casino?


###### Parte b)

 
#' Jugaremos nuevamente a la ruleta. Cada vez que juega tiene que pagar **1 peso** como entrada. 
#' Pero esta vez, en caso de ganar, recibe un premio de **38 pesos** 
#' en lugar de los 36 de la ruleta anterior.


###### Ejecutar desde aqui #################################
entrada <- 1
premio <- 38
###### Hasta aqui ##########################################


#' Jugaremos un millón de veces al número elegido 
#' Calculamos los resultados acumulados. ¿Cuál fue el resultado final?


###### Ejecutar desde aqui #################################
repeticiones <- 1000000
resultados <- replicate(repeticiones, {
  x <- sample(ruleta,1,replace=TRUE)
  if (x==numero_elegido) {
    resultados <- premio-entrada
  } else {
    resultados <- entrada*(-1)
  }
})
resultados_acumulados_38 <- cumsum(resultados)
paste("El resultado final fue: ",resultados_acumulados_38[repeticiones])
###### Hasta aqui ##########################################

 
#' Graficamos (puede demorar unos segundos en aparecer el gráfico)


###### Ejecutar desde aqui #################################
data.frame(tirada=seq(1,repeticiones,25), resultado_acumulado=resultados_acumulados_38[seq(1,repeticiones,25)]) %>%
  ggplot() +
  geom_line(aes(x=tirada, y=resultado_acumulado)) +
  theme_classic() +
  ylab("Resultado Acumulado") +
  xlab("Número de juego") +
  ggtitle("Resultado acumulado de jugar\n1 millón de veces a la ruleta\n con premio=38 ") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################

 
#' Viendo los resultados, ¿Parece un juego equilibrado? ¿Conviene jugar o ser el Casino?

##### Parte c)

#' Jugaremos nuevamente a la ruleta, pero esta vez el premio por ganar es **37 pesos**. 
#' La entrada sigue siendo de **1 peso**.


###### Ejecutar desde aqui #################################
entrada <- 1
premio <- 37
###### Hasta aqui ##########################################

 
#' Jugaremos un millón de veces al número elegido 
#' Calculamos los resultados acumulados. ¿Cuál fue el resultado final?


###### Ejecutar desde aqui #################################
repeticiones <- 1000000
resultados <- replicate(repeticiones, {
  x <- sample(ruleta,1,replace=TRUE)
  if (x==numero_elegido) {
    resultados <- premio-entrada
  } else {
    resultados <- entrada*(-1)
  }
})
resultados_acumulados_37 <- cumsum(resultados)
paste("El resultado final fue: ",resultados_acumulados_37[repeticiones])
###### Hasta aqui ##########################################

 
#' Graficamos comparando con los resultados anteriores (puede demorar unos segundos 
#' en aparecer el gráfico)


###### Ejecutar desde aqui #################################
data.frame(tirada=seq(1,repeticiones,25), resultado_acumulado_36=resultados_acumulados_36[seq(1,repeticiones,25)],
           resultado_acumulado_37=resultados_acumulados_37[seq(1,repeticiones,25)],
           resultado_acumulado_38=resultados_acumulados_38[seq(1,repeticiones,25)]) %>%
  ggplot(aes(x=tirada)) +
  geom_line(aes(y=resultado_acumulado_36, colour="Premio_36")) +
  geom_line(aes(y=resultado_acumulado_37, colour="Premio_37")) +
  geom_line(aes(y=resultado_acumulado_38, colour="Premio_38")) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  ylab("Resultado Acumulado") +
  xlab("Número de juego") +
  ggtitle("Resultado acumulado de jugar\n1 millón de veces a la ruleta\n con distintos premios ") +
  scale_color_manual(values=c("Premio_36"="red", "Premio_37"="cyan", "Premio_38"="green"), name="") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Viendo los resultados con un premio de 37, ¿Parece ser un juego equilibrado? 
#' ¿Conviene jugar o ser el Casino?

#' ¿Qué relación encuentra entre el premio por ganar, el valor esperado del juego y la posibilidad de 
#' ganar o perder dinero en el largo plazo?

#' El resto de las preguntas de este ejercicio deberá responderlas sin ayuda de R.
