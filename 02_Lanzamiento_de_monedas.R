########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################



#' En este ejercicio, jugaremos con el lanzamiento de monedas.

##### Parte a)

#' Realizar 10 lanzamientos de una moneda *equilibrada*. Entendemos por moneda equilibrada 
#' a una moneda en donde la probabilidad de obtener "cara" es la misma a la de obtener "ceca".
#' ¿Qué resultados obtuvo?

###### Ejecutar desde aqui #################################
library(tidyverse)
lanzamientos_10 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
lanzamientos_10
###### Hasta aqui ##########################################


#' ¿Qué proporción de caras y cecas?

###### Ejecutar desde aqui #################################
cat("La proporción de caras fue:", mean(lanzamientos_10=="Cara"))
cat("La proporción de cecas fue:", mean(lanzamientos_10=="Ceca"))
###### Hasta aqui ##########################################


#' Representar los resultados en un gráfico


###### Ejecutar desde aqui #################################
lanzamientos_10 %>% as.data.frame() %>%
  ggplot() +
  geom_bar(aes(.), fill="cyan") +
  theme_classic() +
  ggtitle("10 lanzamientos de moneda equilibrada") +
  ylab("Cantidad de lanzamientos") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Los resultados de una simulación no siempre son iguales. Por lo tanto, 
#' para poder sacar conclusiones mediante simulaciones puede ser necesario 
#' repetir el proceso varias veces. 

#' Repitamos el lanzamiento de una moneda equilibrada otras 5 veces y grafiquemos 
#' la proporción de caras y cecas obtenida en cada caso.


###### Ejecutar desde aqui #################################
lanzamientos_10_2 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
lanzamientos_10_3 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
lanzamientos_10_4 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
lanzamientos_10_5 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
lanzamientos_10_6 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
data.frame(n = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10)),
           resultado = c(lanzamientos_10,lanzamientos_10_2,lanzamientos_10_3,
                         lanzamientos_10_4,lanzamientos_10_5,lanzamientos_10_6)) %>%
  ggplot() +
  geom_bar(aes(x=resultado), fill="cyan") +
  theme_classic() +
  facet_wrap(.~n) +
  ggtitle("10 lanzamientos de moneda equilibrada") +
  ylab("Cantidad de lanzamientos") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' #### Parte b)

#' Realizar 10 lanzamientos de una moneda donde la probabilidad de obtener cara es
#'  0.55 y la de obtener ceca es 0.45. Es decir, la moneda no es *equilibrada* sino 
#'  que está cargada a favor de las caras.
#' ¿Qué resultados obtuvo?


###### Ejecutar desde aqui #################################
lanzamientos_10b <- sample(c("Cara", "Ceca"), 10, prob = c(0.55,0.45), replace = TRUE)
lanzamientos_10b
###### Hasta aqui ##########################################


#' ¿Qué proporción de caras y cecas?


###### Ejecutar desde aqui #################################
cat("La proporción de caras fue:", mean(lanzamientos_10b=="Cara"))
cat("La proporción de cecas fue:", mean(lanzamientos_10b=="Ceca"))
###### Hasta aqui ##########################################

 
#' Representar los resultados en un gráfico
 

###### Ejecutar desde aqui #################################
lanzamientos_10b %>% as.data.frame() %>%
  ggplot() +
  geom_bar(aes(.), fill="cyan") +
  theme_classic() +
  ggtitle("10 lanzamientos de moneda no equilibrada") +
  ylab("Cantidad de lanzamientos") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Nuevamente, para poder sacar conclusiones mediante simulaciones
#' puede ser necesario repetir el proceso varias veces. 

#' Repitamos el lanzamiento de una moneda cargada otras 5 veces y grafiquemos 
#' la proporción de caras y cecas obtenida en cada caso.


###### Ejecutar desde aqui #################################
lanzamientos_10b_2 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
lanzamientos_10b_3 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
lanzamientos_10b_4 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
lanzamientos_10b_5 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
lanzamientos_10b_6 <- sample(c("Cara", "Ceca"), 10, replace = TRUE)
data.frame(n = c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10)),
           resultado = c(lanzamientos_10b,lanzamientos_10b_2,lanzamientos_10b_3,
                         lanzamientos_10b_4,lanzamientos_10b_5,lanzamientos_10b_6)) %>%
  ggplot() +
  geom_bar(aes(x=resultado), fill="cyan") +
  theme_classic() +
  facet_wrap(.~n) +
  ggtitle("10 lanzamientos de moneda cargada") +
  ylab("Cantidad de lanzamientos") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' ¿Obtuvo resultados muy distintos respecto a la Parte A?

#' Con sólo observar estos resultados, ¿estaría en condiciones de afirmar que la 
#' moneda de la Parte B estaba cargada mientras que la de la Parte A era equilibrada?
 
#' ¿Qué tan probables considera los resultados obtenidos? Los siguientes gráficos de la distribución
#' binomial pueden ayudarle a responder.


###### Ejecutar desde aqui #################################
data.frame(Exitos=c(0:10), "probabilidad_0.5"=dbinom((0:10),10,0.5), "probabilidad_0.55"=dbinom((0:10),10,0.55)) %>%
  gather(caso,prob,-Exitos) %>%
  ggplot(aes(x=Exitos, y=prob)) +
  geom_col(fill="cyan") +
  ylab("Probabilidad") +
  xlab("Cantidad de Caras") +
  facet_wrap(.~caso) +
  theme_classic() +
  scale_x_continuous(breaks=c(0:10),labels=c(0:10)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################

 
#' #### Parte C

#' Repetiremos la Part A, pero esta vez lanzando **1 millón** de veces la 
#' moneda equilibrada
#' Muestre los primeros 10 resultados


###### Ejecutar desde aqui #################################
lanzamientos_1m <- sample(c("Cara", "Ceca"), 1000000, replace = TRUE)
head(lanzamientos_1m,10)
###### Hasta aqui ##########################################


#' ¿Qué proporción de caras y cecas en el millón de lanzamientos?


###### Ejecutar desde aqui #################################
cat("La proporción de caras fue:", mean(lanzamientos_1m=="Cara"))
cat("La proporción de cecas fue:", mean(lanzamientos_1m=="Ceca"))
###### Hasta aqui ##########################################


#' Representar los resultados en un gráfico
 

###### Ejecutar desde aqui #################################
lanzamientos_1m %>% as.data.frame() %>%
  ggplot() +
  geom_bar(aes(.), fill="cyan") +
  theme_classic() +
  ggtitle("1 millón de lanzamientos de moneda equilibrada") +
  ylab("Cantidad de lanzamientos") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Viendo los resultados, ¿está en condiciones de afirmar que la moneda es *equilibrada*?

#' Más allá de lo que se pueda responder “a ojo”, puede ser necesario recurrir a 
#' argumentos un tanto más rigurosos. Algunos de estos métodos serán vistos en la 
#' segunda parte de la materia. Por ahora, utilicemos lo que sabemos sobre la 
#' distribución Binomial.

#' La cantidad de caras que se obtienen al lanzar 1 millón de veces una moneda 
#' equilibrada es una variable aleatoria que sigue una distribución Binomial con 
#' n=1.000.000 y p=0,5.

#' ¿Cuál es el valor esperado de esta variable?
 

###### Ejecutar desde aqui #################################
valor_esperado <- 1000000*0.5
valor_esperado
###### Hasta aqui ##########################################


#' ¿Cuál es el desvío standard de esta variable aleatoria?


###### Ejecutar desde aqui #################################
desvio <- sqrt(1000000*0.5*0.5)
desvio
###### Hasta aqui ##########################################

  
#' Volviendo a la cantidad de caras que obtuvo en el lanzamiento de 1 millón de 
#' monedas equilibradas, ¿a cuántos desvíos standards se encuentra del valor 
#' esperado? Es decir, calcule el valor z del resultado obtenido en la simulación.


###### Ejecutar desde aqui #################################
valor_z <- (sum(lanzamientos_1m=="Cara")-valor_esperado)/desvio
valor_z
###### Hasta aqui ##########################################


#' En base a lo anterior, ¿cree que los resultados obtenidos en la simulación son 
#' consistentes con los de una moneda equilibrada? Justifique.


#' #### Parte D
 
#' Repetiremos la Parte B, pero esta vez lanzando **1 millón** de veces la moneda
#'  con probabilidades 0,55 y 0,45
#' Muestre los primeros 10 resultados 


###### Ejecutar desde aqui #################################
lanzamientos_1mb <- sample(c("Cara", "Ceca"), 1000000, prob = c(0.55,0.45), replace = TRUE)
head(lanzamientos_1mb,10)
###### Hasta aqui ##########################################


#' ¿Qué proporción de caras y cecas?


###### Ejecutar desde aqui #################################
cat("La proporción de caras fue:", mean(lanzamientos_1mb=="Cara"))
cat("La proporción de cecas fue:", mean(lanzamientos_1mb=="Ceca"))
###### Hasta aqui ##########################################

 
#' Representar los resultados en un gráfico


###### Ejecutar desde aqui #################################
lanzamientos_1mb %>% as.data.frame() %>%
  ggplot() +
  geom_bar(aes(.), fill="cyan") +
  theme_classic() +
  ggtitle("1 millón de lanzamientos de moneda no equilibrada") +
  ylab("Cantidad de lanzamientos") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Viendo los resultados, ¿está en condiciones de afirmar que la moneda no es *equilibrada*?

#' Nuevamente, puede ser necesario recurrir a argumentos un tanto más rigurosos que 
#' simplemente evaluar un gráfico. 
#' En el ejercicio anterior ya calculó el valor esperado y el desvío de la variable 
#' aleatoria “cantidad de caras en 1 millón de lanzamientos de una moneda equilibrada”.
#' Calcule a cuántos desvíos standards se encuentra del valor esperado el resultado 
#' obtenido con la moneda cargada. Es decir, calcule el valor z del resultado 
#' obtenido en la simulación.


###### Ejecutar desde aqui #################################
valor_z <- (sum(lanzamientos_1mb =="Cara")-valor_esperado)/desvio
valor_z
###### Hasta aqui ##########################################


#' En base a lo anterior, ¿cree que los resultados obtenidos en la simulación 
#' son consistentes con los de una moneda equilibrada? ¿o puede afirmar que la 
#' moneda estaba efectivamente cargada a favor de las caras? Justifique.


#' Para terminar, ¿Qué diferencias nota en la simulación con 10 lanzamientos y 
#' la de 1 millón de lanzamientos? ¿Cuál es más confiable?
