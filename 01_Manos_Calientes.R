########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################


#' Manos Calientes en el Basketball

#' El artículo original fue escrito por profesores y asistentes de 
#' Estadística de UCLA y adaptado para el libro **OpenIntro por Andrew 
#' Bray y Mine Cetinkaya-Rundel**. La versión en inglés del mismo se puede 
#' descargar de http://www.openintro.org/stat/labs/02A_Probability.pdf 

#' La traducción para el curso de Tamara Burdisso fue realizada por
#' *Ignacio Caro Solís*

#' Se suele decir que los jugadores de básquet que tienen varios aciertos
#' consecutivos tienen **“manos calientes”**. Los fanáticos y jugadores 
#' han creído por mucho tiempo en este fenómeno, que niega el supuesto 
#' de que cada tiro es independiente del anterior. Sin embargo, un trabajo 
#' de Gilovich, Vallone y Tversky de 1985 recopiló evidencia que contradecía 
#' esta creencia y mostraba que **los tiros consecutivos son eventos independientes**. 
#' Este paper inició una gran controversia que continúa hasta el día de hoy, 
#' como se puede ver buscando en Google “mano caliente basketball” 
#' (“hot hand basketball”).

#' No esperamos resolver esta controversia aquí. Sin embargo, en este lab 
#' emplearemos un enfoque para responder preguntas como esta. Los objetivos 
#' para este lab son (1) pensar acerca de los efectos de eventos independientes 
#' y dependientes, (2) aprender cómo simular rachas de aciertos en R, y (3) 
#' comparar la simulación con los datos observados para determinar si el 
#' fenómeno de las “manos calientes” parece ser real.

#' Nuestra investigación se concentrará en la performance de un solo jugador: 
#' **Kobe Bryant** de los Los Ángeles Lakers. Su desempeño contra Orlando Magic 
#' en la final de la NBA de 2009 le valió el título de “el jugador más valioso” 
#' y muchos espectadores señalaron cómo pareció mostrar una “mano caliente”.
 
#' Carguemos datos sobre esos juegos y veamos las primeras filas.


###### Ejecutar desde aqui ################################# 
library(tidyverse)
load("./archivos/kobe.RData")
head(kobe)
###### Hasta aqui ##########################################


#' Cada fila de datos muestra un tiro realizado por Kobe Bryant. Observamos que hay cinco variables:
#' “vs”: indica el equipo rival. En este caso, siempre es Orlando Magic (ORL)
#' “game”: indica el número de partido de la serie vinal (valores de 1 a 5)
#' “quarter”: indica el cuarto de cada partido (de 1 a 4)
#' “time”: indica el tiempo transcurrido en el cuarto (los cuartos duran 12 minutos)
#' “description”: una descripción sobre el tiro
#' “basket”: indica si convirtió el tiro (“H”, por *hit* en inglés) o si falló (“M”, por *miss* en inglés)

#' Con ver a simple vista la secuencia de aciertos y desaciertos puede ser 
#' dificil evaluar si Kobe estaba teniendo una **mano caliente** o no. Una 
#' forma en la que podemos encarar esta pregunta es considerar la creencia de 
#' que los jugadores con manos calientes tienden a tener rachas en sus 
#' lanzamientos.
#' 
#' Para este ejercicio **vamos a definir el largo de una racha como el número** 
#' **de aciertos consecutivos que tiene un jugador hasta que comete un error**.
#' 
#' Por ejemplo, analicemos la secuencia de aciertos y errores en los nueve lanzamientos
#' de Kobe durante el primer cuarto del juego 1. ¿Cuál fue esta secuencia?


###### Ejecutar desde aqui ################################# 
kobe %>% filter(game==1, quarter==1) %>% .$basket
###### Hasta aqui ##########################################


#'Dentro de los nueve intentos de tiro anteriores, hay seis rachas. 
#'Recordemos que cada racha termina con un tiro errado (“M”), por lo que la 
#'primer racha está compuesta por la secuencia (“H” “M”), la segunda racha por 
#'la secuencia (“M”), etc. **Definamos como el largo de cada racha a la cantidad** 
#'**de aciertos (H) en ella**. En el caso de las seis rachas anteriores, el largo 
#'de las mismas es: 
#'1, 0, 2, 0, 0, 0 (en orden de ocurrencia).

#' ¿Qué significa una racha de largo 1 (es decir, cuántos aciertos y errores 
#' hay en una racha de 1)? ¿Y en una racha de 0?

#' Al cargar los datos, se cargó también una función que puede ser usada para calcular 
#' los largos de todas las rachas, pudiendo analizar luego su distribución. Por ejemplo, 
#' podemos calcular los largos de las rachas para los 9 lanzamientos del primer 
#' cuarto del juego 1:


###### Ejecutar desde aqui ################################# 
calc_streak(kobe$basket[1:9])
###### Hasta aqui ##########################################


#' Puede verse que los resultados coinciden con los que habíamos calculado 
#' previamente.
 
#' Calcularemos los largos de las rachas para todos los juegos, y los 
#' guardaremos en el objeto *kobe_streak*. Realizamos un gráfico de barras 
#' de la distribución:


###### Ejecutar desde aqui ################################# 
kobe_streak <- calc_streak(kobe$basket)
barplot(table(kobe_streak), xlab="Largo de Rachas", ylab="Cantidad de Rachas")
###### Hasta aqui ##########################################


#' Describir la distribución de los largos de las rachas de Kobe durante la final
#'  de la NBA de 2009. ¿Cuál fue el largo de racha más frecuente? ¿Cuál fue el 
#'  largo máximo de sus rachas?

#' Ya mostramos que Kobe tuvo algunas extensas rachas de aciertos, pero ¿son 
#' suficientes para sostener la creencia de que tuvo manos calientes? ¿Con qué 
#' podemos compararlas?

#' Para responder estas preguntas, volvamos a la idea de independencia. 
#' **Dos procesos son independientes si el resultado de uno no afecta al**
#' **resultado del otro. Si cada tiro que un jugador hace es un proceso **
#' **independiente, haber acertado o no su primer tiro no afecta a la **
#' **probabilidad de que acierte o no el segundo.**

#' Un jugador con una mano caliente tendría tiros no independientes unos 
#' de otros. Específicamente, si el jugador acierta su primer tiro, el modelo 
#' de la mano caliente predice que tendrá una mayor probabilidad de encestar 
#' en su segundo tiro.

#' Supongamos por un momento que este modelo es válido para Kobe. Durante su 
#' carrera, el porcentaje de las veces que Kobe encesta es alrededor de 45%.

#' **Si acierta su primer tiro al aro y además tiene una mano caliente **
#' **(tiros no independientes entre sí), entonces la probabilidad de que **
#' **acierte su segundo tiro será mayor**.
 
#' Supongamos que, cuando se tiene la *mano caliente*, la probabilidad de 
#' acertar el segundo tiro habiendo acertado el primero es 60%.

#' Como resultado de las mayores probabilidades, se esperaría que Kobe tuviese 
#' rachas más largas.

#' Comparemos esto con una **perspectiva escéptica, en la que Kobe no tiene** 
#' **una mano caliente y en la que cada tiro es independiente del anterior**. 
#' Si acertó al aro en su primer tiro, la probabilidad de que acierte el 
#' segundo también sigue siendo 0.45.

#' En otras palabras, haber acertado el primer tiro no afectó la probabilidad 
#' de que hubiera acertado también el segundo. 
#' **Si los tiros de Kobe son independientes, entonces tendría la misma **
#' **probabilidad de acertar cada tiro (sin importar sus anteriores): 45%**.

#' Ahora que planteamos este problema en términos de tiros independientes, 
#' volvamos a la pregunta: **¿cómo definimos si las rachas de Kobe son lo **
#' **suficientemente largas como para indicar que tuvo manos calientes? **
#' **Podemos comparar los largos de sus rachas con los de alguien sin manos **
#' **calientes: un tirador ‘independiente’**.

#' ### Simulaciones

#' Si bien no tenemos datos de algún tirador que sepamos que tenga tiros 
#' independientes, ese tipo de datos es muy sencillo de simular en R. 
#' En una simulación se fijan las reglas de un proceso aleatorio y luego 
#' la computadora usa números al azar para generar un resultado que adhiera 
#' a esas reglas.

#' Para simular un jugador de básquet que tiene tiros independientes debemos 
#' usar el mismo mecanismo que usaríamos para simular los lanzamientos de moneda. 
#' Los resultados posibles de cada lanzamiento son 2: "H" (acierto, por *hit* 
#' en inglés), "M" (fallo, por *miss* en inglés):

#' Para comparar a este jugador independiente con Kobe Bryant, supondremos 
#' que la probabilidad de "H" es 45% y la de "M" 55%.

#' Simular 5 lanzamientos del *jugador independiente*
 

###### Ejecutar desde aqui ################################# 
resultados <- c("H", "M")
sample(resultados, size = 5, prob = c(0.45,0.55), replace = TRUE)
###### Hasta aqui ##########################################


#' Dado que tenemos 133 lanzamientos de Kobe Bryant, simular 133 lanzamientos 
#' del *jugador independiente* y graficar los resultados


###### Ejecutar desde aqui #################################
ind133 <- sample(resultados, size = 133, prob = c(0.45,0.55), replace = TRUE)
table(ind133)
data.frame(tiro=ind133) %>%
  ggplot() +
  geom_bar(aes(x=tiro, fill=tiro)) +
  xlab("Resultado del tiro") +
  ylab("Cantidad de tiros") +
  theme_classic() +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' Los resultados de una simulación no siempre son iguales. Por lo tanto,
#' para poder sacar conclusiones mediante simulaciones puede ser necesario 
#' repetir el proceso varias veces.

#' Simulemos 133 tiros de otros 4 jugadores independientes y grafiquemos los
#' resultados, comparando con los de Kobe.


###### Ejecutar desde aqui #################################
ind133_2 <- sample(resultados, size = 133, prob = c(0.45,0.55), replace = TRUE)
ind133_3 <- sample(resultados, size = 133, prob = c(0.45,0.55), replace = TRUE)
ind133_4 <- sample(resultados, size = 133, prob = c(0.45,0.55), replace = TRUE)
ind133_5 <- sample(resultados, size = 133, prob = c(0.45,0.55), replace = TRUE)
data.frame(jugador = c(rep(1,133),rep(2,133),rep(3,133),rep(4,133),rep(5,133),rep("Kobe",133)),
           tiro=c(ind133,ind133_2,ind133_3,ind133_4,ind133_5,kobe$basket)) %>%
  ggplot() +
  geom_bar(aes(x=tiro, fill=tiro)) +
  facet_wrap(.~jugador) +
  xlab("Resultado del tiro") +
  ylab("Cantidad de tiros") +
  theme_classic() +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' ¿Son muy diferentes?

#' Comparar las rachas de los *jugadores independiente* con las de Kobe Bryant. 


###### Ejecutar desde aqui #################################
data.frame(jugador = c(rep(1,length(calc_streak(ind133))),rep(2,length(calc_streak(ind133_2))),
                       rep(3,length(calc_streak(ind133_3))),rep(4,length(calc_streak(ind133_4))),
                       rep(5,length(calc_streak(ind133_5))),rep("Kobe",length(calc_streak(kobe$basket)))),
           rachas=c(calc_streak(ind133),calc_streak(ind133_2),calc_streak(ind133_3),
                    calc_streak(ind133_4),calc_streak(ind133_5),calc_streak(kobe$basket))) %>%
  ggplot() +
  geom_bar(aes(x=rachas, fill=rachas)) +
  ggtitle("Rachas de Kobe y de los jug. indep.") +
  xlab("Largo de rachas") +
  ylab("Cantidad de rachas") +
  facet_wrap(.~jugador) +
  theme_classic() +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.ontop = TRUE)
###### Hasta aqui ##########################################


#' ¿Son muy diferentes?
 
 
#' En base a los resultados obtenidos, ¿considera que hay evidencia
#' para afirmar que Kobe Bryant tuvo manos calientes durante la final de 2009?
#' Justificar la respuesta.
