########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################




#' En este ejercicio utilizaremos cotizaciones históricas de tres empresas 
#' que cotizan en la Bolsa de Comercio de Buenos Aires: Banco Galicia (GGAL), 
#' Banco Francés (FRAN) e YPF (YPFD). En particular, tenemos información sobre 
#' el rendimiento mensual que tuvo cada una de estas acciones desde enero de 
#' 2010 hasta agosto de 2020.
#' Para calcular el rendimiento mensual, lo que hicimos fue calcular el 
#' valor promedio del precio de cada acción para cada mes, y luego calcular 
#' las variaciones porcentuales mes a mes. 


##### Parte a)

#' Calcular indicadores de centralidad y dispersión para cada el rendimiento 
#' de cada una de las acciones


###### Ejecutar desde aqui #################################
library(tidyverse)
library(gridExtra)
load("./archivos/rendimientos.Rda")
summary(rendimientos$GGAL)
summary(rendimientos$FRAN)
summary(rendimientos$YPFD)
var(rendimientos$GGAL)
var(rendimientos$FRAN)
var(rendimientos$YPFD)
sd(rendimientos$GGAL)
sd(rendimientos$FRAN)
sd(rendimientos$YPFD)
###### Hasta aqui ##########################################


#' Dibujar histograma y boxplot de los rendimientos
 
## Histograma


###### Ejecutar desde aqui #################################
rendimientos %>% gather(Accion, Rendimiento) %>% 
  ggplot(aes(x=Rendimiento)) +
  #geom_histogram(binwidth = 0.5, alpha=.5, position="identity") +
  geom_histogram(position="identity", color="black", fill="cyan") +
  theme_classic() +
  facet_grid(. ~ Accion) +
  ggtitle("Rendimientos mensuales") +
  ylab("Frecuencia Absoluta") +
  xlab("Rendimiento Porcentual")
###### Hasta aqui ##########################################


## Boxplot


###### Ejecutar desde aqui #################################
rendimientos %>% gather(Accion, Rendimiento) %>%
  ggplot(aes(x=Accion, y=Rendimiento)) + 
  geom_boxplot() + 
  theme_classic() +
  ggtitle("Rendimientos mensuales")
###### Hasta aqui ##########################################

 
#' Analice los resultados. Suponiendo que la performance pasada 
#' es un indicador de lo que puede suceder a futuro, 
#' ¿Alguna acción luce más atractiva que otra? 
#' ¿Alguna acción promete mejores rendimientos? ¿Alguna acción promete menor 
#' riesgo?

##### Parte b)

#' Calcule las covarianzas y coeficientes de correlación entre los rendimientos 
#' de cada par de acciones. 


###### Ejecutar desde aqui #################################
paste("Covarianza entre GGAL y FRAN:",cov(rendimientos$GGAL,rendimientos$FRAN))
paste("Covarianza entre GGAL y YPFD:",cov(rendimientos$GGAL,rendimientos$YPFD))
paste("Covarianza entre YPFD y FRAN:",cov(rendimientos$YPFD,rendimientos$FRAN))
paste("Coef. de Correlación entre GGAL y FRAN:",cor(rendimientos$GGAL,rendimientos$FRAN))
paste("Coef. de Correlación entre GGAL y YPFD:",cor(rendimientos$GGAL,rendimientos$YPFD))
paste("Coef. de Correlación entre YPFD y FRAN:",cor(rendimientos$YPFD,rendimientos$FRAN))
###### Hasta aqui ##########################################


#' Realice un gráfico de dispersión para el rendimiento de cada par de acciones.


###### Ejecutar desde aqui #################################
g1 <- rendimientos %>% ggplot(aes(x=GGAL, y=FRAN)) +
  geom_point() +
  theme_classic() +
  ggtitle("Graf. de Dispersión GGAL y FRAN")
g2 <- rendimientos %>% ggplot(aes(x=GGAL, y=YPFD)) +
  geom_point() +
  theme_classic() +
  ggtitle("Graf. de Dispersión GGAL y YPFD")
g3 <- rendimientos %>% ggplot(aes(x=YPFD, y=FRAN)) +
  geom_point() +
  theme_classic() +
  ggtitle("Graf. de Dispersión YPFD y FRAN")
grid.arrange(g1, g2, g3, nrow = 2)
###### Hasta aqui ##########################################


#' ¿Identifica alguna relación positiva o negativa entre los rendimientos? 
#' En caso afirmativo, ¿qué tan fuerte es la relación?
 
##### Parte c)

#' Decide invertir en dos de estas acciones, 50% del dinero en cada una,
#' aunque todavía no sabe qué par de acciones elegirá. 

#' Calcule el rendimiento y la varianza de cada una de las carteras posibles.


###### Ejecutar desde aqui #################################
paste("GGAL y FRAN. Rendimiento esperado:",0.5*mean(rendimientos$GGAL)+0.5*mean(rendimientos$FRAN))
paste("GGAL y YPFD. Rendimiento esperado:",0.5*mean(rendimientos$GGAL)+0.5*mean(rendimientos$YPFD))
paste("YPFD y FRAN. Rendimiento esperado:",0.5*mean(rendimientos$YPFD)+0.5*mean(rendimientos$FRAN))
paste("GGAL y FRAN. Varianza:",(0.5^2)*var(rendimientos$GGAL)+(0.5^2)*var(rendimientos$FRAN)+2*0.5*0.5*cov(rendimientos$GGAL,rendimientos$FRAN))
paste("GGAL y YPFD. Varianza:",(0.5^2)*var(rendimientos$GGAL)+(0.5^2)*var(rendimientos$YPFD)+2*0.5*0.5*cov(rendimientos$GGAL,rendimientos$YPFD))
paste("YPFD y FRAN. Varianza:",(0.5^2)*var(rendimientos$YPFD)+(0.5^2)*var(rendimientos$FRAN)+2*0.5*0.5*cov(rendimientos$YPFD,rendimientos$FRAN))
###### Hasta aqui ##########################################


#' ¿Qué rol juegan las covarianzas calculadas en la Parte b)?
#' ¿Qué par de acciones debería elegir si prioriza el rendimiento, más allá del
#'  riesgo?
#' ¿Qué par de acciones debería elegir si quiere minimizar el riesgo de pérdidas?

##### Parte c)

#' Suponga que, independientemente de los resultados anteriores, luego de leer 
#' informes de distintos analistas decidió que no era buena idea invertir en 
#' acciones del Banco Francés. Por lo tanto, ya tiene decidido que invertirá 
#' una parte de su cartera en acciones del Banco Galicia y la restante en YPF.
#' Resta decidir qué porcentaje de su cartera estará destinada a cada una. 
#' Para esto, realiza el cálculo del valor esperado y varianza de distintas 
#' carteras.


###### Ejecutar desde aqui #################################
m_ggal <- mean(rendimientos$GGAL)
m_ypfd <- mean(rendimientos$YPFD)
var_ggal <- var(rendimientos$GGAL)
var_ypfd <- var(rendimientos$YPFD)
covarianza <- cov(rendimientos$GGAL,rendimientos$YPFD)
tabla <- data.frame("GGAL"=seq(0,100,10), "YPFD"=seq(100,0,-10),
           "Esperanza"=seq(0,100,10)*m_ggal+seq(100,0,-10)*m_ypfd,
           "Varianza"=(seq(0,100,10)^2)*var_ggal+(seq(100,0,-10)^2)*var_ypfd+
             2*seq(0,100,10)*seq(100,0,-10)*covarianza)
tabla$Desvio <- sqrt(tabla$Varianza)
tabla
###### Hasta aqui ##########################################


#' Represente estos resultados en un gráfico ubicando en el eje X 
#' el retorno esperado y en el eje Y el riesgo de la cartera (desvío standard):


###### Ejecutar desde aqui #################################
tabla %>% ggplot(aes(x=Desvio, y=Esperanza)) +
  geom_point() +
  geom_path() +
  theme_classic() +
  ggtitle("Relación Riesgo-Retorno de la cartera")
###### Hasta aqui ##########################################


#' ¿Existe algún punto de esa curva en donde un inversor racional nunca invertiría?

#' ¿Es posible que dos inversores racionales elijan puntos diferentes de la curva?