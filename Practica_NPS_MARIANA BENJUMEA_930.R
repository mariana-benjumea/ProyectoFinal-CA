# MÓDULO IV - SATISFACCIÓN DEL CONSUMIDOR - NPS
#* MARIANA BENJUMEA PALACIO - 930

#NPS - Net Promoter Score - Score promedio de recomendacion
#En R existe una libreria llamada NPS para calcularlo automaticamente
#Deberán trabajar el calculo NPS sobre un dataset y analizar correlaciones con cluster de RFM

#** PASO 1 **
#Deben cargar las librerias tidyverse, lubridate, caret, corrplot y NPS
library(tidyverse)
library(lubridate)
library(caret)
library(corrplot)
library(NPS)
library(ggplot2)



#** PASO 2 **
#Generen un dataset llamado ventas2020 cargando el archivo "NPS_T1.csv" eliminando primera columna
#Ayuda: al utilizar select si escriben select(-1) entonces se seleccionan todas las columnas excepto la primera
getwd()
setwd("C:/Users/marib/OneDrive/Documentos/EAE/Customer Analytics/Modulo 4")
ventas2020 <- read.csv("NPS_T1.csv", header = T, sep = ",")
ventas2020 <- select(ventas2020, -X)
head(ventas2020)


#Eliminen aquellos registros que figuren con un NPS NA utilizando filter
ventas2020 <- ventas2020 %>%
  filter(nps!='NA')   


#Modifiquen la columna nps a columna numerica utilizando mutate y as.numeric
ventas2020 <- ventas2020 %>%
  mutate(nps_numeric = as.numeric(nps))
str(ventas2020)


#Calculen el NPS agrupado por cada tienda, utilizando la función nps del paquete NPS dentro de la funcion summarise
#¿cual es la tienda con mejor performance?
nps_tiendas <- ventas2020 %>%
  group_by(store) %>%
  summarise(nps_final = nps(nps_numeric)); nps_tiendas
max(nps_tiendas$nps_final)
#* La tienda con mejor performance es la tienda número 4 con 0,1922 de NPS


#Realizaremos un analisis entre los meses del año y el NPS para cada tienda
#para ello deben crear una columna mes en el dataframe de ventas2020 
#y agrupar tanto por mes como por store y calcular el nps en un nuevo data frame
ventas2020 <- ventas2020 %>% 
  mutate(fecha_compra = as.Date(ventas2020$fecha_compra),
         mes = month(ventas2020$fecha_compra))
str(ventas2020$fecha_compra)

nps_tiendas <- ventas2020 %>%
  group_by(store,mes) %>%
  summarise(nps = nps(nps)); nps_tiendas


#visualizamos la comparación de NPS de cada tienda para cada mes
#utilicen gráfico de scatter (geom_point) y den color a los puntos con
#columna store
Grafico1 <- ggplot(nps_tiendas, aes(x = mes, y = nps))+
  geom_point(aes(colour = factor(store)), size = 5, shape = 20) +
  labs(title="Net Promotore Score por mes", x="Mes", y="NPS"); Grafico1

range(Calculo_RFM$Monetary_Value)
mean(Calculo_RFM$Frequency)
mean(Calculo_RFM$Monetary_Value)
mean(Calculo_RFM$Recency)
#* Aplico estadistica descriptiva para comprender mejor el resultado


#Desarrollar el cálculo de RFM para cada comprador en otro dataframe 
#sin olvidar de modificar la columna de  fecha para que R la reconozca como tal utilizando as.Date
#Generen 5 clusters a traves de kmean para identificar segmentos de consumidores
#pueden utilizar de referencia el script visto en el modulo II
Calculo_RFM <- ventas2020 %>% 
  group_by(id_member) %>%
  summarise(Recency = as.numeric(as.Date(Sys.Date()) - max(fecha_compra)),
            Frequency = length(id_member), 
            Monetary_Value = sum(gasto)) 

set.seed(1234)
Segmentacion_RFM <- kmeans(scale(Calculo_RFM[,2:4]), 5, nstart = 1) 

Calculo_RFM$ScoreRFM <- as.factor(Segmentacion_RFM$cluster)
table(Calculo_RFM$ScoreRFM)

Clusters <- Calculo_RFM %>% 
  select(Recency, Frequency, Monetary_Value, ScoreRFM) %>% 
  group_by(ScoreRFM) %>% 
  summarise(mean(Recency), mean(Frequency), mean(Monetary_Value))

summary(Clusters)

#*Cluster N1 : Mejor recency, 2da mejor frequency y 3ero mejor monetary value
#*Cluster N2 : Peor recency, pero mejor frequency y monetary value
#*Cluster N3 : Peor frequency y monetary value, pero 2do mejor recency
#*Cluster N4 : Mejor frequency, 2do mejor monetary value pero 2do peor en recency
#*Cluster N5 : Mejor recency, 2do peor frequency y monetary value 

Calculo_RFM$Segmento <- NA
Calculo_RFM$Segmento[Calculo_RFM$ScoreRFM == 1] <- "Inactivo"
Calculo_RFM$Segmento[Calculo_RFM$ScoreRFM == 2] <- "En riesgo"
Calculo_RFM$Segmento[Calculo_RFM$ScoreRFM == 3] <- "Potenciales"
Calculo_RFM$Segmento[Calculo_RFM$ScoreRFM  == 4] <- "Leales"
Calculo_RFM$Segmento[Calculo_RFM$ScoreRFM == 5] <- "Champions"


#Calcular nps agrupando por segmento de consumidores en un nuevo data frame
data_NPS <- ventas2020 %>% 
  group_by(id_member) %>% 
  summarise(nps = nps(nps)) %>% 
  mutate(Segmento = Calculo_RFM$Segmento)

data_NPS2 <- data_NPS %>% 
  group_by(Segmento) %>% 
  summarise(nps = mean(nps))

Grafico2 <- ggplot(data_NPS2, aes(x = Segmento, y = nps, fill = Segmento)) +
  geom_col(show.legend = FALSE) +
  labs(title="NPS por Segmento de consumidores", x="Segmento", y="NPS"); Grafico2


#Ahora realicen una correlacion entre NPS y  los segmentos de consumidores de RFM
#Existen mayor correlacion con aquellos consumidores que gastan mas dinero o menos dinero?
data_NPS3 <- Calculo_RFM %>% 
  group_by(Segmento) %>% 
  summarise(gasto = mean(Monetary_Value))

Grafico3 <- ggplot(data_NPS3, aes(x = Segmento, y = gasto, fill = Segmento)) +
  geom_col(show.legend = FALSE) +
  labs(title="Gasto por Segmento de consumidores", x="Segmento", y="Gasto"); Grafico3

cor(data_NPS2$nps, data_NPS3$gasto) 
#* La correlacion es negativa (-0.5352088), esto significa que a medida que el nivel de 
#* lealtad de los consumidores es mas alto (mayor NPS) gastan menos dinero


#Que sucede si realizamos un promedio de NPS por cada segmentos para cada tienda?
#los segmentos puntúan muy diferente a cada tienda? Observamos algun patron?
promedio_nps <- left_join(ventas2020,Calculo_RFM,"id_member")

promedio_nps2 <- promedio_nps %>% 
  select(store,nps,ScoreRFM) %>% 
  group_by(store,ScoreRFM) %>% 
  summarise(nps_promedio=mean(nps))

head(promedio_nps2)

max(promedio_nps2$nps_promedio)
min(promedio_nps2$nps_promedio)
#* El valor promedio NPS maximo es de 8,04 y lo tiene la tienda 4 con un Score RFM de 5 "Champions"
#* El valor promedio NPS minimo es de 7,57 y lo tiene la tienda 1 con un Score RFM de 4 "Leales"
 
promedio_nps3 <- promedio_nps2 %>% 
  group_by(store) %>% 
  summarise(nps_promedio = mean(nps_promedio))

Grafico4 <- ggplot(promedio_nps3, aes(x = store, y = nps_promedio, fill = nps_promedio)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Promedio NPS por tienda", x="Tienda", y="NPS_Promedio") + 
  theme(plot.title = element_text(size = 20),
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=20), 
        axis.title.y=element_text(size=20)); Grafico4
#*Al analizar la media NPS de cada segmento para cada tienda se pierde informacion, todos 
#*los NPS dados recaen un score de 7.76, lo cual seria un patrón de indiferencia del consumidor 
#*frente a las tiendas, cuando no seria necesariamente asi.


#Que sucede si correlacionamos frecuencia de compra de los 172 ids con el NPS? 
#Los consumidores que tienen mayor frecuencia de compra puntúan mas y mejor?
Frecuencia <- data_NPS %>% 
  mutate(Frecuencia = Calculo_RFM$Frequency)

Grafico5 <- ggplot(Frecuencia, aes(x = Frecuencia, y = nps, colour = Segmento)) + 
  geom_point(aes(size = Frecuencia))+ 
  geom_smooth(method = 'lm', color = 'black', alpha = 0.2)+ 
  theme_classic()+ 
  ggtitle("Relación de NPS y Frecuencia de Compra"); Grafico5

cor(Frecuencia$nps, Frecuencia$Frecuencia)
#* La correlacion es de -0.08367377, una correlacion inversa y en el grafico se puede observar una 
#* pendiente negativa, lo cual indica que aquellos clientes con mayor frecuencia de compra,
#* peor sera la puntuacion a la tienda. Tambien aquellos con menor frecuencia de compra como 
#* nuestro segmento de clientes inactivos son mas propensos a dejar mejor puntuación, menos exigentes.


#En líneas generales luego del análisis exploratorio, ¿podriamos identificar tiendas que sobresalen
#por una buena o una mala performance en terminos de NPS?
#* Las tiendas 4 y 5 son en linea general, las mejores frente a satisfaccion de los consumidores,
#* La tienda 4 tiene el NPS mas alto, sin embargo podemos observar que tiene pobremas con su nps del 
#* segmentos champions(el peor puntaje de NPS de tienda del segmento champions), nuestro segmento mas 
#* importante, podiamos realizar un estudio mas a fondo para revisar la razon de esto y porque con este 
#* segmento en especial, mientras que la tienda 2 mantiene el NPS positivo mas bajo pero siguen siendo positivas.

#####################################