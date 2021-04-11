# MÓDULO IV - SATISFACCIÓN DEL CONSUMIDOR CRM - CSAT
#* MARIANA BENJUMEA PALACIO - 930

#En este ejercicio estaremos trabajando con data típica que uno puede extraer del modulo de Support Service
#de un CRM sobre los problemas que se reportan con el producto o servicio
#Y contiene una encuesta de satisfaccion tanto para el producto como para el servicio brindado por el equipo de custome support

#Para completar el ejercicio deberan cargar las siguientes librerias: tidyverse, corrplot, psych
library(tidyverse)
library(corrplot)
library(psych)
library(ggplot2)
library(lubridate)

#PISTA: Las librerias de corrplot y psych fueron utilizadas en el ejercicio de Percepcion de Marca. 
#Pueden revisar ese script como referencia para esta tarea


#** PASO 1 **
#Cargamos el dataset "data_CRM.csv" eliminando columna 1 que es innecesaria
getwd()
setwd("C:/Users/marib/OneDrive/Documentos/EAE/Customer Analytics/Modulo 4")
data_CRM <- read.csv("data_CRM.csv", header = T, sep = ",")
data_CRM <- select(data_CRM, -X)


#Inspecciones en dataset con summary, describe y head para comprender la información que contiene, entender missing values en las columnas
summary(data_CRM)
head(data_CRM)
tail(data_CRM)
describe(data_CRM)
colnames(data_CRM)
str(data_CRM)

data_CRM$create_date<- data_CRM$create_date %>%
  as.Date("%Y-%m-%d")
str(data_CRM$create_date)
data_CRM$month <- month(data_CRM$create_date)
#* Analizo la data y descubro que la estructura de la columna create_date es tipo character, 
#* así que la convierto en tipo fecha y creo una columna mes para agrupar los datos por mes

complaint <- data.frame(table(data_CRM$complaint_reason)); complaint
#* Creo un nuevo data frame en el que reuno las 23 razones de queja encontradas en la data
#* inicial y su frecuncia de recurrencia


#Realicen un plot para entender la distribución de las quejas a lo largo del período, 
#selecciondo el tipo de gráfico más óptimo para representar la data
complaint_mes <- data_CRM %>% 
  select(month, complaint_reason) %>% 
  group_by(month) %>% 
  count(complaint_reason)

Grafico1 <- ggplot(complaint_mes, aes(x = month, y = complaint_reason))+  
  geom_point(aes(size = n, color = n)) +  
  labs(title = "Numero de quejas por mes", x = "Meses", y = "Motivo de la queja"); Grafico1 
#* Extraigo las columnas que me interesan para poder graficar comodamente que son:
#* month, complaint_reason y con esta data me cuenta el número de veces que se repite
#* una queja en cada mes



#** PASO 2 **
#El dataset presenta todos los casos de los usuarios que han contactado con Customer Support en el período enero-abril 2020
#Pero nos interesa hacer el análisis sobre complaint_reason, por lo cual es necesario crear un nuevo dataset, 
#agrupar los datos por complaint_reason y realizar las siguientes operaciones para las columnas relevantes: 
data_CRM2 <- data_CRM %>% 
  group_by(complaint_reason)


#generar una columna que cuente la cantidad de llamadas para cada tipo de complaint_reason llamada "num_casos"
data_CRM2 <- data_CRM %>% 
  group_by(complaint_reason) %>%
  summarise(num_casos = length(complaint_reason))


#generar una columna que cuente la cantidad de llamadas pendientes para cada tipo de complaint_reason contando la cantidad de "y" llamada pend_calls
data_CRM2 <- data_CRM %>% 
  group_by(complaint_reason) %>%
  summarise(num_casos = length(complaint_reason),
            pend_calls = sum(pending_call == 'y'))


#calcular el promedio de time_to_resolution_min para cada tipo de complaint_reason en una columna nueva llamada avg_time_to_resolution
data_CRM2 <- data_CRM %>% 
  group_by(complaint_reason) %>%
  summarise(num_casos = length(complaint_reason),
            pend_calls = sum(pending_call == "y"),
            avg_time_to_resolution = mean(time_to_resolution_min))
  

#generar una columna que cuente la cantidad de need_replace para cada tipo de complaint_reason contando la cantidad de "TRUE" llamada n_replacements
data_CRM2 <- data_CRM %>% 
  group_by(complaint_reason) %>%
  summarise(num_casos = length(complaint_reason),
            pend_calls = sum(pending_call == "y"),
            avg_time_to_resolution = mean(time_to_resolution_min),
            n_replacements = sum(need_replace == TRUE))


#generar una nueva columna que calcule el Prod_CSAT para cada tipo de complaint_reason en una columna nueva llamada Prod_CSAT
data_CRM$prodCSAT1[data_CRM$ProdCSAT == "4" | data_CRM$ProdCSAT =="5"] <- 1
data_CRM$servCSAT1[data_CRM$ServCSAT == "4" | data_CRM$ServCSAT =="5"] <- 1
#* Creo 2 nuevas columnas al interior de nuestro dataframe inicial, donde evaluo  el producto
#* y el servicio del CSAT, para que todo los datos sean iguales a 4 o 5 con el fin de calcular 
#* y crear las nuevas columnas necesarias para el CSAT

data_CRM2 <- data_CRM %>% 
  group_by(complaint_reason) %>%
  summarise(num_casos = length(complaint_reason),
            pend_calls = sum(pending_call == "y"),
            avg_time_to_resolution = mean(time_to_resolution_min),
            n_replacements = sum(need_replace == TRUE),
            prod_CSAT=((sum(prodCSAT1, na.rm = T)/sum(ProdCSAT>0, na.rm = T))*100))


#generar una nueva columna que calcule el Serv_CSAT para cada tipo de complaint_reason en una columna nueva llamada Serv_CSAT
data_CRM2 <- data_CRM %>% 
  group_by(complaint_reason) %>%
  summarise(num_casos = length(complaint_reason),
            pend_calls = sum(pending_call == "y"),
            avg_time_to_resolution = mean(time_to_resolution_min),
            n_replacements = sum(need_replace == TRUE),
            prod_CSAT = ((sum(prodCSAT1, na.rm = T) / sum(ProdCSAT > 0, na.rm = T))*100),
            serv_CSAT = ((sum(servCSAT1, na.rm = T) / sum(ServCSAT > 0, na.rm = T))*100))


#De esta forma el dataset nuevo debe contener las siguientes columnas: complaint_reason, num_casos, pend_calls, 
#avg_time_to_resolution, n_replacements, Prod_CSAT, Serv_CSAT
view(data_CRM2)
colnames(data_CRM2)
unique(data_CRM2)



#** PASO 3 **
#Seleccionar un plot idoneo para poder realizar una comparativa de C-SATs para cada problema técnico
#Justificar la selección de dicho plot brevemente
Grafico2 <- ggplot(data_CRM2)+ 
  geom_col(aes(x=serv_CSAT, y=complaint_reason), fill = "darkseagreen2") + 
  geom_col(aes(x=prod_CSAT, y=complaint_reason), fill = "darkseagreen4") + 
  xlab("Nivel de satisfaccion") + 
  ylab("Motivo de la queja") + 
  ggtitle("Distribucion de C-SATS por problema tecnico"); Grafico2 
#* Use un grafico de columnas porque es la forma mas facil de poder visualizar los motivos de 
#* las quejas y lograr diferenciar por colores el nivel de satisfaccion de los clientes 



#** PASO 4 **
#Realizar una correlación entre las variables numéricas y analizar si existen correlaciones fuertes entre
#alguna de las variables presentadas. 
#las funciones de correlación poseen un argumento llamado use que permite excluir los NA para que el computo sea
#posible. Para ello incluyan como argumento use = "complete.obs" ya que por default es use = "everything" 
#¿La columna de Serv_CSAT muestra correlaciones con alguna otra columna?
correlacion <- cor(data_CRM2[,2:7])
Grafico3 <- corrplot(correlacion, order = "hclust", use = "complete.obs")
#* Las correlaciones fuertes que se pueden apreciar son: serv_csat con prod_csat; serv_csar~tiempo_promedio_resolucion;
#* numero de caso~n_reemplazo; pend_call~n_reemplazo; serv_cssat~prod_csat

#* Otras correlaciones que existe  al interior de los datos son:
#* variables las cuales tambien son positivas  y altas:
#* Num_casos y replacemente
#* Num_casos y pendind call
#* pendind call y replacement
#* Time to resolution y prod_csat

cor(data_CRM2$avg_time_to_resolution, data_CRM2$serv_CSAT,use = "complete.obs")
#* Existe una correlacion de 0.75 (74%) entre las variables de tiempo de resolucion 
#* y satisfaccion del servicio

cor(data_CRM2$num_casos, data_CRM2$serv_CSAT,use = "complete.obs")
#* Existe una correlacion negativa de -0.02 (2%) entre las variables de numero de casos 
#* y satisfaccion del servicio


#Inspeccionen la funcion cor.test para entender su funcionamiento y apliquenla sobre aquellas correlaciones
#que ustedes opinaron anteriormente que tienen correlación con la columna de Serv_CSAT para verificar si su hipotesis es correcta
#IMPORTANTE: pueden explorar los diferentes métodos, pero el que utilizamos de forma genérica es pearson
##a su vez es importante que comprendan y utilicen el argumento exact con lógica FALSE
?cor.test
cor.test(data_CRM2$avg_time_to_resolution, data_CRM2$serv_CSAT, 
         use = "complete.obs",
         method = 'pearson',
         exact = FALSE )
#* Obtenemos una correlacion positiva de 0.745 (74.5%) entre el tiempo promedio resolucion y 
#* la satisfaccion del servicio. Con un p-value de 4.431e-05 el cual nos indica que tomamos 
#* la hipotesis alterna y con un intervalo de confianza de (0,48, 0,88)


cor.test(data_CRM2$num_casos, data_CRM2$serv_CSAT,
         use = "complete.obs",
         method='pearson',
         exact=FALSE)
#* Obtenemos una correlacion negativa de -0.028 (2,8%) entre el numero de casos y la 
#* satisfaccion percibida del servicio. Con un p-value de 0.89 por lo que debemos rechazar la
#* hipotesis nula, ya que es mayor a 0,05 y se comprueba que el intervalo de confianza es de
#* (-0,43 , 0,38), por lo que toma valor cero (0). Por lo que podemos  decir que a mayor numero 
#* de quejas, menor sera la satisfaccion del cliente. 


#Por último utilicen la función corrplot.mixed() para realizar el plot de todas las correlaciones juntas
#Intenten utilizar algunas de las opciones que presenta para embellecer el gráfico (colores, formas, tamaños, etc)
#La forma de aplicación sería corrplot.mixed(corr = (correlacion que quieren hacer con sus argumentos incluido use = "complete.obs")) 
#y el resto de argumentos que quieran incluir
Grafico4 <- corrplot.mixed(cor(correlacion, use ="complete.obs"),
                           tl.col = "darkslategray4", 
                           lower.col = "seagreen",
                           number.cex = .7,
                           tl.cex=0.8,
                           upper =  "circle"); Grafico4

Grafico5 <- corrplot(cor(Correlacion, use ="complete.obs"),
                     tl.col = "palegreen4", 
                     number.cex = .7,
                     tl.cex=0.8,
                     method =  "ellipse"); Grafico5 



#** PASO 5 **
#Repetir el paso 4 pero enfocando el analisis en la columna Prod_CSAT en vez de Serv_CSAT: realicen hipotesis sobre correlaciones,
#apliquen cor.test para validarlas y corrplot.mixed() para representarlo.

cor.test(data_CRM2$num_casos, data_CRM2$prod_CSAT,use = "complete.obs",
         method='pearson',
         exact=FALSE)
#* Al realizar el analisis de correlacion entre las variables prod_CSAT y Serv_CSAT, podemos 
#* determinar que obtenemos una correlacion positiva de 0,058 (5,8%) entre el numero de casos y 
#* el producto. Con un p-value de 0,79 lo que debemos rechazar la hipotesis nula, ya que es mayor 
#* a 0,05 y se comprueba que el intervalo de confianza de 95% es de (-0,36 , 0,45), por lo que toma valor 
#* cero (0). 


cor.test(data_CRM2$pend_calls,data_CRM2$prod_CSAT,use = "complete.obs",
         method='pearson',
         exact=FALSE)
#* Al realizar el analisis de correlacion entre las variables prod_CSAT y Serv_CSAT, podemos 
#* determinar que obtenemos una correlacion positiva de 0,06 (6%) entre el numero de casos y 
#* el producto. Con un p-value de 0,785 lo que debemos rechazar la hipotesis nula, ya que es mayor 
#* a 0,05 y se comprueba que el intervalo de confianza de 95% es de (-0,36 , 0,46), por lo que toma valor 
#* cero (0). 
#*No podemos afirmar la hipotesis ya que, en ambos casos, la correlacion entre variables es menor al 10%. 

Grafico6 <- corrplot.mixed(cor(Correlacion, use ="complete.obs"),
                           tl.col = "red", 
                           lower.col = "black",
                           number.cex = .7,
                           tl.cex=0.8,
                           upper =  "square"); Grafico6

#####################################