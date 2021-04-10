# MÓDULO III - EMAIL MARKETING
#* MARIANA BENJUMEA PALACIO - 930
 
#Análisis explotatorio de campañas de E-mail Marketing

#** PASO 1 **
#Deben cargar las librerias:
library(tidyverse)
library(lubridate) #trabajo con fechas
library(corrplot)#correlaciones pueda graficar mas amigable
library(dplyr)
library(stats)
library(ggplot2)



#** PASO 2 **
#Generar un dataset llamado email_analysis cargando el archivo "dataset_email-mkt.csv" eliminando primera columna
getwd()
setwd("C:/Users/marib/OneDrive/Documentos/EAE/Customer Analytics/Modulo 3")
email_analysis <- read.csv("dataset_email-mkt.csv", header = TRUE)
email_analysis <- select(email_analysis, -email_subject)


#Modificar la columna de sendout_date para que sea fecha y no character
email_analysis$sendout_date <- as.Date(email_analysis$sendout_date, "%Y-%m-%d")
str(email_analysis$sendout_date)
head(email_analysis)


#** PASO 3 **
#Generar un segundo dataset email_campaign filtrando la columna
#email_scope == "Campaign"
email_campaign <- email_analysis %>%
  filter(email_scope=="Campaign")
head(email_campaign)


#Calculen los datos agregados de todas las columnas que comienzan con "total_"
#agrupando por journeys_id
range(email_campaign$journeys_ids)

journeys <- email_campaign %>% 
  group_by(journeys_ids) %>% 
  summarise_at(vars(matches("^Total_")),
               sum, na.rm = TRUE)
#var variable
#^ como comienza
head(journeys)


#Realicen un plot de la cantidad de envios de mails para cada journeys_id
Grafico1 <- plot(journeys$total_email_sent, 
                 type = "p",
                 main = "Cantidad de envio de mails por cada Journeys_id",
                 xlab = "Journeys_id",
                 ylab = "Emails enviados")

Grafico2 <- ggplot(journeys, aes(journeys_ids, total_email_sent)) +
  geom_point(col = "aquamarine4", size = 2, shape = 20); Grafico2



#** PASO 4 **
#Realizar los cálculos de open_rate y ctor para cada journeys_id
#OR: el porcentaje de emails que fueron abiertos por los
#destinatarios sobre el total de emails enviados.
#Click to Open Rate (CTOR): El porcentaje de usuarios que
#recibieron el mail, lo abrieron y realizaron clic en el link deseado.
calculo <- email_campaign[!is.na(email_campaign$total_email_sent),]
calculo <- email_campaign[!is.na(email_campaign$total_email_open),]
calculo <- email_campaign[!is.na(email_campaign$total_email_clicks),]

journey_id <- calculo %>%
  group_by(journeys_ids) %>%
  summarise(total_email_open, total_email_sent, total_email_clicks)

journey_id2 <- calculo %>%
  group_by(journeys_ids) %>%
  summarise(total_email_open=sum(total_email_open), 
            total_email_sent=sum(total_email_sent), 
            total_email_clicks=sum(total_email_clicks)) %>%
  mutate(OR=(total_email_open/total_email_sent)*100) %>%
  mutate(CTOR=(total_email_clicks/total_email_open)*100); journey_id2


#Cual es el OR y CTOR promedio de todas las campañas realizadas?
mean(journey_id2$OR)
mean(journey_id2$CTOR)
#* El promedio del Open Rate (OR) fue de 200.343
#* El promdeio del Click to Open Rate (CTOR) fue de 14,72


#Cuales son las campañas que mejor han performado?
journey_id2 <- journey_id2 %>% 
  arrange(desc(CTOR))
head(journey_id2)
#* Las 6 campañas que mejor han performado (tienen mejor CTOR) son la 88, 50, 76, 73, 66 y la 68
 

#Las campañas que peor performan son aquellas donde más "flag_unsubscribe"
#con valor TRUE existen?
flag <- email_campaign %>% 
  group_by(journeys_ids = email_campaign$journeys_ids) %>%
  summarise(flag_unsubscribe = sum(flag_unsubscribe==TRUE))

flag <- flag %>% 
  arrange(desc(flag_unsubscribe))
head(flag)
#* Las 6 campañas que mas unsubscribe tienen son las 8, 9, 10, 2, 5 y la 14



#** PASO 5 **
#Realizar análisis de los usuarios según su género, realizando un nuevo
#dataset que agregue los datos según género
#Calcular métricas de OR y CTOR para cada género e identificar si se perciben
#diferencias de comportamiento en relación a la tasa de apertura y clics
genero <- email_campaign %>%
  group_by(gender)%>%
  filter(gender != "u")

calculo_genero <- genero[!is.na(genero$total_email_sent),]
calculo_genero <- genero[!is.na(genero$total_email_open),]
calculo_genero <- genero[!is.na(genero$total_email_clicks),]

genero2 <- calculo_genero %>%
  group_by(gender) %>%
  summarise(total_email_open = sum(total_email_open), 
            total_email_sent = sum(total_email_sent), 
            total_email_clicks = sum(total_email_clicks)) %>%
  mutate(OR = (total_email_open/total_email_sent)*100) %>%
  mutate(CTOR = (total_email_clicks/total_email_open)*100); genero2


#Qué sucede con la cantidad promedio de páginas vistas por género?
#Los hombres o las mujeres exhiben un comportamiento diferencial?
genero_pageviews <- genero[!is.na(genero$total_pageviews),]

genero_pageviews2 <- genero_pageviews %>%
  group_by(gender) %>%
  summarise(total_pageviews = sum(total_pageviews)); genero_pageviews2

Grafico3 <- ggplot(genero_pageviews2, aes(x = gender, y = total_pageviews, fill = gender)) +
  geom_col(show.legend = FALSE); Grafico3
#* En promedio los hombres visitan un 45,5% mas las paginas que las mujeres, siendo 48 el
#* número total de visitas a páginas web por mujeres y 88 por hombres
 
#####################################