# MÓDULO VI - TEXT ANALYTICS
#* MARIANA BENJUMEA PALACIO - 930

#En este ejercicio realizamos un análisis de la conversación en Twitter alrededor de la marca Zara. 
#Realizaremos un análisis exploratorio utilizando las técnicas vistas en clase. Finalmente, aplicaremos 
#un análisis de sentimiento y modelado de tópicos, que nos permitan profundizar en los documentos 
#(tweets) extraídos y en la conversación alrededor de la marca.

#Para completar el ejercicio deberan cargar las siguientes librerías:
#tidyverse, stringr, rtweet, readtext, tidytext, udpipe, quanteda, syuzhet, topicmodels
library(tidyverse)
library(stringr)
library(readtext)
library(tidytext)
library(udpipe)
library(quanteda)
library(syuzhet)
library(topicmodels)
library(dplyr)
library(readr)
library(rtweet)
library(wordcloud)
library(psych)
library(ggplot2)
library(corrplot)

#PISTA: Las librerias fueron utilizadas en los ejercicios prácticos del módulo de 
#Text Analytics.Pueden revisar esos script como referencia para esta tarea


#** PASO 1 **
#Realizamos una búsqueda en Twitter utilizando la query de búsqueda "Zara". Fijamos los parámetros:
# n= 18000 (límite máximo permitido de descarga de registros, es posible que se crearan menos tweets en el intervalo temporal seleccionado)
# include_rts= FALSE
# lang= "es"
#PISTA: consulta la ayuda de la función search_tweets
?search_tweets
Tweets_Zara <- search_tweets(q="Zara", n = 18000, 
                             include_rts = FALSE, 
                             lang= "es")
View(Tweets_Zara)


#Inspecciona el dataframe
#El dataset descargado contiene 90 columnas, pero no nos interesan todas. Selecciona las columnas:
# created_at, screen_name, text, favorite_count, retweet_count, lang, status_url, name, location, 
# description, followers_count, friends_count, statuses_count
Tweets_Zara2 <- Tweets_Zara %>%
  select(created_at, screen_name, text, favorite_count, retweet_count, lang, status_url,
         name, location, description, followers_count, friends_count, statuses_count)
view(Tweets_Zara2)


#Convierte el texto en minúsculas. PISTA: utiliza la librería stringr
#** str_to_lower(Data_Tweets_Zara, locale = "es")
#** Use esta función y se bloqueo el programa (por la cantidad de datos, mi pc no lo
#** soporta), así que la solución la realice pasando la columna deaseada a minucsula:
str(Tweets_Zara2)
Tweets_Zara2$text <- str_to_lower(Tweets_Zara2$text, locale = "es")
view(Tweets_Zara2)

#* Otra opción mas fácil y rápida es usar la función base mutate_if junto con tolower:
Tweets_Zara2 <- mutate_if(Tweets_Zara2, is.character, tolower)


#Convierte la fecha de creación en Date
Tweets_Zara2$created_at <- Tweets_Zara2$created_at %>% 
  as.Date("%Y-%m-%d")
str(Tweets_Zara2$created_at)


#Sustituye las letras acentuadas por letras sin acentuar. PISTA: utiliza la librería stringr
#* str_remove_all(Data_Tweets_Zara, pattern = "[[:punct:]]")
#* Esta función no sa use porque me quita todos los signos de puntuación, no solo las "´"
#* str_remove_all(Data_Tweets_Zara, pattern = "´")
#* Use esta función y se bloqueo el programa (por la cantidad de datos, mi pc no lo
#* soporta), así que la solución la realice quitando solo en la columna deseada las 
#* letras acentuadas con la funcion base chartr:
Tweets_Zara2$text <- chartr("áéíóúü","aeiouu", Tweets_Zara2$text)
view(Tweets_Zara2)


#Inspecciona de nuevo el dataset con summary, str y head.
summary(Tweets_Zara2)
str(Tweets_Zara2)
head(Tweets_Zara2)


#Verifica que el texto se transformó en minúsculas y que las letras con acento se sustituyeron por letras sin acentuar
#* Uso str_detect para verificar, siendo TRUE o FALSE como respuesta según sea el caso
str_detect(Tweets_Zara2$text, pattern = "áéíóúü")
str_detect(Tweets_Zara2$text, pattern = "[:lower:]")


#¿Cuántos registros (tweets) contiene el dataset?
length(Tweets_Zara2$text)
#* El dataset contiene 7.237 tweets


#Añade una nueva columna al dataset que unifique el volumen total de interacciones
#La columna se debe llamar "interacciones" y se calcula como la suma de favorite_count y retweet_count para cada registro
Tweets_Zara2$interacciones <- NA
Tweets_Zara2$interacciones <- Tweets_Zara2$favorite_count + Tweets_Zara2$retweet_count
view(Tweets_Zara2)



#** PASO 2 ** 
#Analizamos los datos extraídos y respondemos a preguntas sobre los datos

#Visualiza el número de tweets por día. ¿En qué día se crearon más tweets?
Tweets_dia <- table(Tweets_Zara2$created_at); Tweets_dia

Data_Tweets_dia <- as.data.frame(Tweets_dia)
colnames() = c("Fecha", "Freq-Tweets")
Data_Tweets_dia <- Data_Tweets_dia %>% arrange(desc(`Freq-Tweets`)); Data_Tweets_dia

max(Data_Tweets_dia$`Freq-Tweets`)
#* Se crearon mas tweets el día 16 de Diciembre de 2020 con 997 tweets


#Calcula el número total (suma) de interacciones por día. Represéntalo gráficamente
#¿En qué día hubo más interacciones?
Sum_interacciones <- Tweets_Zara2 %>%
  group_by(created_at) %>%
  summarise(interacciones = length(interacciones)); Sum_interacciones

max(Sum_interacciones$interacciones)

Grafico1 <- barplot(Sum_interacciones$interacciones ~ Sum_interacciones$created_at,
                    xlab = "Fecha Tweet",
                    ylab = "Número Tweets",
                    col = hcl.colors(14), 
                    las = 2)
#* Hubieron mas interacciones el día 16 de Diciembre de 2020 con 997 interacciones


#¿Qué cuentas (screen_name) tienen mayor (max) número de followers? Pista, necesito utilizar la columna followers_count
Max_screen <- Tweets_Zara2 %>%
  group_by(screen_name) %>%
  summarise(followers = max(followers_count))

Max_screen_sub <- Max_screen %>%
  filter(followers > 800000) %>%
  arrange(desc(followers)); Max_screen_sub

Grafico2 <- ggplot(Max_screen_sub, 
                   aes(x = screen_name, y = followers, fill = screen_name)) +
  geom_col(show.legend = FALSE) + 
  theme(axis.text.x=element_text(angle=90, hjust=1)); Grafico2
#* Filtre los screen_names por los que tienen mas de 800.000 followers para poder graficar y analizar mejor los datos
#* La cuenta que tiene mayor número de followers es @el_pais con 7.863.190 seguidores y le sigue 
#* la cuenta @revistasemana con 4.569.195 seguidores


#¿Cuál fue el tuit con más retweets? Pista, necesito utilizar la columna retweet_count
Max_tuit <- Tweets_Zara2 %>%
  group_by(text) %>%
  summarise(retweet = max(retweet_count))

Max_tuit_sub <- Max_tuit %>%
  filter(retweet > 21) %>%
  arrange(desc(retweet)); Max_tuit_sub
#* Filtre los tweets por los que tienen mas de 21 retweets para poder analizar mejor los datos
#* El tweet que tiene mayor número de retweets es "en colombia hacen jeans de excelente calidad 
#* y ustedes comprando esas marranadas de zara y bershka. rekpaciten" con 352 retweets


#¿Cuál fue el tuit con más likes? Pista, necesito utilizar la columna favorite_count
Max_like <- Tweets_Zara2 %>%
  group_by(text) %>%
  summarise(like = max(favorite_count))

Max_like_sub <- Max_like %>%
  filter(like > 160) %>%
  arrange(desc(like)); Max_like_sub
#* Filtre los tweets por los que tienen mas de 160 likes para poder analizar mejor los datos
#* El tweet que tiene mayor número de likes es "en colombia hacen jeans de excelente calidad 
#* y ustedes comprando esas marranadas de zara y bershka. rekpaciten" con 4.556 likes



#** PASO 3 **
#Tokenizamos el texto, separándolo en palabras y contando el número de palabras.
#Filtramos menciones y visualizamos hashtags

#Utiliza la función unnest_tokens() de la librería tidytext para tokenizar el texto
#Cuenta el número de palabras y ordenálas en orden descendente según su frecuencia
#PISTA: utiliza el parámetro token= "tweets". Consulta la ayuda de la función unnest_tokens()
?unnest_tokens
Token <- Tweets_Zara2 %>%
  unnest_tokens(input = text, 
                output = "palabra", 
                token = "tweets", 
                drop = FALSE) %>%
  count(palabra) %>%
  arrange(desc(n)); Token
view(Token)


#Excluye menciones del dataframe tokenizado Tweets_Zara_Token.
#PISTA: utiliza filter() junto con str_detect() con pattern = "@[:alnum:]", consulta el script 2_Libreria_RTWEET
Menciones <- Token %>%
  filter(str_detect(palabra, pattern = "@[:alnum:]")); Menciones


#Crea un dataframe que contenga los hashtags. PISTA: consulta el script 2_Libreria_RTWEET
Hashtags <- Token %>%
  filter(str_detect(palabra, pattern = "#[:alnum:]")); Hashtags


#Representamos los hashtags como un wordcloud utilizando la librería wordcloud, que no fue introducida en las sesiones prácticas
#Puedes hacer pruebas y variar los parámetros max.words, min.freq, scale, etc para observar como varía el resultado
Grafico3 <- wordcloud(words = Hashtags$palabra, 
                      freq = Hashtags$n,
                      scale =c(2.8,0.75),
                      min.freq = 4,
                      max.words = 80,
                      random.order = F,
                      rot.per = 0.3,
                      random.color = T,
                      color = brewer.pal(12, "Paired"))



#** PASO 4 **
#Realizamos un análisis de sentimiento utilizando la librería SYUZHET
#A diferencia del script 6_Libreria_SYUZHET, donde aplicamos un análisis de sentimiento por palabra (token),
#en este caso apliqueremos la función get_nrc_sentiment a cada tweet (documento)

#Como el dataset es relativamente grande, en esta sección trabajaremos con una muestra.
#Seleccionamos una muestra de 500 tweets de forma aleatoria utilizando la función sample.
Muestra_Data <- Tweets_Zara2[sample(nrow(Tweets_Zara2), size=500), ]
view(Muestra_Data)


#La función get_nrc_sentiment de la librería Syuzhet permite visualizar las emociones y sentimiento
#Analiza 8 "emociones": anger, anticipation, disgust, fear, joy, sadness, surprise y trust
#Así como la polaridad positivo o negativo.
#Utilizamos la función get_nrc_sentiment() con el parámetro language= "spanish"
Analisis_NRC <- get_nrc_sentiment(char_v = Muestra_Data$text, language = "spanish")


#Inspecciona el resultado utilizando View()
view(Analisis_NRC)


#Unificamos el resultado y el dataframe de partida Muestra Data, utilizando la función cbind()
Analisis_NRC2 <- cbind(Muestra_Data, Analisis_NRC)
view(Analisis_NRC2)


#Inspecciona de nuevo el resultado utilizando summary
#Observa los valores mínimo, máximo y medio para cada una de las 8 emociones y para las columnas negative/positive
head(Analisis_NRC2)
summary(Analisis_NRC2)
#* El valor maximo lo tiene la emoción sadness (tristeza) y la polaridad negative

Analisis_NRC[ , 1:10] <- (scale(Analisis_NRC[ , 1:10]))
Grafico4 <- corrplot(cor(Analisis_NRC[ , 1:10]), order="hclust")


# 1) Calcula la suma total de la columna positive
sum(Analisis_NRC2$positive)
# 2) Calcula la suma total de la columna negative
sum(Analisis_NRC2$negative)
#¿La polaridad de la conversación es positiva o negativa?. PISTA: resta el total negativo al total positivo
polaridad <- sum(Analisis_NRC2$positive) - sum(Analisis_NRC2$negative); polaridad
#* La polaridad de la conversación es positiva (90), por lo que se puede concluir que 
#* el agrado y la percepción de la marca es positiva


#Finalmente podemos analizar el porcentaje de cada emoción en la conversación
#Solución: utilizamos la función prop.table y colSums para obtener el porcentaje de cada emoción
# La función prop.table divide el valor de cada celda entre la suma total de filas y columnas (% de la celda)
# La función colSums() suma el valor de todas las celdas de cada columna (% de la columna)
Analisis_NRC_emotions <- colSums(prop.table(Analisis_NRC2[c("anger", "anticipation", "disgust", "fear",
                                                              "joy", "sadness", "surprise", "trust")]))
sort(Analisis_NRC_emotions*100, decreasing= TRUE)


#Inspeccionamos ejemplos
angry_items <- which(Analisis_NRC2$anger > 0)
Analisis_NRC2[angry_items, "text"]

joy_items <- which(Analisis_NRC2$joy > 0)
Analisis_NRC2[joy_items, "text"]


#Nota: este ejercicio es un ejemplo de cómo trabajar con la librería Syuzhet y realizar un análisis de sentimiento
# En un caso real, se debe analizar y limpiar en profundidad el conjunto de documentos (tuits en este caso),
# por ejemplo eliminando menciones, urls y documentos (tuits) no relevantes del análisis de sentimiento.
Analisis_NRC_limpio <- Analisis_NRC2 %>%
  filter(str_detect(text, pattern = "@[:alnum:]", negate = TRUE))

Analisis_NRC_limpio <- Analisis_NRC_limpio %>%
  filter(str_detect(text, pattern = "\\bhttps://", negate = TRUE))
view(Analisis_NRC_limpio)

str_detect(Analisis_NRC_limpio$text, pattern = "@[:alnum:]")
str_detect(Analisis_NRC_limpio$text, pattern = "\\bhttps://")

disgust_items <- which(Analisis_NRC_limpio$disgust > 0)
disgust_items <- Analisis_NRC2[disgust_items, "text"]
length(disgust_items)

surprise_items <- which(Analisis_NRC_limpio$surprise > 0)
surprise_items <- Analisis_NRC2[surprise_items, "text"]
length(surprise_items)
#* Después de realizar una limpieza quitando las menciones y urls en el data de Analisis de
#* sentimiento quedaron 119 tweets, de los cuales 15 tweets, un 12,6% fueron relacionados al 
#* sentimiento disgust y 17 tweets, un 14,2% fueron relacionados al sentimiento surprise



#** PASO 5 **
#Analizamos el dataset utilizando la libería udpipe.
getwd()
setwd("C:/Users/marib/OneDrive/Documentos/EAE/Customer Analytics/Modulo 6")
#Descargamos y cargamos el modelo para español. 
ud_model <- udpipe_download_model(language = "spanish")
#ud_model <- udpipe_load_model(ud_model$file) #Esta línea no se ejecuta correctamente si existe más de un modelo en el directorio de nuestro ordenador
ud_model <- udpipe_load_model(file= "spanish-gsd-ud-2.5-191206.udpipe") #Al especificar el nombre del modelo a cargar, aseguramos que sí cargue el modelo correctamente


#Lo aplicamos sobre la columna del texto de tuits, generando 14 variables
Tweets_Zara_ud <- udpipe_annotate(ud_model,
                                   x = Tweets_Zara2$text,
                                   parallel.cores = 2)


#Convertimos en data frame. Inspecciona el resultado, revisa las variables generadas por la función udpipe_annotate()
Tweets_Zara_ud <- as.data.frame(Tweets_Zara_ud)
view(Tweets_Zara_ud)


#Observa que los signos de puntuación no han sido eliminados
#Utilizando la columna "token", elimina los signos de puntuación y las menciones
#PISTA: para eliminar signos de puntuación utiliza el patrón "[:punct:]". Revisa la cheatsheet de stringr vista en clase.
Tweets_Zara_ud_limpio <- Tweets_Zara_ud %>%
  filter(str_detect(token, pattern = "@[:alnum:]", negate = TRUE))

Tweets_Zara_ud_limpio <- Tweets_Zara_ud %>%
  filter(str_detect(token, pattern = "[:punct:]", negate = TRUE))

str_detect(Tweets_Zara_ud_limpio$token, pattern = "@[:alnum:]")
str_detect(Tweets_Zara_ud_limpio$token, pattern = "[:punct:]")
view(Tweets_Zara_ud_limpio)


#Analicemos los adjetivos
Adjetivos_Zara <- Tweets_Zara_ud_limpio %>%
  filter(upos == "ADJ") %>%
  count(token) %>%
  arrange(desc(n)); head(Adjetivos_Zara)

Grafico5 <- wordcloud(
  words = Adjetivos_Zara$token, 
  freq = Adjetivos_Zara$n, 
  max.words = 80,
  min.freq = 5,
  scale =c(4.8,0.4),
  random.order = T, 
  rot.per = 0.3, 
  random.color = T,
  color = brewer.pal(12, "Paired"))
#* Los adjetivos que mas se repiten son "zara", "hola" y "mejor". (tener presente que se catalogan
#* algunas palabras de forma errada, como zara en adjetivo)


#Analiza los verbos y representa un wordcloud como hemos hecho en el caso de los adjetivos
#PISTA: utiliza la condición de filtrado upos == "VERB"
Verbos_Zara <- Tweets_Zara_ud_limpio %>%
  filter(upos == "VERB") %>%
  count(token) %>%
  arrange(desc(n)); head(Verbos_Zara)

Grafico6 <- wordcloud(
  words = Verbos_Zara$token, 
  freq = Verbos_Zara$n, 
  max.words = 80,
  min.freq = 5,
  scale =c(4.8,0.4),
  random.order = T, 
  rot.per = 0.3, 
  random.color = T,
  color = brewer.pal(12, "Paired"))
#* Los verbos que mas se repiten son "zara", "comprar" y "hay"
#Nota: observa que "Zara" ha sido incorrectamente clasificado como Adjetivo y como Verbo.
#De la misma forma, otros tokens no fueron clasificados correctamente.
#En un caso real, sería necesario corregir estos defectos en la anotación del dataframe.


#Leemos el resultados de los pasos anteriores
Tweets_Zara_ud2 <- read.csv("Dataset_Zara_ud.csv")
view(Tweets_Zara_ud2)



#** PASO 6 **
#Realizamos un modelado de tópicos utilizando la librería topicmodels
#El objetivo es identificar temas en la conversación en Twitter sobre la marca Zara

#Para ello realizamos los siguientes pasos:
# - Seleccionamos nombres y adjetivos
# - Excluímos palabras muy frecuentes en los documentos pero sin significado relevante,
#   como el término de búsqueda de tuits "Zara" o palabras como "gracias", "por favor", etc.
# - Trabajamos con el id de documento (doc_id) y el lema (token lematizado)

#Nota: la libería topicmodels está construida utilizando objetos del paquete tm. Para poder ejecutar funciones
#de este paquete, debemos transformar en Document Term Matrix (dtm) utilizando la función cast_dtm()
Modelo_Zara <- Tweets_Zara_ud2 %>% 
  filter(upos %in% c("NOUN", "ADJ")) %>% 
  filter(!token %in% c("zara", "gracias", "por", "favor", "vez", "que", "hola", "cosa")) %>%
  select(id = doc_id, word = lemma) %>%
  mutate(id = str_replace_all(id, "doc", "")) %>% 
  count(word, id) %>% 
  cast_dtm(id, word, n)


#Generamos varios modelos, variando el número de temas definido en cada modelo (parámetro k)
#Utilizamos la función LDA() del paquete topicmodels
set.seed(1234)
Modelo_Zara_LDA <- LDA(Modelo_Zara, k = 3, control = list(seed = 1234))
Modelo_Zara_LDA2 <- LDA(Modelo_Zara, k = 5, control = list(seed = 1234))
Modelo_Zara_LDA3 <- LDA(Modelo_Zara, k = 8, control = list(seed = 1234))


#Transformamos en formato tidy (tibble data.frame) utilizando la función tidy()
Zara_topics <- tidy(Modelo_Zara_LDA, matrix = "beta")
Zara_topics2 <- tidy(Modelo_Zara_LDA2, matrix = "beta")
Zara_topics3 <- tidy(Modelo_Zara_LDA3, matrix = "beta")


#Inspecciona los dataframes. Puedes realizar una primera inspección ordenando de forma descendente utilizando la columa beta
Zara_topics <- Zara_topics %>% arrange(desc(beta)); Zara_topics
Zara_topics2 <- Zara_topics2 %>% arrange(desc(beta)); Zara_topics2
Zara_topics3 <- Zara_topics3 %>% arrange(desc(beta)); Zara_topics3


#Seleccionamos los top terms de cada modelo y los visualizamos
## Modelo k=3
Zara_top_terms <- Zara_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta); Zara_top_terms

Zara_top_terms_facet <- Zara_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


## Modelo k=5
Zara_top_terms2 <- Zara_topics2 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta); Zara_top_terms

Zara_top_terms_facet2 <- Zara_top_terms2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


## Modelo k=8
Zara_top_terms3 <- Zara_topics3 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta); Zara_top_terms3

Zara_top_terms_facet3 <- Zara_top_terms3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  scale_x_reordered() #reorder_within y scale_x_reordered permiten ordenar en cada facet (tema)


#Seleccionamos el modelo k=5
#Intenta describir cada uno de los 5 temas identificados, revisando las palabras con mayor probabilidad
#(beta) de pertenecer a cada tema
#PISTA: ejecuta el objeto Zara_top_terms_facet2 para realizar un análisis visual
Zara_top_terms_facet2
#* El analisis de los topicos es un poco complejo, ya que no están muy definidas las categorias 
#* en los 5 temas, pero se puede sacar una pequeña conclusión por cada tema:
#* Tema 1: Calificación de la tienda física 
#* Tema 2: Descripción de la ropa y la fecha de la compra
#* Tema 3: Compra online
#* Tema 4: Compras en navidad
#* Tema 5: Sobre la competencia


#Finalmente, utilizando un wordcloud visualizamos las palabras más relevantes por tópico.
#Por ejemplo, para el tópico número 2
Zara_wordcloud <- Zara_topics2 %>%
  filter(topic == "2") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000); Zara_wordcloud

Grafico7 <- wordcloud(
  words = Zara_wordcloud$term, 
  freq = Zara_wordcloud$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, 
  random.color = T,
  color = brewer.pal(10, "Paired"))

#####################################