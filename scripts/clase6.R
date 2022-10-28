library(tidyverse)
library(lubridate)

# 0 Seteo y levanto data ####

options(scipen = 9999) #elimino notación científica de números

clase<-"clase6" #defino clase

#levanto data

df<-read.csv(paste0("./data/", clase, "/noticias_para_clase.csv"), encoding = "UTF-8")

#exploro

colnames(df)

glimpse(df)

unique(df$diario)
unique(df$seccion)

#acomodo algunas variables

df<-df%>%
  mutate(fecha=ymd_hms(fecha),
         diario=as.factor(diario),
         seccion=as.factor(seccion))

glimpse(df)

# 1 Manipulación de strings (remover signos, acentos) ####

#Antes de arrancar a manipular el df, veamos un poco cómo se comportan los 
#datos con los que vamos a trabajar: los strings

#definamos un string (cadena de caracteres)
marx<-"Sobre la cuestión judía"
webber<-"El político y el científico"
durkheim<-"La división del trabajo social"

#peguemos los strings
todas_las_obras<-str_c(marx, webber, durkheim) 
todas_las_obras

todas_las_obras<-str_c(marx, ",", webber, ",", durkheim)
todas_las_obras

todas_las_obras<-str_c(marx,webber,durkheim, sep = ", ")
todas_las_obras

#contemos la cantidad de caracteres
str_count(todas_las_obras) 
nchar(todas_las_obras)

#llevemos todo a minusculas
todas_las_obras<-str_to_lower(todas_las_obras) 
todas_las_obras

#llevemos todo a mayusculas
todas_las_obras<-str_to_upper(todas_las_obras) 
todas_las_obras

#llevemos todo a sustantivo propio
todas_las_obras<-str_to_title(todas_las_obras) 
todas_las_obras

#existen muchas operaciones para hacer con strings, muchas funciones
# comienzan con str_


# NORMALIZAR TEXTOS 

#vamos a quitar numeros y signos de puntuacion del titulo de las noticias

#install.packages("tm")
library(tm)
#https://tm.r-forge.r-project.org/users.html

df_2<-df%>%
  mutate(titulo=str_to_lower(titulo), #pasamos a minusculas el titulo de las noticias
         titulo=removePunctuation(titulo, ucp=T, preserve_intra_word_dashes=T), # quitamos signos
         titulo=removeNumbers(titulo), #quitamos numeros
         titulo=str_replace_all(titulo, "á", "a"),#reemplazamos acentos
         titulo=str_replace_all(titulo, "é", "e"),#reemplazamos acentos
         titulo=str_replace_all(titulo, "í", "i"),#reemplazamos acentos
         titulo=str_replace_all(titulo, "ó", "o"),#reemplazamos acentos
         titulo=str_replace_all(titulo, "ú", "u"),#reemplazamos acentos
         titulo=str_replace_all(titulo, "ñ", "n"))#reemplazamos enies

#vamos a quitar numeros y signos de puntuacion de los textos de las noticias pero con una mejor forma

# install.packages("stringi")
library(stringi)

df_2<-df_2%>%
  mutate(texto= stri_trans_general(texto, "Latin-ASCII"), #una forma rápida de quitar acentos y ñ
         texto= str_replace_all(texto,"www\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto,"https\\S*", ""), #saco urls con REGEX
         texto= str_replace_all(texto, "[[:punct:]]", " "), #reemplazo puntuaciones con REGEX
         texto= str_replace_all(texto, "[[:digit:]]+", " "), #reemplazo numeros con REGEX
         texto= str_to_lower(texto))

#otra operacion que es conveniente hacer, consiste en eliminar dobles espacios que podrian aparecer

df_2<-df_2%>%
  mutate(titulo=str_squish(titulo),
         texto=str_squish(texto))


# 2 Conteo de palabras ####

#para contar palabras vamos a necesitar pasar el df a formato tidy

# install.packages("tidytext")
library(tidytext)

df_tidy<-df_2%>%
  unnest_tokens(input = texto, output = word) #cada palabra va a ser un token

df_tidy_cantidad<-df_tidy%>%
  group_by(diario, word)%>%
  summarise(cantidad=n())%>%
  arrange(desc(cantidad))

#quitemos palabras comunes que no aportan al analisis
filtro_palabras<-c("de", "la", "el") 

df_tidy_cantidad<-df_tidy_cantidad%>%
  filter(!word%in%filtro_palabras)

#puedo perder mucho tiempo agregando palabras manualmebte al filtro, veamos una libreria
library(stopwords)

stopwords("es")

filtro_palabras<-stri_trans_general(stopwords("es"),"Latin-ASCII")

df_tidy_cantidad<-df_tidy_cantidad%>%
  filter(!word%in%filtro_palabras)

#ploteamos
plot_palabras_frecuentes<-ggplot(df_tidy_cantidad%>%
                                   slice_max(n=15, 
                                             order_by = cantidad),
                                 aes(x=reorder_within(word,cantidad, diario), 
                                     y=cantidad, 
                                     fill=diario))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~diario, scales = "free")+
  scale_x_reordered()+
  coord_flip()+
  labs(title = "Palabras más frecuentes por diario")
plot_palabras_frecuentes

#tambien podemos separar por ngrams (conjuntos de palabras)
df_tidy_2grams<-df_2%>%
  mutate(texto2=removeWords(x = texto, filtro_palabras))%>%#saco palabras comunes
  unnest_tokens(input = texto2, output = bigram, 
                token = 'ngrams', n=2)%>% #cada par de palabras va a ser un token
  group_by(diario, bigram)%>%
  summarise(cantidad=n())%>%
  arrange(desc(cantidad))

# 3 TF_IDF ####

#vamos a tratar de extraer mas info usando 3 indicadores muy comunes en text mining

#TF (term-frecuency): indica la frecuencia de aparición de un término
df_tidy_cantidad_tf<-df_tidy_cantidad%>%
  mutate(total_palabras=sum(cantidad),
         term_freq=cantidad/total_palabras)

plot_palabras_tf<-ggplot(df_tidy_cantidad_tf%>%
                           mutate(diario=as.factor(diario))%>%
                           filter(diario%in%c("ambito", "clarin", "eldestape"))%>%
                           slice_max(n=15, order_by=term_freq),
                         aes(x=reorder_within(word, term_freq, diario), 
                             y=round(term_freq,digits = 3), 
                             color=diario))+
  geom_pointrange(aes(ymin=0, ymax=round(term_freq, digits = 3)))+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~diario, scales = "free")+
  scale_x_reordered()+
  labs(title = "Frecuencia de palabras")
plot_palabras_tf

#hay una mejor forma de obtener este datos y el de otros indicadores:

df_tidy_cantidad_tfidf<-df_tidy_cantidad%>%
  bind_tf_idf(term = word, document = diario, n = cantidad)

# IDF es un indicador que mide la relevancia de la palabra para cada documento, 
# mientras la palabra aparezca en menos documentos (diarios), mayor será su idf y viceversa

# chequeemos si esto ocurre así: si IDF varía en función de en cuántos documentos aparece la palabra,
# debería tener tantos valores de IDF como documentos 

length(unique(df_tidy_cantidad_tfidf$idf))
length(unique(df$diario))

# TF_IDF es un indicador que multiplica la frecuencia del término (tf) con la importancia del término (IDF)

#ploteamos

plot_palabras_idf<-ggplot(df_tidy_cantidad_tfidf%>%
                            mutate(diario=as.factor(diario))%>%
                            filter(diario%in%c("ambito", "clarin", "eldestape"))%>%
                            arrange(desc(idf))%>%
                            slice_head(n=15),
                         aes(x=reorder_within(word, idf, diario), 
                             y=round(tf_idf, digits = 3), 
                             color=diario))+
  geom_pointrange(aes(ymin=0, ymax=round(idf, digits = 3)))+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~diario, scales = "free")+
  scale_x_reordered()+
  labs(title = "Importancia de palabras")
plot_palabras_idf

plot_palabras_tfidf<-ggplot(df_tidy_cantidad_tfidf%>%
                              mutate(diario=as.factor(diario))%>%
                              filter(diario%in%c("ambito", "clarin", "eldestape"))%>%
                              slice_max(n=15, order_by=tf_idf),
                          aes(x=reorder_within(word, tf_idf, diario), 
                              y=round(tf_idf, digits = 3), 
                              color=diario))+
  geom_pointrange(aes(ymin=0, ymax=round(tf_idf, digits = 3)))+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~diario, scales = "free")+
  scale_x_reordered()+
  labs(title = "Representatividad de palabras")
plot_palabras_tfidf

# install.packages("ggpubr")
library(ggpubr)

plot_todos_los_indicadores<-ggarrange(plot_palabras_tf,
                                      plot_palabras_idf,
                                      plot_palabras_tfidf,
                                      ncol = 1,
                                      nrow = 3)
plot_todos_los_indicadores

# 4 Wordcloud ####

# install.packages("ggwordcloud")
library(ggwordcloud)
library(viridis)

df_tidy_cantidad_tfidf_ambito<-df_tidy_cantidad_tfidf%>%
  ungroup()%>%
  filter(diario=="ambito", 
         nchar(word)>4)%>%
  rename(freq=tf)%>%
  select(word, freq)

plot_wc_palabras_ambito<-ggplot(df_tidy_cantidad_tfidf_ambito%>%
                                  slice_max(n=75, 
                                            order_by=freq),
                                aes(label=word, 
                                    size=freq, 
                                    color=freq))+
  geom_text_wordcloud(rm_outside = T, show.legend = T)+
  scale_size(range = c(5,20), breaks = 5)+
  theme_minimal()+
  scale_color_binned(type = "viridis", n.breaks=5, direction=-1)
  # scale_color_viridis(option="inferno", direction = -1)
  # scale_colour_gradientn(colours = c("Red", "yellow", "green"))
  # scale_color_fermenter(palette = "RdYlGn", direction = 1, guide =  "colourbar", n.breaks=7)
plot_wc_palabras_ambito

# devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)

plot_wc_palabras_ambito_silueta<-wordcloud2(data = df_tidy_cantidad_tfidf_ambito,
                                            figPath = paste0("./data/", clase, "/Logos Diarios/ámbito.png"),
                                            maxRotation = 0,
                                            color = "#048BD3", 
                                            size = 0.7)
plot_wc_palabras_ambito_silueta

df_tidy_cantidad_tfidf_clarin<-df_tidy_cantidad_tfidf%>%
  ungroup()%>%
  filter(diario=="clarin", 
         nchar(word)>4)%>%
  rename(freq=tf)%>%
  select(word, freq)

plot_wc_palabras_clarin_silueta<-wordcloud2(data = df_tidy_cantidad_tfidf_clarin,
                                            figPath = paste0("./data/", clase, "/Logos Diarios/Clarin.png"),
                                            maxRotation = 0,
                                            # color = "#FB0424",
                                            color=rep_len(c('Black','Red'),nrow(df_tidy_cantidad_tfidf_clarin)),
                                            size = 0.7)
plot_wc_palabras_clarin_silueta

#para guardar estos plots, hay que "sacar screen" (no funciona del todo bien)
library(webshot)
library(htmlwidgets)

dir.create("./plots")
dir.create("./plots/clase6")

saveWidget(plot_wc_palabras_clarin_silueta, 
           file = paste0("./plots/", clase, "/nube_clarin.html"),
           selfcontained = F)
webshot(url = paste0("./plots/", clase, "/nube_clarin.html"),
        file = paste0("./plots/", clase, "/nube_clarin.png"), 
        delay =10, vwidth = 480, vheight=480)

  
# 5 Sentiment analysis ####

# Hacer sentiments es como pelar una naranja

#levantamos el primer df con palabras preclasificadas

sentiment_kaggle<-read.csv(paste0("./data/", clase, "/sentiment_lexicon_kaggle.csv"), encoding = "UTF-8")

unique(sentiment_kaggle$sentiment)

sentiment_kaggle<-sentiment_kaggle%>%
  mutate(word=stri_trans_general(word, "Latin-ASCII"))

df_tidy_cantidad_sentiment_k<-df_tidy_cantidad%>%
  left_join(sentiment_kaggle)%>%
  # mutate(sentiment=replace_na(sentiment, "neutral"))%>%
  group_by(diario, sentiment)%>%
  summarise(cantidad=n())

plot_sentiment_kaggle<-ggplot(df_tidy_cantidad_sentiment_k%>%
                                filter(!is.na(sentiment)), 
                              aes(diario, cantidad, fill=sentiment))+
  geom_bar(stat="identity", position = "fill")+
  theme_minimal()+
  # scale_fill_manual(values = colores_positividad)+
  scale_y_continuous(labels=scales::percent_format(scale = 1))+
  coord_flip()+
  labs(x="", 
       y="", 
       title = "Positividad de diarios",
       subtitle = "Noticias del mes de julio",
       caption = "Fuente: DLM")
plotly::ggplotly(plot_sentiment_kaggle)

# probemos con otro df 

# https://bibliotecadigital.exactas.uba.ar/download/technicalreport/technicalreport_n00001.pdf

sentiment_liia<-read.csv(paste0("./data/", clase, "/sentiment_lexicon_liia.csv"), encoding = "UTF-8")%>%
  mutate(sentiment=cut(x = mean_likeness,
                       breaks = 5,
                       include.lowest = T,
                       labels = c("Negativo", 
                                  "Algo negativo", 
                                  "Neutral", 
                                  "Algo positivo", 
                                  "Positivo")))%>%
  mutate(word=stri_trans_general(word, "Latin-ASCII"))

df_tidy_cantidad_sentiment_l<-df_tidy_cantidad%>%
  left_join(sentiment_liia)%>%
  mutate(sentiment=as.character(sentiment),
         sentiment=replace_na(sentiment, "Neutral"))%>%
  group_by(diario, sentiment)%>%
  summarise(cantidad=n())

colores_positividad<-c("Positivo"="#38C477",
                       "Algo positivo"="#87EBA8",
                       "Neutral"="#EEEEEE", 
                       "Algo negativo"="#F28268", 
                       "Negativo"="#F2543D")

plot_sentiment_liia<-ggplot(df_tidy_cantidad_sentiment_l, 
                            aes(diario, cantidad, fill=factor(sentiment, levels = c("Negativo", 
                                                                                    "Algo negativo", 
                                                                                    "Neutral", 
                                                                                    "Algo positivo", 
                                                                                    "Positivo"))))+
  geom_bar(stat="identity", position = "fill")+
  theme_minimal()+
  scale_fill_manual(values = colores_positividad)+
  scale_y_continuous(labels=scales::percent_format(scale = 1))+
  coord_flip()+
  labs(x="", 
       y="", 
       fill= "Sentiment",
       title = "Positividad de diarios",
       subtitle = "Noticias del mes de julio",
       caption = "Fuente: DLM")
plotly::ggplotly(plot_sentiment_liia)


#probemos con un paquete

# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
library(syuzhet)

sentiment_syu<-get_nrc_sentiment(char_v = df_2$texto,language = "spanish")

df_tidy_cantidad_sentiment_s<-cbind(df_2, sentiment_syu)%>%
  group_by(diario)%>%
  summarise(positive=sum(positive),
            negative=sum(negative))%>%
  pivot_longer(cols = c("positive", "negative"),names_to = "sentiment", values_to = "cantidad")

plot_sentiment_syu<-ggplot(df_tidy_cantidad_sentiment_s, 
                           aes(diario, cantidad, fill=sentiment))+
  geom_bar(stat="identity", position = "fill")+
  theme_minimal()+
  # scale_fill_manual(values = colores_positividad)+
  scale_y_continuous(labels=scales::percent_format(scale = 1))+
  coord_flip()+
  labs(x="", 
       y="", 
       title = "Positividad de diarios",
       subtitle = "Noticias del mes de julio",
       caption = "Fuente: DLM")
plotly::ggplotly(plot_sentiment_syu)


# 6 Comparación de discursos ####

#vamos a levantar tweets de algunos políticos importantes de Argentina para comparar 
#sus discursos con los discursos de los diarios
timelines_politicos<-read.csv(paste0("./data/", clase, "/timelines.csv"), encoding = "UTF-8")

glimpse(timelines_politicos)

timelines_politicos<-timelines_politicos%>%
  filter(is_retweet=="FALSE" | is_quote== "TRUE")%>%  #me quedo con los tweets que son organicos o rt con texto
  select(created_at, screen_name, is_quote, quoted_name, text, status_url)

#primero probemos con los diarios entre si
# install.packages("quanteda")
# install.packages("quanteda.textstats")
library(quanteda)
library(quanteda.textstats)

corpus_df<-corpus(df_tidy%>%
                    filter(!word%in%filtro_palabras)%>%
                    rename(text=word))

dfm_df_tidy<-dfm(corpus_df)

dfm_df_tidy_by_diario<-dfm_group(dfm_df_tidy,groups = diario)

# para realizar la comparación vamos a utilizar el elemento estadístico "similitud coseno"
# https://www.iteramos.com/pregunta/28221/puede-alguien-dar-un-ejemplo-de-similitud-del-coseno-de-forma-muy-sencilla-y-grafica
# aunque otro elemento podría ser el "índice de jaccard"
# https://www.grapheverywhere.com/algoritmo-de-similaridad-de-jaccard/

df_similitud_diarios<-textstat_simil(dfm_df_tidy_by_diario,
                             method = "cosine", #c("correlation", "cosine", "jaccard", "ejaccard", "dice", "edice", "hamman","simple matching")
                             margin = "documents") %>%#al poner documents comparo entre autores
  as.data.frame()

plot_similitud_diarios<-ggplot(df_similitud_diarios,
                       aes(document1, document2, fill= cosine)) + 
  geom_tile(color="white", stat = )+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12), 
        plot.title = element_text(size = 25))+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  labs(title = "Similitud de discursos",
       subtitle = "Método: similitud coseno")
plot_similitud_diarios

#ahora probemos entre los politicos entre si

timelines_politicos_tidy<-timelines_politicos%>%
  mutate(text=str_replace_all(text,"http\\S*", ""), #regex para quitar links
         text=stri_trans_general(text, "Latin - ASCII"),
         text=str_replace_all(text,"[[:punct:]]", " "),
         text=str_replace_all(text,"[[:digit:]]+", " "),
         text=str_to_lower(text),
         text=str_squish(text))%>%
  unnest_tokens(input = text, output = word)


corpus_timelines<-corpus(timelines_politicos_tidy%>%
                           filter(!word%in%filtro_palabras)%>%
                           rename(text=word))

dfm_timelines_politicos_tidy<-dfm(corpus_timelines)

dfm_timelines_politicos_tidy_by_author<-dfm_group(dfm_timelines_politicos_tidy,groups = screen_name)

df_similitud_politicos<-textstat_simil(dfm_timelines_politicos_tidy_by_author,
                             method = "cosine", #c("correlation", "cosine", "jaccard", "ejaccard", "dice", "edice", "hamman","simple matching")
                             margin = "documents") %>% #al poner documents comparo entre autores
  as.data.frame()

plot_similitud_politicos<-ggplot(df_similitud_politicos,
                               aes(document1, document2, fill= cosine)) + 
  geom_tile(color="white", stat = )+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12), 
        plot.title = element_text(size = 25))+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  labs(title = "Similitud de discursos",
       subtitle = "Método: similitud coseno")
plot_similitud_politicos


# 7 LDA ####

# LDA es un algoritmo de modelado de tópicos. Un tópico es un conjunto de palabras que determinan un tema

# https://www.cs.columbia.edu/~blei/papers/Blei2012.pdf
# https://www.jmlr.org/papers/volume3/blei03a/blei03a.pdf?ref=https://githubhelp.com

# install.packages("topicmodels")
library(topicmodels)

#creamos un DTM

disc_ds<-df_tidy_cantidad%>%
  mutate(id=row_number())%>%
  cast_dtm(id, word, cantidad)

# lda_20<-LDA(disc_ds, k = 20, control = list(seed=1234))

dir.create("./data/clase6/models")
write_rds(lda_20, './data/clase6/models/lda_20.rds')

ap_topics <- tidy(lda_20, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales='free_y') +
  scale_y_reordered() +
  theme_minimal()

# Práctica ####

# 1 Buscar las palabras más usadas por las cuentas de twitter de CFK, MM, HRL y AK

# 2 Buscar las palabras más representativas de las cuentas de twitter de CFK, MM, HRL y AK

# 3 Qué sentimiento prevalece en los tweets de CFK? Y en los de MM?

# 4 Comparar los tweets de los polítios con las noticias de los diarios

# 5 Es posible detectar 5 temas en los tweets de los políticos?

