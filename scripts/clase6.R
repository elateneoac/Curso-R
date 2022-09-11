library(tidyverse)
library(lubridate)

# CREO ARCHIVO PARA USAR EN CLASE ####

archivos<-list.files("./data/clase6") #creo vector con los nombres de archivos a pegar 

base_full<-data.frame() #creo df vacio donde van a ir pegandose los csv

for (archivo in archivos) {
  
  base<-read.csv(paste0("./data/clase6/",archivo), encoding = "UTF-8")
  
  base_full<-rbind(base_full,base)
  
}

unique(base_full$seccion) #chequeo las secciones que pueden ser interesantes

set.seed(1234) #defino semilla
df_para_clase<-base_full%>%
  filter(seccion%in%c("politica","economia","negocios","sociedad"))%>%
  sample_n(size = 1500) #creo sample

write.csv(x = df_para_clase, file =  "./data/clase6/noticias_para_clase.csv",
          fileEncoding = "UTF-8", row.names = F)

df<-read.csv("./data/clase6/noticias_para_clase.csv", encoding = "UTF-8")
#0 SETEO ####

clase<-"clase6"

#levanto data
df<-read.csv(paste0("./data/", clase, "/noticias_para_clase.csv"), encoding = "UTF-8")

#exploro
colnames(df)

glimpse(df)

unique(df$diario)

#acomodo
df<-df%>%
  mutate(fecha=ymd_hms(fecha),
         diario=as.factor(diario),
         seccion=as.factor(seccion))

#1 Manipulación de strings (remover signos, acentos) ####

#definamos un string (cadena de caracteres)
el_profe<-"Joaquin"
la_profe<-"Melina"

les_profes<-str_c(el_profe, la_profe) #si quiero pegar cadenas de caracteres
les_profes<-str_c(el_profe, " y ", la_profe)
les_profes<-str_c(el_profe,la_profe, sep = ",")

str_count(les_profes) #si quiero contar cantidad de caracteres
nchar(les_profes)

les_profes<-str_to_lower(les_profes) #llevar todo a minusculas
les_profes<-str_to_upper(les_profes) #llevar todo a mayusculas
les_profes<-str_to_title(les_profes) #llevar todo a sustantivo propio

#NORMALIZAR TEXTOS

#vamos a quitar numeros y signos de puntuacion del titulo de las noticias
library(tm)

df_2<-df%>%
  mutate(titulo=str_to_lower(titulo),
         titulo=removePunctuation(titulo, ucp=T, preserve_intra_word_dashes=T),
         titulo=removeNumbers(titulo),
         titulo=str_replace_all(titulo, "á", "a"),
         titulo=str_replace_all(titulo, "é", "e"),
         titulo=str_replace_all(titulo, "í", "i"),
         titulo=str_replace_all(titulo, "ó", "o"),
         titulo=str_replace_all(titulo, "ú", "u"),
         titulo=str_replace_all(titulo, "ñ", "n"))

#vamos a quitar numeros y signos de puntuacion de los textos de las noticias pero con una mejor forma
library(stringi)

df_2<-df_2%>%
  mutate(texto= stri_trans_general(texto, "Latin-ASCII"),#una forma rápida de quitar acentos y ñ
         texto= str_replace_all(texto, "[[:punct:]]", " "), #reemplazo puntuaciones
         texto= str_replace_all(texto, "[[:digit:]]+", " "), 
         texto= str_to_lower(texto))

#otra operacion que es conveniente hacer consiste en eliminar dobles espacios que podrian aparecer
df_2<-df_2%>%
  mutate(titulo=str_squish(titulo),
         texto=str_squish(texto))

#2 Conteo de palabras ####

#para contar palabras vamos a necesitar pasar el df a formato tidy
library(tidytext)

df_tidy<-df_2%>%
  unnest_tokens(input = texto, output = word)

df_tidy_cantidad<-df_tidy%>%
  group_by(diario, word)%>%
  summarise(cantidad=n())%>%
  arrange(desc(cantidad))

#quitemos palabras comunes que no aportan al analisis
filtro_palabras<-c("de", "la", "el") #puedo perder mucho tiempo haciendo esto manualmente

df_tidy_cantidad<-df_tidy_cantidad%>%
  filter(!word%in%filtro_palabras)

library(stopwords)

filtro_palabras<-stri_trans_general(stopwords("es"),"Latin-ASCII")

df_tidy_cantidad<-df_tidy_cantidad%>%
  filter(!word%in%filtro_palabras)

#plot
plot_palabras_frecuentes<-ggplot(df_tidy_cantidad%>%
                                   slice_max(n=15, order_by = cantidad),
                                 aes(x=reorder_within(word,cantidad, diario), y=cantidad, fill=diario))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~diario, scales = "free")+
  scale_x_reordered()+
  coord_flip()+
  labs(title = "Palabras más frecuentes por diario")
plot_palabras_frecuentes

#3 TF_IDF ####

#vamos a tratar de extraer mas info usando 3 indicadores muy comunes en text mining

#TF (term-frecuency)
df_tidy_cantidad_tf<-df_tidy_cantidad%>%
  mutate(total_palabras=sum(cantidad),
         term_freq=cantidad/total_palabras)

#4 Wordcloud (con siluetas?) y emojis ####
#5 Sentiment analysis ####
#6 Comparación de discursos ####
#7 LDA? ####