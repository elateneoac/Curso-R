library(tidyverse)
library(tidytext)

# CREO ARCHIVO PARA USAR EN CLASE

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

#Manipulación de strings (remover signos, acentos, stopwords) ####
#Conteo de palabras ####
#TF_IDF ####
#Wordcloud (con siluetas?) y emojis ####
#Sentiment analysis ####
#Comparación de discursos ####
#LDA? ####