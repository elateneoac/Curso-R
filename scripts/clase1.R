library(haven)
library(tidyverse)

# 1- SETEO ####
#defino la clase para levantar data y guardar plots
clase<-"clase1"

# 2- EMPIEZO A TRABAJAR ####
#levanto data
base<-read_sav(paste0("./data/",clase, "/Base Portenos Octubre 2019.sav"))

#exploro
head(base)
colnames(base)
str(base)
glimpse(base)

#reemplazo values por labels
base2<-base%>%
  mutate(across(!FECHA, as_factor))

# 3- EXPORTO ####
#guardo la data con la extension de archivo que me interese



