library(rtweet)
library(tidyverse)

options(scipen = 999)

# 0 - APIs y seteo ####

# Creamos una cuenta developer en twitter
# https://developer.twitter.com/en/portal/petition/essential/basic-info

# Creamos una app de developer para poder pedirle data a twitter
# https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html

# Creamos el token de autorización 
# https://docs.ropensci.org/rtweet/reference/create_token.html

api_key <- "asdasdasdasd"
api_secret_key <- "asdasdasdasdasdasd"
access_token<-"asdasdasdasdasd-asdasdasdsad"
access_token_secret<-"asdasdasdasdsad"

token <- create_token(
  app = "Nombre de la app",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

auth_as('create_token')

df_query<-search_tweets(q="causa vialidad", n = 15000)

#escribo csv con la data descargada
df_query<-data.frame(lapply(df_query, as.character), stringsAsFactors = F)

dir.create("./data/clase7")

write.csv(x = df_query, file = "./data/clase7/df_query.csv", fileEncoding = "UTF-8")


# levanto la data y construyo los df con los que voy a trabajar
df_query<-read.csv("./data/clase7/df_query.csv", encoding = "UTF-8")%>%
  mutate(followers_count=as.numeric(followers_count),
         retweet_followers_count=as.numeric(retweet_followers_count))

df_query_interacciones<-df_query%>%
  filter(followers_count>2000)%>%
  # select(screen_name,reply_to_screen_name,retweet_screen_name)%>%
  mutate(from=screen_name, 
         to=retweet_screen_name)%>%
  # mutate(from=user_id,
  #        to=ifelse(is.na(reply_to_user_id), retweet_user_id, reply_to_user_id))%>%
  select(from,to)%>%
  drop_na()%>%
  filter(!from==to)%>%
  group_by(from, to)%>%
  summarise(rt_count=n())

hist(x = df_query$followers_count)

cuentas<-c(unique(df_query_interacciones$from), unique(df_query_interacciones$to))

data_from<-df_query%>%
  select(screen_name, followers_count)

data_to<-df_query%>%
  select(retweet_screen_name, retweet_followers_count)%>%
  rename(screen_name=retweet_screen_name, followers_count=retweet_followers_count)

data<-rbind(data_from, data_to)%>%
  filter(screen_name%in%cuentas)%>%
  group_by(screen_name)%>%
  # filter(followers_count==max(followers_count))
  arrange(desc(followers_count))%>%
  slice(1)

rm(data_from, data_to)


## 1- IGRAPH: objeto especial para redes ####
# install.packages("igraph")
library(igraph)

g<-graph_from_data_frame(df_query_interacciones,
                         directed = TRUE,
                         vertices = data) #NECESITO ARMAR DF CON LA DATA ()

class(g) #chequeamos que el objeto es el correcto

summary(g) #vemos alguna información adicional sobre la nube

is.directed(g) # le preguntamos si es un grafo dirigido

V(g) #Podemos acceder a data sobre los vertices

E(g) #podemos acceder a data sobre las edges

V(g)$name #Podemos pedirle info adicional sobre los diferentes atributos de nuestro gráfico

V(g)$followers_count #Podemos pedirle info adicional sobre los diferentes atributos de nuestro gráfico


## 1.a Ploteo ####
plot(g)

## 1.b Customizo (colores, tamaños) ####

#cambio color de los nodos y quito etiquetas de nombres
plot(g, vertex.label = NA, vertex.color = "red")

#cambio tamaño de los nodos
plot(g, vertex.label=NA, vertex.color = "red", vertex.size=5) 

#cambio tamaño de los nodos en función de un cálculo sobre una variable
plot(g,
     vertex.label=NA,
     vertex.size=sqrt(V(g)$followers_count)/75)

#cambio tamaño de flechas
plot(g,
     vertex.label=NA,
     vertex.size=sqrt(V(g)$followers_count)/75, 
     edge.arrow.size=0.5)

#grafico labels según una variable
label <- ifelse(V(g)$followers_count > quantile(V(g)$followers_count, 0.99), 
                yes = V(g)$name, no = NA)
plot(g,
     vertex.label= label,
     vertex.size=sqrt(V(g)$followers_count)/75, 
     edge.arrow.size=0.5)

#ordeno el plot según un algoritmo de distribución de red y achico flechas
plot(g,
     vertex.label= label,
     vertex.size=sqrt(V(g)$followers_count)/75, 
     edge.arrow.size=0.5,
     layout=layout.kamada.kawai(g))


## 1.c Degrees ####

grado_salida<-degree(g, mode = c("out"))

hist(grado_salida, breaks = 30)
max(grado_salida)


grado_entrada<-degree(g, mode = c("in"))

hist(grado_entrada, breaks = 90)
max(grado_entrada) #si el valor es muy alto, vamos a tener que modificarlo en el plot


label_degree_entrada<-ifelse(grado_entrada > quantile(grado_entrada, 0.99),
                             yes = V(g)$name, no = NA)

label_degree_salida<-ifelse(grado_salida > quantile(grado_salida, 0.99),
                             yes = V(g)$name, no = NA)
  
plot_red_degree<-plot(g,
                      vertex.label= label_degree_entrada,
                      vertex.size=sqrt(grado_entrada),
                      vertex.size=grado_salida,
                      layout=layout.kamada.kawai(g),
                      edge.arrow.size=0.5)


## 1.d Layouts ####
methods(layout)

# Utilicemos el layout "random"
plot(g,
     vertex.label= label_degree_entrada,
     vertex.size=sqrt(grado_entrada),
     edge.arrow.size=0.5,
     layout=layout.random(g))

# Utilicemos el "layout_in_circle"
plot(g,
     vertex.label= label_degree_entrada,
     vertex.size=sqrt(grado_entrada),
     edge.arrow.size=0.5,
     layout=layout_in_circle(g))

# Utilicemos el "layout.fruchterman.reingold"
plot(g,
     vertex.label= label_degree_entrada,
     vertex.size=sqrt(grado_entrada),
     edge.arrow.size=0.5,
     layout=layout.fruchterman.reingold(g))

# Utilicemos el "layout_on_grid"
plot(g,
     vertex.label= label_degree_entrada,
     vertex.size=sqrt(grado_entrada),
     edge.arrow.size=0.5,
     layout=layout_on_grid(g))

# Dejemos que R decida por nosotros

layout_calculado<-layout_nicely(g)

plot(g,
     vertex.label= label_degree_entrada,
     vertex.size=sqrt(grado_entrada),
     edge.arrow.size=0.5,
     layout=layout_calculado)

## 1.e Comunidades ####

comunidad<-walktrap.community(g) #averiguar como reducir cantidad de comunidades

asignacion_comunidad<-membership(comunidad)

plot(comunidad, 
     g, 
     vertex.label=label_degree_entrada, 
     vertex.size=sqrt(grado_entrada), 
     vertex.label.color="white",
     # layout=layout.kamada.kawai(g),
     layout=layout.random(g),
     # vertex.color=V(g)$color, 
     # vertex.frame.color=V(g)$color, 
     edge.arrow.size=.5)

## 1.f Centralidad ####

g.ec <- eigen_centrality(g)
which.max(g.ec$vector)

plot(g, 
     vertex.label=label_degree_entrada, 
     vertex.size=50*(g.ec$vector),
     layout=layout.random(g),
     # layout=(layout.kamada.kawai(g)),
     edge.arrow.size=.5)

## 1.f Más data ####

# todas las conexiones de un nodo
incident(g, 'mistelchipa', mode = c('all'))

# identificamos vecinos de una cuenta 
neighbors(g, 'LunaInsurrecta', mode = c('all'))

## 1.e Exportamos ####
dir.create("./plots")
dir.create("./plots/clase7")

jpeg(file = "./plots/clase7/Análisis de redes 2.jpg", 
     width = 4000, 
     height = 4000, 
     units = "px", 
     pointsize = 75, 
     quality = 95, 
     bg = "white", 
     res = NA, 
     family = "", 
     restoreConsole = TRUE, 
     type = c("windows", "cairo"))

plot(comunidad, 
     g, 
     vertex.label=label_degree_entrada, 
     vertex.size=sqrt(grado_entrada), 
     vertex.label.color="white",
     # layout=layout.kamada.kawai(g),
     layout=layout.random(g),
     # vertex.color=V(g)$color, 
     # vertex.frame.color=V(g)$color, 
     edge.arrow.size=.5)

dev.off()

## 2.a GGally (para usar ggnet2) ####
"https://briatte.github.io/ggnet/"

# install.packages("GGally")
# install.packages("network")
library(GGally)
library(network)
library(tidytext)
library(stopwords)

colnames(df_query)
unique(df_query$verified)

# vamos a obtener los HT de la conversación
hashtags<-df_query%>%
  # filter(verified=="TRUE")%>%
  mutate(text=str_replace_all(text, "\\#", "ABC"))%>%
  unnest_tokens(input=text, output=word)%>%
  filter(str_detect(word, "abc"))%>%
  mutate(word=str_replace_all(word, "abc", "\\#"))%>%
  group_by(screen_name, word, followers_count)%>%
  summarise(freq=n())

# nos quedamos con los HT más frecuentes
hashtags_mas_usados<-hashtags%>%
  group_by(word)%>%
  summarise(cantidad=n())%>%
  slice_max(n = 15, order_by=cantidad)

# nos quedamos con las cuentas más grandes que usaron los HT más frecuentes
hashtags_plot<-hashtags%>%
  filter(word%in%c(unique(hashtags_mas_usados$word)))%>%
  ungroup()%>%
  slice_max(n=50, order_by=followers_count)

# generamos un objeto network
network<-network(hashtags_plot)

# agregamos un parámetro para el color
network %v% "color" <- ifelse(
  lapply(network$val, "[[", 2) %>% as.character()%in%c(unique(hashtags_mas_usados$word)),
  "red", "green")
                                                       
# ploteamos
plot_redes<-ggnet2(
  network,
  mode = "fruchtermanreingold",
  color = "grey",
  size = "degree",
  arrow.size = 6,
  arrow.gap = 0.02,
  max_size = 8,
  legend.position = "none",
  layout.exp = 0,
  label = T,
  label.size = ,
  label.color = "black",
  label.alpha = .8,
  node.color = "color",
  node.alpha = 0.5
  )

# podemos customizar el plot con funciones de ggplot
plot_redes<-plot_redes+
  labs(title = "Hashtags más usados en la conversación")+
  theme(title = element_text(size = 15))

plot_redes

# guardamos
ggsave(plot_redes, 
       filename = "./plots/clase7/hashtags_mas_usados.png",
       device = "png")

## 2.b GGraph ####
"https://www.data-imaginist.com/2017/ggraph-introduction-layouts/"

## 3.a - Interactividad: networkD3 ####
"https://christophergandrud.github.io/networkD3/"

# install.packages("networkD3")
library(networkD3)

data_nueva<-graph_from_data_frame(hashtags_plot%>%
                                    rename(from=screen_name, to=word))

wc<-cluster_walktrap(data_nueva)
members<-membership(wc)
  
data_nueva<-igraph_to_networkD3(data_nueva, group = members)

forceNetwork(Links = data_nueva$links, Nodes = data_nueva$nodes, 
             NodeID = 'name', Group = 'group', fontSize = 20)

## 3.b Interactividad: visNetwork ####
"https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html
"
## 4- Gephi para salir del apuro (SI LLEGAMOS) ####
write.graph(simplify(g), file = "./data/clase7/query_para_gephi.gml", format = "gml")

## 5- Nueva API ####

base<-readRDS("./data/clase7/query_humedales.rds")

glimpse(base)

base2<-base%>%
  unnest_wider(retweeted_status, names_sep = "-", transform = c)

glimpse(base2)

unique(base2$`retweeted_status-retweeted`)
