# 0 - APIs
options(scipen = 9999)

library(rtweet)
# crear token e importar data (poner un ejemplo pero que no ejecuten, 
# la data ya va a estar subida a /data)

df_query<-search_tweets(q="causa vialidad", n = 15000)

#escrivo csv con la data descargada
df_query<-data.frame(lapply(df_query, as.character), stringsAsFactors = F)

dir.create("./data/clase7")

write.csv(x = df_query, file = "./data/clase7/df_query.csv", fileEncoding = "UTF-8")

df_query<-read.csv("./data/clase7/df_query.csv", encoding = "UTF-8")%>%
  mutate(followers_count=as.numeric(followers_count),
         retweet_followers_count=as.numeric(retweet_followers_count))

# df<-read.csv(paste0("./data/clase6/timelines.csv"), encoding = "UTF-8")
# colnames(df)

df_query_interacciones<-df_query%>%
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

cuentas<-c(unique(df_query_interacciones$from), unique(df_query_interacciones$to))

data_from<-df_query%>%
  select(screen_name, followers_count)

data_to<-df_query%>%
  select(retweet_screen_name, retweet_followers_count)%>%
  rename(screen_name=retweet_screen_name, followers_count=retweet_followers_count)

data<-rbind(data_from, data_to)%>%
  group_by(screen_name)%>%
  # filter(followers_count==max(followers_count))
  arrange(desc(followers_count))%>%
  slice(1)

rm(data_from, data_to)

## 0- PPT

## 1- IGRAPH: objeto especial para redes
library(igraph)

g<-graph_from_data_frame(df_query_interacciones, vertices = data) #NECESITO ARMAR DF CON LA DATA ()

class(g) #chequeamos que el objeto es el correcto

summary(g) #vemos alguna información adicional sobre la nube

is.directed(g) # le preguntamos si es un grafo dirigido

V(g) # Podemos acceder a data sobre los vertices

E(g) #podemos acceder a data sobre las edges

V(g)$name# Podemos pedirle info adicional sobre los diferentes atributos de nuestro gráfico


## 1.a Ploteo 
plot(g)

## 1.b Customizo (colores, tamaños)

plot(g, vertex.label = NA, vertex.color = "red")

V(g)$from # Podemos pedirle info adicional sobre los diferentes atributos de nuestro gráfico

plot(g, vertex.size=df_redes$rt_count)
plot(g, vertex.size=sqrt(V(g)$rt_count)/22)

## 1.c Degrees
## 1.d Layouts
## 1.e Comunidades

## 2.a GGally (para usar ggnet2)
"https://briatte.github.io/ggnet/"

## 2.b GGraph
"https://www.data-imaginist.com/2017/ggraph-introduction-layouts/"

## 3- Interactividad: networkD3 o visnetwork?
"https://christophergandrud.github.io/networkD3/"
"https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html"
## 4- Gephi para salir del apuro (SI LLEGAMOS)