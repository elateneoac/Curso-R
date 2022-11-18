# Shiny apps

# ¿Qué es Shiny? ####
#install.packages("shiny")
library(shiny)
library(tidyverse)

runExample("01_hello")
runExample ("02_text") 
runExample ("03_reactivity")
runExample ("04_mpg") 
runExample ("07_widgets")
runExample ("10_download")

# Interactividad: Usuario puede cambiar el número de intervalos con un deslizador 

# Estructura de la aplicación ####

ui <- fluidPage(
  "¡Hola mundo!"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)


# UI: Estructura básica ####

ui <- fluidPage(
  titlePanel("Título"),
  sidebarLayout(position = "left", 
                sidebarPanel("panel lateral"),
                mainPanel("panel principal") ) )

server <- function(input, output) {}

shinyApp(ui = ui, server = server)


##  Caso 1: sidebarLayout() ####
ui <- fluidPage(
  titlePanel("Título"),
  sidebarLayout(position = "left", 
                
                sidebarPanel("panel lateral"),
                
                mainPanel("panel principal") ) )

server <- function(input, output) {}

shinyApp(ui = ui, server = server)


## Caso 2: tabsetPanel() ####
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Pestaña 1", "Aqui hay un hola mundo, y se ponen los inputs"),
    tabPanel("Pestaña 2", "lo mismo que en la pestaña 1"),
    tabPanel("Pestaña 3", "Vamos a jugar con más")
  )
)
server <- function(input, output, session) {
}
shinyApp(ui, server)

## Caso 3: estructuras combinadas ####
ui <- fluidPage(
  titlePanel(h1("Título"), windowTitle = "Ejemplo"),
  tabsetPanel(
    tabPanel(title = "Pestaña 1", 
             br(),
             sidebarLayout(
               position = "left", 
               sidebarPanel("panel lateral: parte 1",
                            #podemos agregar divisores
                            hr(),
                            "panel lateral: parte 2"),
               mainPanel("panel principal",
                         #podemos agregar espacios
                         br(),br(),
                         "subtítulo del panel"),
             )),
    
    tabPanel("Pestaña 2", "lo mismo que en la pestaña 1"),
    tabPanel("Pestaña 3", "Vamos a jugar con más")
  )
)
server <- function(input, output, session) {
}
shinyApp(ui, server)

#Podemos chequear otras estructuras en en el link con ejemplos de la ppt

## Caso 4: Custom UI: formato de salida e iconos en UI ####

ui <- fluidPage(
  titlePanel(h1("Título")),
  tabsetPanel(
    tabPanel(title = "Pestaña 1", icon = icon("user"), 
             br(),
             sidebarLayout(
               position = "left", 
               sidebarPanel("panel lateral"),
               mainPanel(
                 p("p crea un parrafo de texto."),
                 p("Un nuevo p() inicia un nuevo párrafo. Puede cambiarse su
        estilo.", style = "font-family: 'times'; font-si16pt"),
                 strong("strong() texto en negrita."),  br(),
                 em("em() texto en cursiva."), br(),
                 code("code() texto con formato de código"),
                 div("div() segmentos de texto con estilo similar y se puede 
          cambiar de color con: 'style = color:blue' ",
                     style = "color:blue"), br(),
                 p("span() es igual a div(), pero trabaja en",
                   span("grupos de palabras", style = "color:orange"),
                   "que pueden estar dentro de un párrafo."))
             )
    ),
    
    tabPanel("Pestaña 2", icon = icon("house"),
             "lo mismo que en la pestaña 1"),
    tabPanel("Pestaña 3", icon = icon("rectangle-list"),
             "Vamos a jugar con más")
  )
)
server <- function(input, output, session) {
}
shinyApp(ui, server)

## Caso 5: widgets ####

#chequeamos la data
library(PASWR2)
df <- CARS2004
head(df)

#incorporamos dos widgets en la UI
ui <- fluidPage(
  titlePanel(h1("Título")),
  tabsetPanel(
    tabPanel(title = "Pestaña 1", icon = icon("user"), 
             br(),
             sidebarLayout(
               position = "left", 
               sidebarPanel("panel lateral",
                            #selectInput
                            selectInput(inputId = "boton_selector",
                                        label = h3("Selecciona el país:"), 
                                        choices = unique(df$country), 
                                        selected = "Belgium"),
                            #sliderInput
                            sliderInput(inputId ="cantidad_poblacion", 
                                        label = h3("Seleccione el tamaño de población:"), 
                                        min = 0, 
                                        max = 100, 
                                        value = 50)),
               mainPanel(
                 p("p crea un parrafo de texto."))
             )
    ),
    
    tabPanel("Pestaña 2", icon = icon("house"),
             "lo mismo que en la pestaña 1"),
    tabPanel("Pestaña 3", icon = icon("rectangle-list"),
             "Vamos a jugar con más")
  )
)
server <- function(input, output, session) {
  output$value <- renderPrint({ input$select })
}
shinyApp(ui, server)

## Caso 6: server ####
library(ggplot2)
#install.packages("DT")
library(DT)

ui <- fluidPage(
  titlePanel(h1("Población y Accidentes vehiculares en países de la Unión Europea",
                style="color:coral;padding-left: 15px; font-family: 'Encode Sans', sans-serif, cursive;")),
  br(),
  tabsetPanel(
    tabPanel(title = "Población", icon = icon("person"), 
             br(),
             sidebarLayout(
               position = "left", 
               sidebarPanel(width = 4,
                            selectInput(inputId = "boton_selector",
                                        label = h3("Seleccione el país:"), 
                                        choices = unique(df$country), 
                                        selected = "Belgium")),

               mainPanel(width = 8,
                 #objeto de la ui
                 h3("Gráfico: Cantidad de habitantes por país", style = "font-family: 'Encode Sans', sans-serif, cursive; font-size:125%;"),
                 plotOutput("grafico_paises"),
                 br(),
                 h3("Tabla: Cantidad de habitantes por país", style = "font-family: 'Encode Sans', sans-serif, cursive; font-size:125%;"),
                 dataTableOutput("tabla_paises"),
                 br())
             )
    ),
    
    tabPanel("Accidientes", icon = icon("car"),
             br(),
             sidebarLayout(
               position = "left", 
               sidebarPanel("panel lateral",
                  sliderInput(inputId ="cantidad_poblacion", 
                         label = h3("Seleccione el tamaño de población:"), 
                         min = 0, 
                         max = 100, 
                         value = 50)),
             mainPanel ("aca va algo")
             )
  )
))
server <- function(input, output, session) {
  
  #grafico de paises en server
  output$grafico_paises <- renderPlot({

  plot_paises <-ggplot(df, 
                       aes(x=reorder(country,population),
                                      y=population, 
                                      fill=population))+
    geom_bar(stat="identity", position = "dodge")+
    coord_flip()+ 
    scale_y_continuous(n.breaks = 10)+
    theme_minimal()+ 
    theme(axis.text.y = element_text(size = 17))+ #agrandamos los numeros
    labs(x="País", y="Población",
         caption = "Elaboración propia a partir de datos de PASWR2",
         fill="Población") +
    scale_fill_gradientn(colours = c("#C86D3C", "#CB7352","#E39E8E", "#F0B7B7"),
                         values = scales::rescale(c(0, 1000, 25000, 50000, 82532)))
  plot_paises

  })  
  
  #tabla paises en server
  output$tabla_paises <- renderDataTable ({
    tabla <- df %>% 
              select(country, population) %>% 
              arrange(desc(population)) %>% 
              datatable(options = list(pageLength = 5, dom = 'tip'))
    tabla
    
  })
  
  
  
}
shinyApp(ui, server)

## Caso 7: reactividad ####

ui <- fluidPage(
  titlePanel(h1("Población y Accidentes vehiculares en países de la Unión Europea",
                style="color:coral;padding-left: 15px; font-family: 'Encode Sans', sans-serif, cursive;")),
  br(),
  tabsetPanel(
    tabPanel(title = "Población", icon = icon("person"), 
             br(),
             sidebarLayout(
               position = "left", 
               sidebarPanel(width = 4,
                            selectInput(inputId = "boton_selector",
                                        label = h3("Seleccione el país:"), 
                                        choices = unique(df$country), 
                                        selected = "Belgium")),
               
               mainPanel(width = 8,
                         #objeto de la ui
                         h3("Gráfico: Cantidad de habitantes por país", style = "font-family: 'Encode Sans', sans-serif, cursive; font-size:125%;"),
                         plotOutput("grafico_paises"),
                         br(),
                         h3("Tabla: Cantidad de habitantes por país", style = "font-family: 'Encode Sans', sans-serif, cursive; font-size:125%;"),
                         dataTableOutput("tabla_paises"),
                         br())
             )
    ),
    
    tabPanel("Accidientes", icon = icon("car"),
             br(),
             sidebarLayout(
               position = "left", 
               sidebarPanel("panel lateral",
                            sliderInput(inputId ="cantidad_poblacion", 
                                        label = h3("Seleccione el tamaño de población:"), 
                                        min = 0, 
                                        max = 100, 
                                        value = 50)),
               mainPanel ("aca va algo")
             )
    )
  ))
server <- function(input, output, session) {
  
  #agregamos reactividad
  df_filtrado <- reactive ({
    df_filtrado <- df %>% filter(country %in% c(input$boton_selector))
    
  })
  
  
  
  #grafico de paises en server
  output$grafico_paises <- renderPlot({
    
    plot_paises <-ggplot(df_filtrado(), 
                         aes(x=reorder(country,population),
                             y=population, 
                             fill=population))+
      geom_bar(stat="identity", position = "dodge")+
      coord_flip()+ 
      scale_y_continuous(n.breaks = 10)+
      theme_minimal()+ 
      theme(axis.text.y = element_text(size = 17))+ #agrandamos los numeros
      labs(x="País", y="Población",
           caption = "Elaboración propia a partir de datos de PASWR2",
           fill="Población") +
      scale_fill_gradientn(colours = c("#C86D3C", "#CB7352","#E39E8E", "#F0B7B7"),
                           values = scales::rescale(c(0, 1000, 25000, 50000, 82532)))
    plot_paises
    
  })  
  
  #tabla paises en server
  output$tabla_paises <- renderDataTable ({
    tabla <- df_filtrado() %>% 
      select(country, population) %>% 
      arrange(desc(population)) %>% 
      datatable(options = list(pageLength = 5, dom = 'tip'))
    tabla
    
  })
  
  
  
}
shinyApp(ui, server)


## Caso 8: condicionales y reactividad ####
library(RColorBrewer)

ui <- fluidPage(
  titlePanel(h1("Población y Accidentes vehiculares en países de la Unión Europea",
                style="color:coral;padding-left: 15px; font-family: 'Encode Sans', sans-serif, cursive;")),
  br(),
  tabsetPanel(
    tabPanel(title = "Población", icon = icon("person"), 
             br(),
             sidebarLayout(
               position = "left", 
               sidebarPanel(width = 4,
                            selectInput(inputId = "boton_selector",
                                        label = h3("Seleccione el país:"), 
                                        choices = c("Todos", unique(levels(df$country))), 
                                        selected = "Todos")),
               
               mainPanel(width = 8,
                         #objeto de la ui
                         h3("Gráfico: Cantidad de habitantes por país", style = "font-family: 'Encode Sans', sans-serif, cursive; font-size:125%;"),
                         plotOutput("grafico_paises"),
                         br(),
                         h3("Tabla: Cantidad de habitantes por país", style = "font-family: 'Encode Sans', sans-serif, cursive; font-size:125%;"),
                         dataTableOutput("tabla_paises"),
                         br())
             )
    ),
    
    tabPanel("Accidientes", icon = icon("car"),
             br(),
             sidebarLayout(
               position = "left", 
               sidebarPanel("panel lateral",
                            sliderInput(inputId ="cantidad_poblacion", 
                                        label = h3("Seleccione el tamaño de población:"), 
                                        min = 0, 
                                        max = 90000, 
                                        value = 40000)),
               mainPanel (
                 h3("Cantidad de vehículos y accidentes", style = "font-family: 'Encode Sans', sans-serif, cursive; font-size:125%;"),
                 plotOutput("grafico_autos")
                 )
             )
    )
  ))
server <- function(input, output, session) {
  
  #agregamos reactividad: pestaña 1
  df_filtrado <- reactive ({
    df_filtrado <- if(input$boton_selector == "Todos"){
                        df_filtrado <- df}
                   else{df_filtrado <- df %>% filter(country == input$boton_selector)}
  })
  
  
  
  #grafico de paises en server
  output$grafico_paises <- renderPlot({
    
    plot_paises <-ggplot(df_filtrado(), 
                         aes(x=reorder(country,population),
                             y=population, 
                             fill=population))+
      geom_bar(stat="identity", position = "dodge")+
      coord_flip()+ 
      scale_y_continuous(n.breaks = 10)+
      theme_minimal()+ 
      theme(axis.text.y = element_text(size = 17))+ #agrandamos los numeros
      labs(x="País", y="Población",
           caption = "Elaboración propia a partir de datos de PASWR2",
           fill="Población") +
      scale_fill_gradientn(colours = c("#C86D3C", "#CB7352","#E39E8E", "#F0B7B7"),
                           values = scales::rescale(c(0, 1000, 25000, 50000, 82532)))
    plot_paises
    
  })  
  
  #tabla paises en server
  output$tabla_paises <- renderDataTable ({
    tabla <- df_filtrado() %>% 
      select(country, population) %>% 
      arrange(desc(population)) %>% 
      datatable(options = list(pageLength = 5, dom = 'tip'))
    tabla
    
  })
  
  #agregamos reactividad: pestaña 2
  df_filtro_poblacion <- reactive ({
    df_filtro_poblacion <- df %>% filter(population <= input$cantidad_poblacion)
  })
  
  #grafico autos
  output$grafico_autos <- renderPlot({
    
    plot_autos <-ggplot(df_filtro_poblacion(), 
                         aes(x=cars,
                             y=deaths, 
                             color=population))+
      geom_point(size=5, alpha=1)+
      theme_minimal()+ 
      theme(axis.text.y = element_text(size = 17))+ #agrandamos los numeros
      labs(x="Cantidad de autos/1000 habitantes", y="Víctimas mortales",
           caption = "Elaboración propia a partir de datos de PASWR2",
           color="Población")+
    viridis::scale_color_viridis(option = "inferno", discrete = F)
    
      plot_autos
    
  })  
  
  
}
shinyApp(ui, server)


