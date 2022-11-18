#Librerías ####
library(shiny)
library(tidyverse)
library(PASWR2)
df <- CARS2004

#app ####

#UI ####
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

# server ####
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









