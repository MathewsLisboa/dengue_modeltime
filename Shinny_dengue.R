### Carregando bibliotecas ####

library(pacman)

p_load(shiny,ggplot2,readxl,plotly,shinythemes,tidyverse,shinydashboard,sass,leaflet,tmap,shinyWidgets,sp,reshape2,geobr,
       xgboost,tidymodels,modeltime,lubridate,timetk,mapview)

#### Primeiro set o diretório do console para o lugar do arquivo, 
### Vá em Session > Set Working ... > To source file location 


`%notin%` <- Negate(`%in%`)

#### Carregando arquivos #####



#### Banco necessário ######

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("consolidado\\DENG_UF.RDATA")

######## Agora que possuimos um data-frame agregado por estado e também por UF ######
ufs <- temp2$state %>% unique()

#### Carregando funções #####

source('Filtros_Laterais.R', encoding = 'UTF-8')

source('tabs.R', encoding = 'UTF-8')


#### Fazendo as escolhas #####


dbHeader <- dashboardHeader(title = tags$a(href='https://www.paho.org/pt/brasil',
                                           "Casos de Dengue"))

##### Página UI ######

ui <-  tags$html(
  tags$head( 
    tags$style(sass(sass_file('www/styles.scss')))
  ),
  dashboardPage(
    skin = "green",
    dbHeader,
    dashboardSidebar(
      sidebarMenu(
        menuItem("Página Inicial", tabName = "inicial", icon = icon("info-circle")),
        menuItem("Visão Geral", tabName = "analises", icon = icon("chart-line")),
        menuItem("Mapa", tabName = "mapas", icon = icon("globe-americas")),
        menuItem("Sobre",tabName = 'sobre', icon = icon('question-circle')))
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'inicial',
                fluidRow(
                  tabsetPanel(
                    inicial_ui()
                  )
                  
                )),
        
        tabItem(tabName = "analises", fluidRow(
          filtros_lateral_inicial(),
          
          box(class = 'floating-box',
              tabsetPanel(
                          Linha_temporal_ui()),
              width = 12
          ))
          
        ),
        
        tabItem(tabName = 'mapas',fluidRow(
          filtros_mapas(),
          box(class='floating-box',
              tabsetPanel(
                mapa_ui()
              ),width = 9))),
        tabItem(tabName = "sobre", sobre_ui())
        
      )
    )
  )
)





server <- function(input,output){
  
  #### Reactive para os Forecasts #####
  
  df_linha <- reactive({
    
  })
  
  output$linhatemporal <- renderPlotly({
    previsoes <- tabela_de_modelos %>%
      modeltime_calibrate(new_data = testing(splits))
    
    #plotando das previs?es dos modelos com os valores reais
    previsoes %>%
      modeltime_forecast(
        new_data    = testing(splits),
        actual_data = df
      ) %>%
      plot_modeltime_forecast(
        .legend_max_width = 30, # For mobile screens
      )
    
    
  })
  
  
  
  #### Reactive para os mapas #####

  df_mapa <- reactive({
    states <-  read_state(year=2020)
    names(states)[2] <- 'state'
    temp_mapa <- temp2 %>% group_by(ano=year(DT_SIN_PRI),state) %>% summarise(n= sum(n)) %>% as.data.frame()
    temp_mapa2 <- temp_mapa %>% filter(ano%in%input$anos_mapas) 
    df_mapa <- left_join(states, temp_mapa2) %>% select(state,n, geom)
    df_mapa
  })
  
  output$mapa1 <- renderLeaflet({
    map1 <- teste %>% tm_shape()+
      tm_polygons(col = "n", palette = "Reds", title ="Casos de Dengue por UF em 2021")
    
    
    tmap_leaflet(map1,add.titles = F,
                 in.shiny=F)
    
    })
  
  
}



shinyApp(ui = ui, server = server)




states <-  read_state(year=2020)
names(states)[2] <-'state' 
temp_mapa <- temp2 %>% group_by(ano=year(DT_SIN_PRI),state) %>% summarise(n= sum(n)) %>% as.data.frame()
temp_mapa <- temp_mapa %>% filter(ano%in%c(2021)) 
df_mapa <- left_join(states, temp_mapa) %>% select(state,n, geom)

teste <- sf::as_Spatial(df_mapa)

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

map<- ggplot() +
  geom_sf(data=df_mapa, aes(fill=n), color= NA, size=.15) +
  labs(subtitle="", size=8) +
  scale_fill_distiller(palette = "Reds", name="Papinho", limits = c(min(df_mapa$n),max(df_mapa$n)), direction = 1) +
  theme_minimal() +
  no_axis


ggplotly(map)

library(mapview)


