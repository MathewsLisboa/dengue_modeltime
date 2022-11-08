library(shiny)
library(magick)
#### Tab inicial ####


inicial_ui <- function(){
  url = "https://www.insper.edu.br/wp-content/uploads/2020/07/Organiza%C3%A7%C3%A3o-Mundial-da-Sa%C3%BAde-OPAS-BRASIL.png"
  
  fluidRow(
    solidHeader = F,
    collapsible = T, width = 12,
    column(12, align = "center", 
           tags$div(
             class='floating-box about-text',
             tags$h2('Casos de Dengue por Estados Brasil', class='about-title'),
             img(src=url))
           
    ))
}

#### Linha temporal 
Linha_temporal_ui <- function(){
  tabPanel("Linha Temporal",plotlyOutput('linhatemporal')
           
  )
}

##### Mapas ######

mapa_ui <- function(){
  tabPanel("mapas",
           plotlyOutput("mapa1")
           
  )
}




##### ABA SOBRE  #####
#readLines('www/sobre.txt', encoding='UTF-8')



sobre_ui <- function() {
  texto <- readLines('www/sobre.txt', encoding='UTF-8')
  
  fluidRow(
    solidHeader = F,
    collapsible = T, width = 12,
    column(12, align = "left", 
           tags$div(
             class='floating-box about-text',
             tags$h2('Sobre este trabalho', class='about-title'),
             HTML(texto)
           )
    ))
}


