pacman:: p_load(shiny,shinyWidgets,geobr)

##### Filtros iniciais #####

load(file = "consolidado/DENG_UF.RDATA")

ufs <- temp2$state %>% unique()

filtros_lateral_inicial <- function() {
 
  box(class='floating-box',
      selectInput("variable", "Variáveis",
                  choices = ufs,
                  selected = "SP"
      ),
      width = 3
  )
}


#### Filtros mapas #####

filtros_mapas <- function(){
  
  anos_mapas <- c(2010:2021)

  box(class='floating-box',
      pickerInput(inputId = 'anos_mapas','Ano de Infecção',
                  choices = anos_mapas,
                  selected = 2021,
                  options = list('actions-box'=TRUE),
                  multiple = T),
      
      width = 3
  )
  
}

