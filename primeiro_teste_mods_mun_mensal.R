#### Lendo bibliotecas ######

library(pacman)

p_load(shiny,ggplot2,readxl,plotly,shinythemes,dplyr,shinydashboard,sass,
       leaflet,tmap,shinyWidgets,sp,reshape2,geobr,
       xgboost,tidymodels,modeltime,lubridate,timetk,reactable,stringr)

#### Lendo dados ######
load("consolidado/todos_dias_caso.RDATA")

load("consolidado/lista_dfs_mun_mensal.RDATA")

library(foreach)
library(doParallel)
## colocando o númerod e treads 
n_cores <- detectCores()-1
registerDoParallel(n_cores)

df <- df %>% group_by(ano,week) %>% summarise(n= sum(n)) %>% as.data.frame()
df <- df %>% filter(ano>=(max(ano)-4))
df$date <-  str_c(as.character(df$ano),as.character(df$week),'1', sep = '-')

#### Salvando dados de todos municipios a tempo mensal ########3
i <- 5570
codigos <- sort(todos_dias_caso$code_mun %>% unique())

df <- todos_dias_caso %>% filter(code_mun ==codigos[i]) %>% group_by(ano = year(dias), mes=month(dias)) %>% summarise(n = sum(n)) %>% as.data.frame()

df$date <-  str_c(as.character(df$ano),as.character(df$mes),'1', sep='-')

df$date <- df$date %>% as.Date()

df <- df %>% select(n,date)

lista_dfs_mun_mensal[[i]] <- df


lista_dfs_mun_mensal <- foreach(i =1:5570,.packages = c('tidyverse','stringr','lubridate'))%dopar%{ 
  df <- todos_dias_caso %>% filter(code_mun ==codigos[i]) %>% group_by(ano = year(dias), mes=month(dias)) %>% summarise(n = sum(n)) %>% as.data.frame()
  
  df$date <-  str_c(as.character(df$ano),as.character(df$mes),'1', sep='-')
  
  df$date <- df$date %>% as.Date()
  
  df <- df %>% select(n,date)
  df
}

names(lista_dfs_mun_mensal) <- codigos

save(lista_dfs_mun_mensal, file = "consolidado/lista_dfs_mun_mensal.RDATA")

load(file = "consolidado/lista_dfs_mun_mensal.RDATA")


lista_dfs_mun_semanal <- foreach(i =1:5570,.packages = c('tidyverse','stringr','lubridate'))%dopar%{ 
  df <- todos_dias_caso %>% filter(code_mun == codigos[i]) %>%  group_by(ano =epiyear(dias),week = epiweek(dias)) %>% summarise(n= sum(n)) %>% as.data.frame()
  
  df$week[df$week=='53'] <- "52"
  
  df <- df %>% group_by(ano,week) %>% summarise(n= sum(n)) %>% as.data.frame()
  df <- df %>% filter(ano>=(max(ano)-4))
  df$date <-  str_c(as.character(df$ano),as.character(df$week),'1', sep = '-')
  
  df$date <- as.Date(df$date, "%Y-%U-%u")
  
  df <- df %>% select(n,date)
  
  df
}

names(lista_dfs_mun_semanal) <- codigos

save(lista_dfs_mun_semanal, file = "consolidado/lista_dfs_mun_semanal.RDATA")


load(file = "consolidado/lista_dfs_mun_semanal.RDATA")



#### Salvando dados de todos munuicipios a tempo de semana epidemiológica ########

df_time_series <- ts(lista_dfs_mun_mensal[[3050]]$n,start=c(2010,1),frequency=12)

forecast:: autoplot(df_time_series) +
  labs(x = "Ano", y = "Valor Observado") +
  theme_bw()


decomp.mstl <- decompose(df_time_series,type = "additive")
forecast::autoplot(decomp.mstl) +
  labs(x = "Ano") +
  theme_bw() 

df <- lista_dfs_mun_mensal[[3050]]
splits <- initial_time_split(df, prop = 0.92)

recipe_spec <- recipe(n ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  #step_normalize(date_index.num, date_year) %>% 
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())


#### MODELO 1:  SARIMA OU ARIMA CLÁSSICO

modelo_1 <- arima_boost(min_n = 2,learn_rate = 0.015) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(n ~ date, data = training(splits))

##### modelo 2 : xgbooost , árvores de decisão  
modelo_2 <- workflow() %>%
  add_model(
    spec = boost_tree(
      mode = "regression"
    ) %>%
      set_engine("xgboost")
  ) %>%
  add_recipe(recipe_spec %>% 
               update_role(date, new_role = "indicator")) %>%
  fit(training(splits)) 



#modelo 3 : PROPHET
modelo_3 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(n ~ date, data = training(splits))

#modelo 4:  Random Forest

modelo_RF <- workflow() %>%
  add_model(spec = rand_forest(trees=500, min_n=45, mode="regression") %>%  set_engine("randomForest")) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% fit(training(splits))

#modelo 5

modelo_5 <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(n ~ date, data = training(splits))

#model 6

modelo_6 <- seasonal_reg() %>%
  set_engine(engine = "stlm_arima") %>%
  fit(n ~ date, data = training(splits))

#model 7

modelo_7 <- seasonal_reg() %>%
  set_engine(engine = "stlm_ets") %>%
  fit(n ~ date, data = training(splits))


#modelo 9

modelo_8 <- nnetar_reg(mode = 'regression', seasonal_period = 12, epochs = 5, hidden_units = 2, penalty = 0.001) %>%
  set_engine(engine = "nnetar") %>%
  fit(n ~ date, data = training(splits))

#Criando a tabela com os modelos

tabela_de_modelos <- modeltime_table(
  modelo_1,
  modelo_2,
  modelo_3,
  modelo_RF,
  modelo_5,
  modelo_6,
  modelo_7,
  modelo_8
)

previsoes <-  tabela_de_modelos %>%
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


#Tabela com as m?tricas de avalia??o para cada um dos modelos
previsoes %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.title = 'TABLE', .round_digits = 4, .searchable = F)

#### Previsões for do período dos dados 

retreino <- previsoes %>%
  modeltime_refit(data = df)

#precvisão gerada pelos modelos para um per?odo de tempo desconhecido
retreino %>%
  modeltime_forecast(h = 12, actual_data = df, new_data   = testing(splits)) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, #For mobile screens
  )



