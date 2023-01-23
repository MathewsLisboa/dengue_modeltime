###### Leitura dos pacotes ###########

library(pacman)

p_load(shiny,ggplot2,readxl,plotly,shinythemes,dplyr,shinydashboard,sass,leaflet,tmap,shinyWidgets,sp,reshape2,geobr,
       xgboost,tidymodels,modeltime,lubridate,timetk,reactable,stringr)

library(doParallel)
library(foreach)

## colocando o númerod e treads 
n_cores <- detectCores()-1
registerDoParallel(n_cores)


##### transformação input #######
todos_dias_caso$code_reg <-  str_sub(todos_dias_caso$code_mun, start = 1, end = 1)

todos_dias_caso$code_uf <-  str_sub(todos_dias_caso$code_mun, start = 1, end = 2)

input <- todos_dias_caso %>% group_by(dias,code_reg) %>% summarise(n = sum(n)) %>% as.data.frame()


codigos_reg <- input$code_reg %>% unique()


set.seed(123)

##### rodando os modelos de fato ######

tbl_previsao_models_semanal_reg <- foreach(i = 1:5, .combine = rbind, .packages = c('tidyverse','stringr','lubridate', 'modeltime','xgboost','tidymodels','recipes','timetk')) %dopar% {
  
  df <- input %>% filter(code_reg == codigos_reg[i]) %>%  group_by(ano =epiyear(dias),week = epiweek(dias)) %>% summarise(n= sum(n)) %>% as.data.frame()
  
  df$week[df$week=='53'] <- "52"
  
  df <- df %>% group_by(ano,week) %>% summarise(n= sum(n)) %>% as.data.frame()
  df <- df %>% filter(ano>=(max(ano)-4))
  df$date <-  str_c(as.character(df$ano),as.character(df$week),'1', sep = '-')
  
  df$date <- as.Date(df$date, "%Y-%U-%u")
  
  df$n <- log(df$n)
  
  df <- df %>% select(n,date)
  
  ### separando em treino e teste os dados
  
  splits <- initial_time_split(df, prop = 0.91)
  
  ### criando receita para modelos de machine learing
  recipe_spec <- recipe(n ~ date, training(splits)) %>%
    step_timeseries_signature(date) %>%
    step_fourier(date, period = 365, K = 5) %>%
    step_dummy(all_nominal())
  
  
  #### Modelo 1: Arima 
  modelo_1 <- arima_boost(min_n = 2,learn_rate = 0.025,seasonal_period = 52) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(n ~ date, data = training(splits))
  
  
  ### Modelo 2: XGBOOST
  
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
  
  ### Modelo 3: PROPHET
  modelo_3 <- prophet_reg(seasonality_weekly = T) %>%
    set_engine(engine = "prophet") %>%
    fit(n ~ date, data = training(splits))
  
  ### Modelo 4:  Random Forest
  modelo_4 <- workflow() %>%
    add_model(spec = rand_forest(trees=500, min_n=45, mode="regression") %>%  set_engine("randomForest")) %>%
    add_recipe(recipe_spec %>% step_rm(date)) %>% fit(training(splits))
  
  ## Modelo %:  ETS 
  
  ## o modelo ETS não é recomendado pro caso semanal
  
  #model 6: STLM_ARIMA
  modelo_6 <- seasonal_reg(seasonal_period_1 = 52) %>%
    set_engine(engine = "stlm_arima") %>%
    fit(n ~ date, data = training(splits))
  #model 7: STLM_ETS
  modelo_7 <- seasonal_reg(seasonal_period_1 = 52) %>%
    set_engine(engine = "stlm_ets") %>%
    fit(n ~ date, data = training(splits))
  
  #modelo 8: NNETAR
  modelo_8 <- nnetar_reg(mode = 'regression', seasonal_period = 52, epochs = 5, hidden_units = 2, penalty = 0.001) %>%
    set_engine(engine = "nnetar") %>%
    fit(n ~ date, data = training(splits))
  
  tabela_de_modelos <- modeltime_table(
    modelo_1,
    modelo_2,
    modelo_3,
    modelo_4,
    modelo_6,
    modelo_7,
    modelo_8
  )
  
  ### fazendo as previsoes para o banco de teste, nesse caso vou avaliar qual acurácia do meu modelo com o ano que deixei de fora
  previsoes <- tabela_de_modelos %>%
    modeltime_calibrate(new_data = testing(splits))
  
  id_model <- previsoes %>% modeltime_accuracy() %>% filter(mae==min(mae)) %>% .$.model_id
  
  retreino <- previsoes %>%
    modeltime_refit(data = df)
  
  tbl_aux <- retreino %>% modeltime_forecast(h=52, actual_data = df)
  
  tbl_aux <- tbl_aux %>% filter(.model_id == id_model) %>% mutate(.value = exp(.value),
                                                                  .conf_lo=exp(.conf_lo),
                                                                  .conf_hi=exp(.conf_hi),
                                                                  codigos_reg = codigos_reg[i])
  tbl_aux                                                                                 
  
}


save(tbl_previsao_models_semanal_reg ,file= 'consolidado/tbl_previsao_models_semanal_reg.RDATA')

