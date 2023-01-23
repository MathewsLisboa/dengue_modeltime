#### Esse código vai realizar os modelos para UF e retornar as previsões futuras em uma tabela de previsões 
### Dessa forma sendo possível atualização sempre que possível.
### Nesse caso, o código serve para casos semanais


###### Leitura dos pacotes ###########

library(pacman)

p_load(shiny,ggplot2,readxl,plotly,shinythemes,dplyr,shinydashboard,sass,leaflet,tmap,shinyWidgets,sp,reshape2,geobr,
       xgboost,tidymodels,modeltime,lubridate,timetk,reactable,stringr)

library(doParallel)
library(foreach)

## colocando o númerod e treads 
n_cores <- detectCores()-1
registerDoParallel(n_cores)

##### Lendo os dados ####

## no meu caso é uma tabela temporária que eu crieie salvei no meu computador
##  a parte importante sobre ela é que tenha as seguintes informações 
## EPI ANO , EPIWEEK, Quantidade de casos ou pelo menos dados agrupados por UF e por dia

## eu uo ufs por sigla, mas pode ser por código sem problemas, desde que,
## seja feito as alterações

load("consolidado/DENG_UF.RDATA")

#salvando as ufs em um array separado

ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO",
         "MA","MG","MS","MT","PA","PB","PE","PI","PR",
         "RJ","RN","RO","RR","RS","SC","SE","SP","TO")

input <- temp2

set.seed(123)


tbl_previsao_models_semanal_uf <- foreach(i = 1:27, .combine = rbind, .packages = c('tidyverse','stringr','lubridate', 'modeltime','xgboost','tidymodels','recipes','timetk')) %dopar% {
  
  df <- input %>% filter(state == ufs[i]) %>%  group_by(ano =epiyear(DT_SIN_PRI),week = epiweek(DT_SIN_PRI)) %>% summarise(n= sum(n)) %>% as.data.frame()
  
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
  
  tbl_aux <- retreino %>% modeltime_forecast(h=24, actual_data = df)
  
  tbl_aux <- tbl_aux %>% filter(.model_id == id_model) %>% mutate(.value = exp(.value),
                                                                  .conf_lo=exp(.conf_lo),
                                                                  .conf_hi=exp(.conf_hi),
                                                                  sigla_uf = ufs[i])
  tbl_aux                                                                                 
  
}



save(tbl_previsao_models_semanal_uf ,file= 'consolidado/tbl_previsao_models_semanal_uf.RDATA')
