#### Esse código vai realizar os modelos para UF e retornar as previsões futuras em uma tabela de previsões 
### Dessa forma sendo possível atualização sempre que possível.
### Nesse caso, o código serve para o caso nacional tanto mensal quanto semanal. 


###### Leitura dos pacotes ###########

library(pacman)

p_load(shiny,ggplot2,readxl,plotly,shinythemes,dplyr,shinydashboard,sass,leaflet,tmap,
       shinyWidgets,sp,reshape2,geobr,xgboost,tidymodels,modeltime,lubridate,timetk,reactable,stringr)


library(doParallel)
library(foreach)

##### para isso é preciso carregar dados que sejam agregados nacionalmente
#### e que seja bom que fosse por dia, mas pode ser também no mesmo molde anteriro
### ano, mes, quantidade caso mensal
#### epiano, epiweek, quantidade -- caso semanal

load("consolidado/DENG_UF.RDATA")

##### Para o caso do modelo no formato nacional mensal  ####### 

input <- temp2

df <- input  %>% group_by(ano = year(DT_SIN_PRI),mes = month(DT_SIN_PRI)) %>% summarise(n= sum(n)) %>% as.data.frame()

df$n <- log(df$n)

df$date <-  str_c(as.character(df$ano),as.character(df$mes),'1', sep='-')

df$date <- df$date %>% as.Date()

df <- df %>% select(n,date)

### separando em treino e teste os dados
splits <- initial_time_split(df, prop = 0.92)

### criando receita para modelos de machine learing
recipe_spec <- recipe(n ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())

#### Modelo 1: Arima 
modelo_1 <- arima_boost(min_n = 2,learn_rate = 0.015) %>%
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
modelo_3 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(n ~ date, data = training(splits))

### Modelo 4:  Random Forest
modelo_4 <- workflow() %>%
  add_model(spec = rand_forest(trees=500, min_n=45, mode="regression") %>%  set_engine("randomForest")) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% fit(training(splits))

#modelo 5: ETS

modelo_5 <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(n ~ date, data = training(splits))

#model 6: STLM_ARIMA

modelo_6 <- seasonal_reg() %>%
  set_engine(engine = "stlm_arima") %>%
  fit(n ~ date, data = training(splits))


#model 7: STLM_ETS

modelo_7 <- seasonal_reg() %>%
  set_engine(engine = "stlm_ets") %>%
  fit(n ~ date, data = training(splits))

#modelo 8: NNETAR 

modelo_8 <- nnetar_reg(mode = 'regression', seasonal_period = 12, epochs = 5, hidden_units = 2, penalty = 0.001) %>%
  set_engine(engine = "nnetar") %>%
  fit(n ~ date, data = training(splits))

tabela_de_modelos <- modeltime_table(
  modelo_1,
  modelo_2,
  modelo_3,
  modelo_4,
  modelo_5,
  modelo_6,
  modelo_7,
  modelo_8
)

### fazendo as previsoes para o banco de teste, nesse caso vou avaliar qual acurácia do meu modelo com o ano que deixei de fora
previsoes <- tabela_de_modelos %>%
  modeltime_calibrate(new_data = testing(splits))

### aqui da pra observar as previões em gráficos 

# previsoes %>%
#   modeltime_forecast(new_data = testing(splits),
#                      actual_data = df) %>% 
#   plot_modeltime_forecast(.title = paste0("Treino de Série Período Mensal para: ",ufs[28]," Escala log"),
#                           .legend_max_width = 30)



id_model <- previsoes %>% modeltime_accuracy() %>% filter(mae==min(mae)) %>% .$.model_id

retreino <- previsoes %>%
  modeltime_refit(data = df)

tbl_aux <- retreino %>% modeltime_forecast(h=12, actual_data = df)

tbl_aux <- tbl_aux %>% filter(.model_id == id_model) %>% mutate(.value = exp(.value),
                                                                .conf_lo=exp(.conf_lo),
                                                                .conf_hi=exp(.conf_hi),
                                                                territorio = 'BRASIL')
tbl_nacional_mensal <- tbl_aux


##### Caso semanal #######


input <- temp2

df <- input %>%  group_by(ano =epiyear(DT_SIN_PRI),week = epiweek(DT_SIN_PRI)) %>% summarise(n= sum(n)) %>% as.data.frame()

df$week[df$week=='53'] <- "52"

df <- df %>% group_by(ano,week) %>% summarise(n= sum(n)) %>% as.data.frame()
df <- df %>% filter(ano>= (max(ano)-4) )
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

tbl_aux <- tbl_aux %>% filter(.model_id == 5) %>% mutate(.value = exp(.value),
                                                         .conf_lo=exp(.conf_lo),
                                                         .conf_hi=exp(.conf_hi),
                                                         territorio = 'BRASIL')



tbl_nacional_semanal <- tbl_aux                                    


save( tbl_nacional_semanal ,file= 'consolidado/tbl_nacional_semanal.RDATA')
