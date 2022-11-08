#Instalando e importando os pacotes

# install.packages('xgboost')
# install.packages('tidymodels')
# install.packages('modeltime')
# install.packages('tidyverse')
# install.packages('lubridate')
# install.packages('timetk')


library(pacman)

p_load(shiny,ggplot2,readxl,plotly,shinythemes,tidyverse,shinydashboard,sass,leaflet,tmap,shinyWidgets,sp,reshape2,geobr,
       xgboost,tidymodels,modeltime,lubridate,timetk)


gc()
#https://fred.stlouisfed.org/series/PCOPPUSDM

#Importando os dados

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


load("consolidado\\DENG_UF.RDATA")

DENG$code_state <- str_sub(DENG$ID_MN_RESI,1,2)

temp <- DENG %>% group_by(DT_SIN_PRI,code_state) %>% count()

anos <- c(2010:2021)

anos <- as.character(anos)

index <- str_sub(temp$DT_SIN_PRI,start = 1, end=4) %in% anos

temp2 <- temp[index, ]

`%notin%` <- Negate(`%in%`)
temp2 <- temp2 %>% filter(code_state%notin%c("20",'99'))

######## Agora que possuimos um data-frame agregado por estado e também por UF ######
ufs <- temp2$state %>% unique()

## Qual melhor maneira de trablhar, um código pra cada ? ou uma função : Vamos tentar com mato_grosso primeiro

## Vamos brincar de trocar code por abreviação dos estados

states <- read_state(year = 2020)

code_e_abbrev <- data_frame(code_state = as.character(states$code_state),
                            state = states$abbrev_state)

temp2 <- left_join(temp2, code_e_abbrev) 

temp2 <- temp2 %>% as.data.frame() %>%  select(n,DT_SIN_PRI,state)  

save(temp2, file='consolidado/DENG_UF.RDATA')

df <- temp2 %>% filter(state%in%c(ufs)) %>% group_by(year(DT_SIN_PRI),month(DT_SIN_PRI)) %>% summarise(n= sum(n)) %>% as.data.frame()

df$n <- log(df$n)

df$date <-  str_c(as.character(df$`year(DT_SIN_PRI)`),as.character(df$`month(DT_SIN_PRI)`),'1', sep='-')

df$date <- df$date %>% as.Date()

df <- df %>% select(n,date)

df_time_series <- ts(df$n,start=c(2010,1),frequency=12)


splits <- initial_time_split(df, prop = 0.92)

autoplot(df_time_series) +
  labs(x = "Ano", y = "Valor Observado") +
  theme_bw()


decomp.mstl <- decompose(df_time_series,type = "additive")
autoplot(decomp.mstl) +
  labs(x = "Ano") +
  theme_bw() 


recipe_spec <- recipe(n ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  #step_normalize(date_index.num, date_year) %>% 
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice() %>% glimpse()


##### Trienamento de modelos #######

set.seed(123)
modelo_1 <- arima_boost(min_n = 2,learn_rate = 0.015) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(n ~ date, data = training(splits))


#modelo 2 : 
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

#modelo 4 : PROPHET XGBOOST
#model_4 <- prophet_reg() %>%
# set_engine(engine = "prophet_xgboost") %>%
#fit(MRTSSM4453USN ~ date, data = training(splits))

#modelo 4:  Random Forest

model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
  set_engine("randomForest")

modelo_RF <- workflow() %>%
  add_model(spec = rand_forest(trees=500, min_n=45, mode="regression") %>%  set_engine("randomForest")) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% fit(training(splits))

model_spec_glmnet <- linear_reg(penalty = 0.001, mixture = 0.5) %>%
  set_engine("glmnet")

modelo_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
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


#modelo 8
modelo_8 <- workflow() %>%
  add_model(
    spec = prophet_reg(
      seasonality_daily  = FALSE, 
      seasonality_weekly = FALSE, 
      seasonality_yearly = TRUE
    ) %>% 
      set_engine("prophet") ) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

#modelo 9
modelo_9 <- nnetar_reg(mode = 'regression', seasonal_period = 12, epochs = 5, hidden_units = 2, penalty = 0.001) %>%
  set_engine(engine = "nnetar") %>%
  fit(n ~ date, data = training(splits))


# modelo_10 <- prophet_boost(seasonality_daily = T) %>%
#   set_engine(engine = "prophet_xgboost") %>%
#   fit(n ~ date, data = training(splits))


#modelo 10
#modelo_10 <- naive_reg() %>%
# set_engine(engine = "naive") %>%
#  fit(MRTSSM4453USN ~ date, data = training(splits))

#Criando a tabela com os modelos

tabela_de_modelos <- modeltime_table(
  modelo_1,
  modelo_2,
  modelo_3,
  modelo_RF,
  modelo_glmnet,
  modelo_5,
  modelo_6,
  modelo_7,
  modelo_8,
  modelo_9
  # modelo_10
)

#tabela com os modelos
tabela_de_modelos

#Gerando as previs?es para cada uma dos modelos
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

#Tabela com as m?tricas de avalia??o para cada um dos modelos
previsoes %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.round_digits = 4)



teste <-sf:: as_Spatial(df_mapa)


