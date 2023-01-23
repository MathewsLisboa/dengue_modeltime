codigos <-  names(lista_dfs_mun_semanal)

sample(codigos,1)

sp <- "355030"
porto <- '431490'
igaratinga <- '313020'

df <- lista_dfs_mun_semanal[[igaratinga]]

df_time_series <- ts(df$n, start = c(2017,1), frequency = 52)

autoplot(df_time_series) + 
  labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
  theme_bw()
ggsave("figuras_relatorio/serie_mun_igaratinga_semanal.png", width = 158, height = 93, units = 'mm')



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

previsoes %>%
  modeltime_accuracy() %>% arrange(mae) %>% select(.model_desc, mae) %>% xtable::xtable(align = 'ccc')

id_model <- previsoes %>% modeltime_accuracy() %>% filter(mae==min(mae)) %>% .$.model_id

retreino <- previsoes %>%
  modeltime_refit(data = df)

tbl_aux <- retreino %>% modeltime_forecast(h=52, actual_data = df)

tbl_aux <- tbl_aux %>% filter(.model_id == id_model) %>% mutate(code_mun = '00')
tbl_aux    


preds <- previsoes %>%  
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>% filter(.model_id==id_model)

autoplot(df_time_series,series = 'Observações') + 
  labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
  theme_bw()+
  autolayer(ts(preds$.value, start = c(2021,(52-24)), frequency = 52), series = 'XGBOOST')+
  scale_colour_manual(values = c("XGBOOST"='darkred', "Observações"= 'black'), name='', breaks = c('XGBOOST', 'Observações'))
ggsave("figuras_relatorio/serie_mun_igaratinga_semanal_teste.png", width = 158, height = 93, units = 'mm')


pontual <- ts(tbl_aux$.value, start = c(2022,1), frequency = 52)


autoplot(df_time_series,series = 'Observações') + 
  labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
  theme_bw()+
  autolayer(pontual,series = 'XGBOOST')+
  scale_colour_manual(values = c("XGBOOST"='darkred', "Observações"= 'black'), name='', breaks = c('XGBOOST', 'Observações'))
ggsave("figuras_relatorio/serie_mun_igaratinga_semanal_prev.png", width = 158, height = 93, units = 'mm')










