load(file = 'consolidado/lista_df_semanal.RDATA')
load(file = 'consolidado/lista_precisoes_UF_semanal.RDATA')
load(file = 'consolidado/lista_retreino_semanal.RDATA')

ufs <- names(lista_df_semanal)
ufs[21]

df <- lista_df_semanal[[21]] %>% filter(year(date)>=2017)

df_time_series <- ts(exp(df$n), start = c(2017,1), frequency = 52)

autoplot(df_time_series) + 
  labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
  theme_bw()
ggsave("figuras_relatorio/serie_uf_21_semanal.png", width = 158, height = 93, units = 'mm')

lista_precisoes_UF_semanal[[21]] %>%
  modeltime_accuracy() %>% arrange(mae) %>% select(.model_desc, mae) %>% xtable::xtable(align = 'ccc')


preds <- lista_precisoes_UF_semanal[[21]] %>%  
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>% filter(.model_id==5)

autoplot(df_time_series,series = 'Observações') + 
  labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
  theme_bw()+
  autolayer(ts(exp(preds$.value), start = c(2021,(52-24)), frequency = 52), series = 'STL ARIMA')+
  scale_colour_manual(values = c("STL ARIMA"='darkred', "Observações"= 'black'), name='', breaks = c('STL ARIMA', 'Observações'))
ggsave("figuras_relatorio/serie_uf_21_semanal_teste.png", width = 158, height = 93, units = 'mm')


tbl_aux <- lista_retreino_semanal[[21]] %>% modeltime_forecast(h=52, actual_data = df)
                                  
tbl_aux <- tbl_aux %>% filter(.model_id == 5) %>% mutate(.value = exp(.value),
                                                          .conf_lo=exp(.conf_lo),
                                                          .conf_hi=exp(.conf_hi),
                                                          territorio = 'BRASIL')
                                  
pontual <- ts(tbl_aux$.value, start = c(2022,1), frequency = 52)



autoplot(df_time_series,series = 'Observações') + 
  labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
  theme_bw()+
  autolayer(pontual,series = 'STL ARIMA')+
  scale_colour_manual(values = c("STL ARIMA"='darkred', "Observações"= 'black'), name='', breaks = c('STL ARIMA', 'Observações'))
ggsave("figuras_relatorio/serie_uf_21_semanal_prev.png", width = 158, height = 93, units = 'mm')

