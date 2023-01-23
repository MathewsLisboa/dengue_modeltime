###### GERANDO IMAGENS PARA O RELATÓRIO ####### 

df_time_series <- ts(exp(df$n), start = c(2017,1), frequency = 52)


autoplot(df_time_series) + 
  labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
  theme_bw()+
  ggsave("figuras_relatorio/serie_nacional_semanal.png", width = 158, height = 93, units = 'mm')

previsoes %>%
  modeltime_accuracy() %>% arrange(mae) %>% select(.model_desc, mae) %>% xtable::xtable(align = 'ccc')


preds <- previsoes %>%  
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df
  ) %>% filter(.model_id==2)

df_time_series <- ts(exp(df$n), start = c(2017,1), frequency = 52)


autoplot(df_time_series,series = 'Observações') + 
  labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
  theme_bw()+
  autolayer(ts(preds$.value, start = c(2021,(52-24)), frequency = 52), series = 'XGBOOST')+
  scale_colour_manual(values = c("XGBOOST"='darkred', "Observações"= 'black'), name='', breaks = c('XGBOOST', 'Observações'))
  ggsave("figuras_relatorio/serie_nacional_teste_semanal.png", width = 158, height = 93, units = 'mm')
  

pontual <- ts(tbl_aux$.value, start = c(2022,1), frequency = 52)  


  
autoplot(df_time_series,series = 'Observações') + 
    labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
    theme_bw()+
    autolayer(pontual,series = 'XGBOOST')+
    scale_colour_manual(values = c("XGBOOST"='darkred', "Observações"= 'black'), name='', breaks = c('XGBOOST', 'Observações'))
    ggsave("figuras_relatorio/serie_nacional_prev_semanal.png", width = 158, height = 93, units = 'mm')



 
    
##### REGIONAL ########
    
    df <- input %>% filter(code_reg == 2) %>%  group_by(ano =epiyear(dias),week = epiweek(dias)) %>% summarise(n= sum(n)) %>% as.data.frame()
    
    df$week[df$week=='53'] <- "52"
    
    df <- df %>% group_by(ano,week) %>% summarise(n= sum(n)) %>% as.data.frame()
    df <- df %>% filter(ano>=(max(ano)-4))
    df$date <-  str_c(as.character(df$ano),as.character(df$week),'1', sep = '-')
    
    df$date <- as.Date(df$date, "%Y-%U-%u")
    
    df$n <- log(df$n)
    
    df <- df %>% select(n,date)
    
    df_time_series <- ts(exp(df$n), start = c(2017,1), frequency = 52)
    
    autoplot(df_time_series) + 
      labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
      theme_bw()
    ggsave("figuras_relatorio/serie_reg_5_semanal.png", width = 158, height = 93, units = 'mm')
    
    previsoes %>%
      modeltime_accuracy() %>% arrange(mae) %>% select(.model_desc, mae) %>% xtable::xtable(align = 'ccc')
    
    
    preds <- previsoes %>%  
      modeltime_forecast(
        new_data    = testing(splits),
        actual_data = df
      ) %>% filter(.model_id==4)
    
    autoplot(df_time_series,series = 'Observações') + 
      labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
      theme_bw()+
      autolayer(ts(exp(preds$.value), start = c(2021,(52-24)), frequency = 52), series = 'Random Forest')+
      scale_colour_manual(values = c("Random Forest"='darkred', "Observações"= 'black'), name='', breaks = c('Random Forest', 'Observações'))
    ggsave("figuras_relatorio/serie_reg_5_semanal_teste.png", width = 158, height = 93, units = 'mm')
    
    
    pontual <- ts(tbl_aux$.value, start = c(2022,1), frequency = 52)
    
    
    
    autoplot(df_time_series,series = 'Observações') + 
      labs(x= 'Semanas Epidemiológicas', y= 'Casos')+
      theme_bw()+
      autolayer(pontual,series = 'XGBOOST')+
      scale_colour_manual(values = c("XGBOOST"='darkred', "Observações"= 'black'), name='', breaks = c('XGBOOST', 'Observações'))
    ggsave("figuras_relatorio/serie_reg_2_semanal_prev.png", width = 158, height = 93, units = 'mm')
    
    