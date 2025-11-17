#ids faltantes en abandono que no tienen actividad 
#por mas de un año

marco <- ("bases EFI 2025/bases EFI 2017/marco muestral.dta")
todos_act <- read_dta("bases EFI 2025/TODOS Cdor actividades_EFI2024.dta")

marco$def_abandono_2024 <- 'Si'
todos_act %>% 
   filter(nom_oferta == 'CONTADOR PUBLICO') %>% 
   group_by(numerodoc) %>% 
   summarise(ultima_fecha = as.Date(max(fecha_activ))) %>% 
   left_join(marco[,c('numerodoc', 'def_abandono_2024')], 
             by='numerodoc') %>% 
   mutate(def_abandono_2025 = case_when(
      ultima_fecha <= as.Date("2020-03-31") ~ 2020,
      ultima_fecha <= as.Date("2021-03-31") ~ 2021,
      ultima_fecha <= as.Date("2022-03-31") ~ 2022,
      ultima_fecha <= as.Date("2023-03-31") ~ 2023,
      TRUE ~ 0
   )) %>% 
   filter(is.na(def_abandono_2024), def_abandono_2025!=0) %>% View()
