##### Librerias #####

library(tidyverse)
library(DescTools)
library(haven)

###### Funciones ######

fun_abandono_2024 <- function(data){
   data_fil <- data %>% filter(nom_oferta == "CONTADOR PUBLICO")
   if (nrow(data_fil) == 0) return(0)
   #las actividades se definen al finalizar la carrera, por lo que 
   #actividades en los meses 12,3 y 5 son de semestre par y 6,7 y 8 son de semestre impar.
   ultima_fecha <- max(data_fil$fecha_activ, na.rm = TRUE)
   #la variable indica el año de abandono y 0 ni no hay abandono
   case_when(
      ultima_fecha < as.Date("2020-05-31") ~ 2020,
      ultima_fecha < as.Date("2021-05-31") ~ 2021,
      ultima_fecha < as.Date("2022-05-31") ~ 2022,
      ultima_fecha < as.Date("2023-05-31") ~ 2023,
      TRUE ~ 0
   )
}

fun_act_post_abandono <- function(data){
   #funcion para ver si tiene actividad en otra carrera post abandono
   data_fil <- data %>% filter(nom_oferta == "CONTADOR PUBLICO")
   ultima_fecha_cont <- max(data_fil$fecha_activ, na.rm = TRUE)
   data_fil <- data %>% filter(nom_oferta != "CONTADOR PUBLICO")
   ultima_fecha_otro <- max(data_fil$fecha_activ,na.rm = T)
   ifelse(ultima_fecha_cont>=ultima_fecha_otro | is.na(ultima_fecha_otro), 0, 1)
   #posteriormente hay que decir que si no abandono sea NA
}

fun_eventos <- function(data, fecha_ini, fecha_fin){
   data_fil <- data %>% 
      filter(nom_oferta == "CONTADOR PUBLICO",
             as.Date(fecha_activ) >= fecha_ini,
             as.Date(fecha_activ) <= fecha_fin)
   nrow(data_fil)
}

fun_aprobado <- function(data, fecha_ini, fecha_fin){
   data_fil <- data %>% 
      filter(nom_oferta == "CONTADOR PUBLICO",
             as.Date(fecha_activ) >= fecha_ini,
             as.Date(fecha_activ) <= fecha_fin)
   sum(data_fil$aprobada)
}

fun_prom_aprobado <- function(data, fecha_ini, fecha_fin){
   # promedio de nota aprobacion al la fecha (diciembre 2023)
   data_fil <- data %>% 
      filter(nom_oferta == "CONTADOR PUBLICO",
             as.Date(fecha_activ) >= fecha_ini,
             as.Date(fecha_activ) <= fecha_fin,
             aprobada==1)
   mean(data_fil$nota, rm.na=T) %>% round(2)
}

fun_prom_general <- function(data, fecha_ini, fecha_fin){
   # promedio de nota general al la fecha (diciembre 2023)
   data_fil <- data %>% 
      filter(nom_oferta == "CONTADOR PUBLICO",
             as.Date(fecha_activ) >= fecha_ini,
             as.Date(fecha_activ) <= fecha_fin)
   mean(data_fil$nota, rm.na=T)%>% round(2)
}

##### Datos ######

todos_act <- read_dta("bases EFI 2025/TODOS Cdor actividades_EFI2024.dta")
todos_form <- read_dta("bases EFI 2025/TODOS Cdor form_ingreso_ EFI2024.dta")
todos_inscr <- read_dta("bases EFI 2025/datos_persona Cdor_id_numerodoc_EFI2024.dta")

##### Limpieza ######

df_1 <- todos_act %>%
   group_by(id) %>%
   summarise({
      df <- cur_data()
      tibble(
         numerodoc = Mode(numerodoc),
         num_oferta = 907,
         nom_oferta = 'CONTADOR PUBLICO',
         in_administracion = ifelse(nrow(filter(df,codigo_servicio==1061))==0,0,1),
         in_economia = ifelse(nrow(filter(df,codigo_servicio==1062))==0,0,1),
         eventos_tot = fun_eventos(cur_data(), -Inf, Inf),
         eventos_2019_1 = fun_eventos(df, 
                                      as.Date('2019-04-01'), 
                                      as.Date('2019-09-30')),
         eventos_2019_2 = fun_eventos(df, 
                                      as.Date('2019-10-01'), 
                                      as.Date('2020-03-31')),
         eventos_2020_1 = fun_eventos(df, 
                                      as.Date('2020-04-01'), 
                                      as.Date('2020-09-30')),
         eventos_2020_2 = fun_eventos(cur_data(), 
                                      as.Date('2020-10-01'), 
                                      as.Date('2021-03-31')),
         eventos_2021_1 = fun_eventos(df, 
                                      as.Date('2021-04-01'), 
                                      as.Date('2021-09-30')),
         eventos_2021_2 = fun_eventos(df, 
                                      as.Date('2021-10-01'), 
                                      as.Date('2022-03-31')),
         eventos_2022_1 = fun_eventos(df, 
                                      as.Date('2022-04-01'), 
                                      as.Date('2022-09-30')),
         eventos_2022_2 = fun_eventos(df, 
                                      as.Date('2022-10-01'), 
                                      as.Date('2023-03-31')),
         eventos_2023_1 = fun_eventos(df, 
                                      as.Date('2023-04-01'), 
                                      as.Date('2023-09-30')),
         eventos_2023_2 = fun_eventos(df, 
                                      as.Date('2023-10-01'), 
                                      as.Date('2024-03-30')),
         promedio_aprobaciones = fun_prom_aprobado(df, -Inf, Inf),
         promedio_general = fun_prom_general(df, -Inf, Inf),
         abandono = fun_abandono_2024(df),
         act_post_abandono = fun_act_post_abandono(df)
      )
   })

# se va a poner informacion desagregada para las materias comunes para economia y contador publico
actividades = todos_act  %>%  
  filter(nom_oferta == "CONTADOR PUBLICO" & codigo_mat %in% c(3505, 3462, 3539, 2573, 3378,3543)) %>%    
  mutate(carrera = 1) %>% 
  pivot_wider(names_from = "nom_oferta", values_from = "carrera", values_fill = 0) %>% 
  group_by(id, codigo_mat, fecha_activ) %>% 
  mutate(oferta = max(`CONTADOR PUBLICO`)) %>%   
  slice(1) %>% 
  group_by(id, codigo_mat) %>% 
  arrange(fecha_activ, .by_group = T) %>%
  summarise(
    fecha_activ_inicial = first(fecha_activ),
    fecha_activ_final = last(fecha_activ),
    cursadas = n(),
    aprobada_act = last(aprobada),
    oferta = max(oferta)) %>% 
   ungroup() %>%  
   mutate(anio_aprobada = if_else(aprobada_act==1, fecha_activ_final,"no"))
## me creo una variable fecha_activ_final que nos indica la ultima vez que un estudiante interactuo con cada materia 
## mientras que cursadas dice las veces que la curso
## aprobada_act si la aprobo o no 

act_wide <- actividades %>%
   group_by(id, codigo_mat) %>%
   mutate(n = row_number()) %>% 
   ungroup() %>%
   pivot_wider(
      names_from = c(codigo_mat, n),
      values_from = c(fecha_activ_inicial, fecha_activ_final, cursadas, aprobada_act, oferta, anio_aprobada)
   )

# agregar actividades a df_1
df_2 <- full_join(df_1, act_wide,by=c("id"))

# agregar df_2 a todos_form
df_3 <- full_join(todos_form, df_2,by=c("id", 'numerodoc')) %>% 
   full_join(todos_inscr, by=c("id", 'numerodoc', 'nom_oferta'))


# acomodar
df_4 <- df_3 %>% 
  mutate(edad_final_3462 = interval(as.Date(fechanacimiento_fe), as.Date(fecha_activ_final_3462_1)) / years(1),
         edad_final_3539 = interval(as.Date(fechanacimiento_fe), as.Date(fecha_activ_final_3539_1)) / years(1),
         edad_final_3378 = interval(as.Date(fechanacimiento_fe), as.Date(fecha_activ_final_3378_1)) / years(1),
         edad_final_2573 = interval(as.Date(fechanacimiento_fe), as.Date(fecha_activ_final_2573_1)) / years(1),
         edad_final_3505 = interval(as.Date(fechanacimiento_fe), as.Date(fecha_activ_final_3505_1)) / years(1),
         edad_final_3543 = interval(as.Date(fechanacimiento_fe), as.Date(fecha_activ_final_3543_1)) / years(1),
         act_post_abandono = ifelse(abandono ==0, NA, act_post_abandono),
         edad_ingreso = interval(as.Date(fechanacimiento_fe), as.Date('2019-03-01')) / years(1)) %>% 
   mutate(num_oferta = 907,
          gen = 2019,
          nom_oferta='CONTADOR PUBLICO') %>% 
   select(id, gen, numerodoc, tipodocumento_fe, pais_doc_fe, sexo_fe,
          fechanacimiento_fe, cod_deptonacimiento_fe, cod_paisnacimiento_fe,
          sd_1_1_fe, sd_1_2_fe, sd_3_1_fe, sd_3_2_fe, sd_3_3_fe, sd_4_fe,
          ascendencia_principal_fe, sd_8_fe, sd_9_fe, sd_10_fe, sd_11_1_fe,
          sd_11_2_fe, sd_11_3_fe, sd_11_4_fe, sd_11_7_fe, educ_padre_fe,
          educ_madre_fe, ep_14_4_1_fe, ep_14_4_2_fe, ep_14_4_3_fe, t_15_fe, 
          t_16_fe,fecha_insc_plan,edad_ingreso, num_oferta, nom_oferta, in_administracion, in_economia,
          eventos_tot, eventos_2019_1, eventos_2019_2, eventos_2020_1, 
          eventos_2020_2, eventos_2021_1, eventos_2021_2, eventos_2022_1,
          eventos_2022_2, eventos_2023_1, eventos_2023_2, 
          promedio_aprobaciones, promedio_general,creditos_obtenidos, oferta_2573_1,
          cursadas_2573_1, aprobada_act_2573_1, anio_aprobada_2573_1,
          fecha_activ_inicial_2573_1, fecha_activ_final_2573_1,
          edad_final_2573, oferta_3462_1,
          cursadas_3462_1, aprobada_act_3462_1, anio_aprobada_3462_1,
          fecha_activ_inicial_3462_1, fecha_activ_final_3462_1,
          edad_final_3462, oferta_3539_1,
          cursadas_3539_1, aprobada_act_3539_1, anio_aprobada_3539_1,
          fecha_activ_inicial_3539_1, fecha_activ_final_3539_1,
          edad_final_3539,oferta_3378_1,
          cursadas_3378_1, aprobada_act_3378_1, anio_aprobada_3378_1,
          fecha_activ_inicial_3378_1, fecha_activ_final_3378_1,
          edad_final_3378,oferta_3505_1,
          cursadas_3505_1, aprobada_act_3505_1, anio_aprobada_3505_1,
          fecha_activ_inicial_3505_1, fecha_activ_final_3505_1,
          edad_final_3505,oferta_3543_1,
          cursadas_3543_1, aprobada_act_3543_1, anio_aprobada_3543_1,
          fecha_activ_inicial_3543_1, fecha_activ_final_3543_1,
          edad_final_3543, abandono, act_post_abandono)

write_dta(df_4, 'datos_limpios/Act_2024_limpio.dta')



