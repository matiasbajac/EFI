###### Funciones

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

#hacer funcion para ver si tiene actividad en otra carrera post abandono

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

# decidir sobre las notas de aprobacion y general, las hago por semestre, agregadas o solo las pongo en materias principales
# decidir si hacer variable para todas las materias o solo primer año
# para abondono agregar ultima materia cursada? Ultima materia aprobada? año de aprobada?

# cuando agregue las personas de abandono temprano, definir que es 0 y que es NA.



a <- data %>%
   group_by(id) %>%
   summarise({
      df <- cur_data()
      tibble(
         eventos_tot = fun_eventos(cur_data(), -Inf, Inf),
         eventos_2019_1 = fun_eventos(df, 
                                      as.Date('2019-06-01'), 
                                      as.Date('2019-10-31')),
         eventos_2019_2 = fun_eventos(df, 
                                      as.Date('2019-11-01'), 
                                      as.Date('2020-05-31')),
         eventos_2020_1 = fun_eventos(df, 
                                      as.Date('2020-06-01'), 
                                      as.Date('2020-10-31')),
         eventos_2020_2 = fun_eventos(cur_data(), 
                                      as.Date('2020-11-01'), 
                                      as.Date('2021-05-31')),
         eventos_2021_1 = fun_eventos(df, 
                                      as.Date('2021-06-01'), 
                                      as.Date('2021-10-31')),
         eventos_2021_2 = fun_eventos(df, 
                                      as.Date('2021-06-01'), 
                                      as.Date('2022-10-31')),
         eventos_2022_1 = fun_eventos(df, 
                                      as.Date('2022-06-01'), 
                                      as.Date('2022-10-31')),
         eventos_2022_2 = fun_eventos(df, 
                                      as.Date('2022-11-01'), 
                                      as.Date('2023-05-31')),
         eventos_2023_1 = fun_eventos(df, 
                                      as.Date('2023-06-01'), 
                                      as.Date('2023-10-31')),
         eventos_2023_2 = fun_eventos(df, 
                                      as.Date('2023-11-01'), 
                                      as.Date('2024-05-31')),
         abandono = fun_abandono_2024(df)
      )
   })

