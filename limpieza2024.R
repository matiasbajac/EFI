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

fun_eventos(data,"2022-01-01","2023-01-01")

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



actividades = data  %>%  filter(nom_oferta == "CONTADOR PUBLICO" & codigo_mat %in% c(3505, 3462, 3539, 2573, 3378,3543)) %>%    mutate(carrera = 1) %>% 
  pivot_wider(names_from = "nom_oferta", values_from = "carrera", values_fill = 0) %>% 
  group_by(id, codigo_mat, fecha_activ) %>% mutate(contador = max(`CONTADOR PUBLICO`)) %>%   slice(1) %>% 
  group_by(id, codigo_mat) %>% 
  arrange(fecha_activ, .by_group = T) %>%
  summarise(
    fecha_activ_inicial = first(fecha_activ),
    fecha_activ_final = last(fecha_activ),
    cursadas = n(),
    aprobada_act = last(aprobada),
    contador = max(contador)) %>% ungroup() %>%  mutate(anio_aprobada = if_else(aprobada_act==1, fecha_activ_final,"no"))

## me creo una variable fecha_activ_final que nos indica la ultima vez que un estudiante interactuo con cada materia 
## mientras que cursadas dice las veces que la curso
## aprobada_act si la aprobo o no 

## funcion de lo anterior

## usando la base de inscriptos e 2019 la junto con la de actividades y  calculo la edad en relacion a la ultima actividad que rindieron y la fecha de nacimiento 

todos_ = todos_ %>%  select(id, fechanacimiento_fe,sexo_fe)  

dos=actividades %>%  left_join(todos_,by=c("id")) %>% 
  mutate(edad_final = interval(as.Date(fechanacimiento_fe), as.Date(fecha_activ_final)) / years(1))




