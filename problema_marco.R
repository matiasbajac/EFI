# Analisis de marco muestral y actividad
marco <- marco_muestral <- read_dta("bases EFI 2025/marco muestral 2024/marco muestral.dta")
todos_act <- read_dta("bases EFI 2025/TODOS Cdor actividades_EFI2024.dta")
todos_form <- read_dta("bases EFI 2025/TODOS Cdor form_ingreso_ EFI2024.dta")

ci_marco <- marco$numerodoc
ci_act <- todos_act$numerodoc %>% unique()

# el marco dice que hay 375 abandono

# cuanta gente esta en el marco definida como abandono pero no tienen actividad
setdiff(ci_marco, ci_act) %>% length() #267

# cuanta gente esta en el marco y en actividad
intersect(ci_marco, ci_act) %>% length() #108

# por definicion de abandono de los registros 2025, tenemos 200

# cuanta gente esta en marco y formulario
ci_form <- todos_form$numerodoc
setdiff(ci_marco, ci_form) %>% length() #20

# vamos a chequear que efectivamente tenemos definido como abandono los que estan ene el marco
abandono_marco_act <- intersect(ci_marco, ci_act)
filter(Act_2024_limpio, numerodoc%in%abandono_marco_act) %>% select(abandono) %>% table()

# nuestros totales abandono
Act_2024_limpio$abandono %>% table()
