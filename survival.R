## Survival Analysis 

library(survival)
library(survminer)
library(tidyverse)

unas_sola= todos_act %>% distinct(id, .keep_all = TRUE)
act_w_carr=left_join(act_wide,select(unas_sola,nom_oferta,id),by="id")

act_w_carr = act_w_carr %>%
  filter(!nom_oferta %in% c(
    "LICENCIATURA EN ADMINISTRACION",
    "TECNOLOGO EN ADMINISTRACION Y CONTABILIDAD",
    "TECNICO EN ADMINISTRACION"
  ))



datos_surv <- act_w_carr %>%
  mutate(across(starts_with("fecha_"), as.Date)) %>% 
  rowwise() %>%
  mutate(
    fecha_inicio = min(c_across(starts_with("fecha_activ_inicial")), na.rm = TRUE),
    fecha_fin = max(c_across(starts_with("fecha_activ_final")), na.rm = TRUE)
  ) %>%
  ungroup()


datos_surv <- datos_surv %>%
  mutate(
    tiempo_meses = interval(fecha_inicio, fecha_fin) / months(1)
  )
datos_surv$tiempo_meses


datos_surv <- datos_surv %>%
  mutate(
    abandono = ifelse(fecha_fin < as.Date("2023-12-31"), 1, 0)
  )

surv_obj <- Surv(time = datos_surv$tiempo_meses, event = datos_surv$abandono)

km_fit <- survfit(surv_obj ~ 1, data = datos_surv)



ggsurvplot(km_fit,
           conf.int = F,
           xlab = "Meses desde ingreso",
           ylab = "Probabilidad de supervivencia",
           ggtheme = theme_minimal())



km_carr <- survfit(Surv(tiempo_meses, abandono) ~nom_oferta, data = datos_surv)

ggsurvplot(
  km_carr, data = datos_surv,
  conf.int = TRUE,
  xlab = "Meses desde ingreso", ylab = "Probabilidad de supervivencia",
  ggtheme = theme_minimal()
)
