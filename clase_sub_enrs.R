#Librerías---------------
pacman::p_load(tidyverse, haven)


#Bases------------
enrs <- read_sav("bases/Base de datos ENRS PISAC.sav")


#Variables-----

enrs <- enrs %>%
  mutate(
    across(c(P10k1, P10k2, P10k3, P10k4, P10k5),
           ~ case_when(
             . == 1 ~ 0,
             . == 2 ~ -1,
             . == 3 ~ 1,
             TRUE ~ NA_real_
           )),
    n_validos = rowSums(!is.na(across(c(P10k1, P10k2)))),
    indice_pos_rel = if_else(
      n_validos > 0,
      rowSums(across(c(P10k1, P10k2)), na.rm = TRUE) / n_validos,
      NA_real_
    ),
    ocupacion = haven::as_factor(P50e, levels = "labels"),
    ingresos = haven::as_factor(P51, levels = "labels")
  )


#Pruebas---------

enrs %>%
  group_by(ocupacion) %>%
  summarise(promedio = round(weighted.mean(indice_pos_rel, w = pond_final, na.rm = T), digits = 2))

enrs %>%
  group_by(ingresos) %>%
  summarise(promedio = round(weighted.mean(indice_pos_rel, w = pond_final, na.rm = T), digits = 2))

enrs %>%
  count(ocupacion, P10k1, wt = pond_final) %>%
  janitor::adorn_percentages("row")

