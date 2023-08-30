#Librerías y bases----------
rm(list = ls())

pacman::p_load(tidyverse)

load("bases/base_2021.RData")

argentina <- base_pirc
argentina$M3.9 <- haven::as_factor(argentina$M3.9)

argentina <- subset(argentina, M2.12 == 1)


#Estandarización de variables-----------------
argentina <- argentina %>%
  mutate(pais = "Argentina",
         anio = 2021,
         ciuo4 = as.numeric(CIUO_encuestado),
         sv = car::recode(M3.9, "1:2=1; 3=0; 99=NA"),
         categoria = case_when(M3.5 == 1 ~ 1,
                               M3.5 == 2 & sv == 1 ~ 1,
                               M3.5 == 2 & (sv != 1 | is.na(sv)) ~ 2,
                               M3.5 >= 3 & M3.5 < 99 ~ 3,
                               TRUE ~ NA_real_),
         sv = as.numeric(car::recode(M3.9, "1:2=1; 3=0; 99=NA")),
         tamano = case_when(M3.6 <= 2 ~ 1,
                            M3.6 > 2 & M3.6 < 99 ~ 2,
                            TRUE ~ NA_real_),
         rural = case_when(CAES_letra == "A" ~ 1,
                           TRUE ~ 0),
         sexo = as.numeric(SEXO),
         edad = as.numeric(M2.4),
         pond = POND2R_FIN_n,
         id = CUEST)


#Construcción CIUO 08 --------------

argentina <- argentina %>%
  mutate(ciuo2 = as.numeric(str_sub(ciuo4, 1, 2)),
         domest = ifelse(ciuo4 == 9111, 1, 0))


#Guardado de base---------
argentina <- argentina %>%
  select(id,
         pais,
         anio,
         ciuo2,
         ciuo4,
         categoria,
         tamano,
         rural,
         sexo,
         edad,
         pond)

saveRDS(argentina, "bases/Bases_AL/Argentina/argentina.RDS")

