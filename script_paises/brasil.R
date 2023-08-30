#Librerías y bases----------
rm(list = ls())

pacman::p_load(tidyverse, PNADcIBGE)

brasil <- get_pnadc(year=2018, quarter=2, design=F, labels = F)

brasil <- brasil %>%
  filter(VD4002 == 1)

#Estandarización variables----------
brasil <- brasil %>%
  mutate(pais = "Brasil",
         anio = as.numeric(Ano),
         categoria = car::recode(V4012, "1:4=3; 5=1; 6=2; 7=3"),
         tamano = case_when(V4016 == 1 ~ 1,
                            V4016 > 1 ~ 2,
                            V4018 == 1 ~ 1,
                            V4018 > 1 ~ 2,
                            TRUE ~ NA),
         rural = ifelse(V40132A == 1, 1, 0),
         pond = V1028,
         id = as.character(0),
         sexo = as.numeric(V2007),
         edad = as.numeric(V2009))

#Construcción CIUO 08 --------------
brasil <- brasil %>%
  mutate(ciuo4 = as.numeric(V4010),
         ciuo4 = case_when(ciuo4 == 411 ~ 110,
                           ciuo4 == 412 ~ 210,
                           ciuo4 == 511 ~ 110,
                           ciuo4 == 512 ~ 210,
                           TRUE ~ ciuo4),
         ciuo2 = 0)

#Guardado de base---------
brasil <- brasil %>%
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

saveRDS(brasil, "bases/Bases_AL/Brasil/brasil.RDS")
