# Librerías ----------------------
rm(list = ls())

pacman::p_load(tidyverse)

# Unión de bases --------------

argentina <- readRDS("bases/Bases_AL/Argentina/argentina.RDS")
brasil <- readRDS("bases/Bases_AL/Brasil/brasil.RDS")
chile <- readRDS("bases/Bases_AL/Chile/chile.RDS")
ecuador <- readRDS("bases/Bases_AL/Ecuador/ecuador.RDS")
mexico <- readRDS("bases/Bases_AL/Mexico/mexico.RDS")
peru <- readRDS("bases/Bases_AL/Peru/peru.RDS")
uruguay <- readRDS("bases/Bases_AL/Uruguay/uruguay.RDS")

unlink(c("argentina.RDS", "brasil.RDS", "chile.RDS", "ecuador.RDS", "mexico.RDS",
         "peru.RDS", "uruguay.RDS"))

base_AL <- argentina %>%
  add_row(brasil) %>%
  add_row(chile) %>%
  add_row(ecuador) %>%
  add_row(mexico) %>%
  add_row(peru) %>%
  add_row(uruguay)

rm(argentina, brasil, chile, ecuador, mexico, peru, uruguay)

#Variable país - año------------
base_AL <- base_AL %>%
  mutate(pais_corto = case_when(pais == "Argentina" ~ "ARG",
                                pais == "Brasil" ~ "BRA",
                                pais == "Chile" ~ "CHL",
                                pais == "Ecuador" ~ "ECU",
                                pais == "México" ~ "MEX",
                                pais == "Perú" ~ "PER",
                                pais == "Uruguay" ~ "URY"),
         pais_anio = paste(pais_corto, as.character(anio), sep = "-"),
         sexo_f = case_when(sexo == 1 ~ "Varón",
                            sexo == 2 ~ "Mujer"))


#EGP 5 clases directa-------------
base_AL <- base_AL %>%
  mutate(egp = case_when(
    #Patrones
    categoria == 1 & tamano == 2 ~ 1,
    categoria == 1 & tamano == 1 ~ 3,

    #Cuenta propia
    categoria == 2 & (ciuo4 >= 1000 & ciuo4 < 3000) ~ 1,

    categoria == 2 & ciuo4 %in% c(3221, 3222, 3311:3314, 3411:3413) ~ 3,
    categoria == 2 & (ciuo4 >= 3000 & ciuo4 < 4000) ~ 1,

    categoria == 2 & (ciuo4 %in% c(5211, 5212, 5413, 5414) |
                        (ciuo4 >= 9000 & ciuo4 < 9999) | (ciuo4 >= 6300 & ciuo4 < 7000)) ~ 5,
    categoria == 2 & ((ciuo4 >= 4000 & ciuo4 < 6000) | (ciuo4 >= 7000 & ciuo4 < 9000)) ~ 3,
    categoria == 2 & (ciuo4 >= 6000 & ciuo4 < 6300) ~ 3,

    #Empleados
    categoria == 3 & (ciuo4 >= 1000 & ciuo4 < 3000) ~ 1,

    categoria == 3 & ciuo4 %in% c(3221, 3222, 3311:3314, 3411:3413) ~ 2,
    categoria == 3 & (ciuo4 >= 3000 & ciuo4 < 4000) ~ 1,

    categoria == 3 & (ciuo4 >= 4000 & ciuo4 < 5000) ~ 2,

    categoria == 3 & ciuo4 %in% c(5111:5113) ~ 2,
    categoria == 3 & ciuo4 %in% c(5120, 5141, 5142, 5153, 5163, 5164, 5165, 5169) ~ 4,
    categoria == 3 & ciuo4 %in% c(5211, 5212, 5413, 5414, 5419) ~ 5,
    categoria == 3 & ciuo4 %in% c(5411, 5412) ~ 4,
    categoria == 3 & (ciuo4 >= 5000 & ciuo4 < 6000) ~ 2,

    categoria == 3 & (ciuo4 >= 6000 & ciuo4 < 6300) ~ 4,

    categoria == 3 & (ciuo4 >= 6300 & ciuo4 < 7000) ~ 5,

    categoria == 3 & (ciuo4 >= 7000 & ciuo4 < 9000) ~ 4,
    categoria == 3 & (ciuo4 >= 9000 & ciuo4 < 9999) ~ 5,

    ciuo4 == 110 ~ 1,
    ciuo4 == 210 | ciuo4 == 310 ~ 4,
    TRUE ~ NA_real_
  ))


#pruebas
# perdidos <- base_AL %>%
#   filter(is.na(egp)) %>%
#   arrange(ciuo4, categoria, tamano)

base_AL$egp_f <- factor(base_AL$egp, labels = c("I+II", "III", "IV", "V+VI", "VII"))

saveRDS(base_AL, "bases/Bases_AL/base_al.RDS")
