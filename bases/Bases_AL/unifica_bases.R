# Librerías ----------------------
rm(list = ls())

pacman::p_load(tidyverse)

# Unión de bases --------------

argentina <- readRDS("bases/Bases_AL/Argentina/argentina_eph.RDS")
brasil <- readRDS("bases/Bases_AL/Brasil/brasil.RDS")
chile <- readRDS("bases/Bases_AL/Chile/chile.RDS")
ecuador <- readRDS("bases/Bases_AL/Ecuador/ecuador.RDS")
mexico <- readRDS("bases/Bases_AL/Mexico/mexico.RDS")
peru <- readRDS("bases/Bases_AL/Peru/peru.RDS")
uruguay <- readRDS("bases/Bases_AL/Uruguay/uruguay.RDS")

unlink(c("argentina_eph.RDS", "brasil.RDS", "chile.RDS", "ecuador.RDS", "mexico.RDS",
         "peru.RDS", "uruguay.RDS"))

base_AL <- brasil %>%
  bind_rows(argentina) %>%
  bind_rows(chile) %>%
  bind_rows(ecuador) %>%
  bind_rows(mexico) %>%
  bind_rows(peru) %>%
  bind_rows(uruguay)

rm(brasil, chile, ecuador, mexico, peru, uruguay)


#EGP 5 clases directa-------------
#Con CIUO
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


#Agrego Argentina ----------------
#Con CNO
argentina <- argentina %>%
  mutate(egp_cno = case_when( #Directivos y patrones

    cno12 >= 0 & cno12 <= 2 ~ 1,
    (cno12 >= 5 & cno12 <= 7) & cno5 == 1 ~ 1,
    (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 2 ~ 1,
    (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 1 & (rural == 0 | rural == "" | is.na(rural)) ~ 5,
    (cno12 >= 5 & cno12 <= 7) & cno5 != 1 & tamano == 1 & rural == 1 ~ 7,
    cno12 == 5 & cno5 != 1 & is.na(tamano) & (rural == 0 | rural == "" | is.na(rural)) ~ 5,
    cno12 == 5 & cno5 != 1 & is.na(tamano) & rural == 1 ~ 7,
    (cno12 == 6 | cno12 == 7) & cno5 != 1 & is.na(tamano) ~ 1,
    cno12 == 3 ~ 1,
    cno12 == 4 ~ 2,
    cno12 > 7 & categoria == 1 & tamano == 2 ~ 1,
    cno12 > 7 & categoria == 1 & tamano == 1 & cno5 == 1 ~ 1,
    cno12 > 7 & categoria == 1 & tamano == 1 & cno5 != 1 & (rural == 0 | rural == "" | is.na(rural)) ~ 5,
    cno12 > 7 & categoria == 1 & tamano == 1 & cno5 != 1 & rural == 1 ~ 7,


    #Cuenta propia
    categoria == 2 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 44) | (cno12 >= 60 & cno12 <= 81)
                                  | cno12 >= 90) ~ 1,
    categoria == 2 & cno5 == 1 & (cno12 == 10 | cno12 == 30 | cno12 == 32 | cno12 == 36 |
                                    (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54)
                                  | cno12 == 58 | cno12 == 82) ~ 2,
    categoria == 2 & cno5 == 2 & (cno12 == 10 | cno12 == 32 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 43) | cno12 == 45 | cno12 == 51
                                  | (cno12 >= 60 & cno12 <= 71) | cno12 == 81 | cno12 == 91
                                  | cno12 == 99) ~ 2,
    categoria == 2 & cno5 == 2 & ((cno12 >= 11 & cno12 <= 30) | cno12 == 36 | cno12 == 44 |
                                    cno12 == 46 | cno12 == 47 | cno12 == 50 |
                                    (cno12 >= 52 & cno12 <= 54) | cno12 == 58 | cno12 == 72 |
                                    cno12 == 80 | cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 6,
    categoria == 2 & (cno5 == 3 | cno5 == 4) & ((cno12 >= 10 & cno12 <= 30) | cno12 == 32 |
                                                  (cno12 >= 34 & cno12 <= 47) |
                                                  (cno12 >= 50 & cno12 <= 54) | cno12 == 58
                                                | cno12 == 64 | cno12 >= 70) ~ 6,
    categoria == 2 & (cno12 == 31 | cno12 == 33 | cno12 == 48 | cno12 == 49 |
                        (cno12 >= 55 & cno12 <= 57)) ~ 6,
    categoria == 2 & (cno5 == 3 | cno5 == 4) & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 7,


    #Asalariados jefes
    categoria == 3 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 44) | cno12 == 48 | cno12 == 49 |
                                    (cno12 >= 60 & cno12 <= 81) | cno12 >= 90) ~ 1,
    categoria == 3 & cno5 == 1 & (cno12 == 10 | (cno12 >= 30 & cno12 <= 32) | cno12 == 36 |
                                    (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54) |
                                    cno12 == 58 | cno12 == 82) ~ 2,
    categoria == 3 & cno5 == 2 & ((cno12 >= 10 & cno12 <= 32) | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 49) | (cno12 >= 50 & cno12 <= 53) |
                                    (cno12 >= 60 & cno12 <= 71) | cno12 == 81 |
                                    cno12 == 91 | cno12 == 99) ~ 2,
    categoria == 3 & cno5 == 2 & (cno12 == 33 | cno12 == 36 |
                                    (cno12 >= 56 & cno12 <= 58) | cno12 == 72 | cno12 == 80 |
                                    cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 8,
    categoria == 3 & cno5 == 2 & (cno12 == 54) ~ 3,
    categoria == 3 & cno5 == 3 & ((cno12 >= 10 & cno12 <= 20) | cno12 == 30 | cno12 == 31 |
                                    (cno12 >= 41 & cno12 <= 43) | cno12 == 46 | cno12 == 47 | cno12 == 48 |
                                    cno12 == 81 | cno12 == 91) ~ 2,
    categoria == 3 & cno5 == 3 & (cno12 == 32 | cno12 == 54) ~ 3,
    categoria == 3 & cno5 == 3 & ((cno12 >= 33 & cno12 <=36) | cno12 == 40 | cno12 == 44 | cno12 == 45 |
                                    (cno12 >= 49 & cno12 <= 53) | (cno12 >= 55 & cno12 <= 58) |
                                    cno12 == 64 | (cno12 >= 70 & cno12 <= 80) | cno12 == 82 |
                                    cno12 == 90 | cno12 >= 92) ~ 8,
    categoria == 3 & cno5 == 3 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 64) ~ 11,


    #Asalariados directos
    categoria == 4 & cno5 == 1 & (cno12 == 11 | cno12 == 20 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 44) | cno12 == 48 | cno12 == 49 |
                                    (cno12 >= 60 & cno12 <= 81) | cno12 >= 90) ~ 1,
    categoria == 4 & cno5 == 1 & (cno12 == 10 | cno12 == 30 | cno12 == 32 | cno12 == 36 |
                                    (cno12 >= 45 & cno12 <= 47) | (cno12 >= 50 & cno12 <= 54) |
                                    cno12 == 58 | cno12 == 82) ~ 2,
    categoria == 4 & cno5 == 1 & cno12 == 31 ~ 3,
    categoria == 4 & cno5 == 2 & (cno12 == 10 | cno12 == 32 | cno12 == 34 | cno12 == 35 |
                                    (cno12 >= 40 & cno12 <= 45) | cno12 == 50 | cno12 == 52 |
                                    (cno12 >= 60 & cno12 <= 71) |
                                    cno12 == 81 | cno12 == 91 | cno12 == 99) ~ 2,
    categoria == 4 & cno5 == 2 & ((cno12 >= 11 & cno12 <= 31) | cno12 == 46 | cno12 == 54) ~ 3,
    categoria == 4 & cno5 == 2 & (cno12 == 36 | (cno12 >= 47 & cno12 <= 49) | cno12 == 51 |
                                    cno12 == 53 | cno12 == 56 | cno12 == 58 | cno12 == 72 |
                                    cno12 == 80 | cno12 == 82 | cno12 == 90 | cno12 == 92) ~ 8,
    categoria == 4 & cno5 == 2 & (cno12 == 57) ~ 9,
    categoria == 4 & cno5 == 3 & ((cno12 >= 10 & cno12 <= 20) | cno12 == 31 |
                                    (cno12 >= 41 & cno12 <= 43) | cno12 == 46 | cno12 == 54 |
                                    cno12 == 81 | cno12 == 91) ~ 3,
    categoria == 4 & cno5 == 3 & (cno12 == 30 | cno12 == 32) ~ 4,
    categoria == 4 & cno5 == 3 & (cno12 >= 48 & cno12 <= 50) ~ 8,
    categoria == 4 & cno5 == 3 & (cno12 == 34 | cno12 == 36 | cno12 == 40 | cno12 == 44 | cno12 == 45 | cno12 == 47
                                  | cno12 == 52 | cno12 == 53 | cno12 == 55 |
                                    cno12 == 57 | (cno12 >= 71 & cno12 <= 80) | cno12 == 82 |
                                    cno12 == 90 | cno12 >= 92) ~ 9,
    categoria == 4 & cno5 == 3 & (cno12 == 51 | cno12 == 56 |
                                    cno12 == 58 | cno12 == 64 | cno12 == 70) ~ 10,
    categoria == 4 & cno5 == 3 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 11,
    categoria == 4 & cno5 == 3 & cno12 == 35 & cno4 == 1 ~ 10,
    categoria == 4 & cno5 == 3 & cno12 == 35 & cno4 != 1 ~ 9,
    categoria == 4 & cno5 == 3 & cno12 == 52 & cno4 == 1 ~ 3,
    categoria == 4 & cno5 == 3 & cno12 == 52 & cno4 != 1 ~ 9,
    categoria == 4 & cno5 == 4 & (cno12 == 11 | cno12 == 20 | cno12 == 31 | cno12 == 41
                                  | cno12 == 42 | cno12 == 46) ~ 3,
    categoria == 4 & cno5 == 4 & (cno12 == 30) ~ 4,
    categoria == 4 & cno5 == 4 & (cno12 == 10 | cno12 == 32 | (cno12 >= 34 & cno12 <= 40) | cno12 == 44 |
                                    cno12 == 45 | (cno12 >= 47 & cno12 <= 58) |
                                    cno12 == 64 | cno12 >= 70) ~ 10,
    categoria == 4 & cno5 == 4 & ((cno12 >= 60 & cno12 <= 63) | cno12 == 65) ~ 11,
    categoria == 4 & cno12 == 33 ~ 10,

    TRUE ~ NA_real_
  ))


#Variación Solís y Boado (2016).
argentina <- argentina %>%
  mutate(egp_cno = case_when(categoria == 2 & cno5 == 4 & rural == 0 ~ 10,
                             categoria == 2 & cno5 == 4 & rural == 1 ~ 11,
                             categoria == 2 & cno12 == 33 ~ 10,
                             categoria == 2 & cno5 == 3 & cno12 == 10 ~ 10,
                             categoria == 2 & cno5 == 3 & cno12 == 32 ~ 10,
                             categoria == 2 & cno5 == 3 & cno12 == 34 & cno4 == 1 ~ 10,
                             categoria == 2 & cno5 == 3 & cno12 == 34 & cno4 != 1 ~ 6,
                             categoria == 2 & cno5 == 3 & cno12 == 35 & cno4 == 1 ~ 10,
                             categoria == 2 & cno5 == 3 & cno12 == 35 & cno4 != 1 ~ 6,
                             TRUE ~ egp_cno),
         egp_cno = car::recode(egp_cno, "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=5; 8=7; 9=8; 10=9; 11=9"))

argentina <- mutate(argentina, egp = car::recode(argentina$egp_cno, "1:2=1; 3:4=2; 5:6=3; 7:8=4; 9=5"))


#pruebas
# perdidos <- base_AL %>%
#   filter(is.na(egp)) %>%
#   arrange(ciuo4, categoria, tamano)

argentina$egp_cno <- NULL


# Pego Argentina a base_AL
base_AL <- base_AL %>%
  bind_rows(argentina)

base_AL$egp_f <- factor(base_AL$egp, labels = c("I+II", "III", "IV", "V+VI", "VII"))

rm(argentina)

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



#Guardo base
saveRDS(base_AL, "bases/Bases_AL/base_al.RDS")
