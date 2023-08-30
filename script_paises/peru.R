#Librerías y bases----------
rm(list = ls())

pacman::p_load(tidyverse, occupationcross, googledrive)

drive_deauth()

url <-  drive_get(as_id("1s6Ox9SGnG6lSz_zoDvCC6inerD8XBvTw"))
drive_download(url, overwrite = TRUE)
peru_empleo <- haven::read_dta("enaho01a-2018-500.dta")

url <-  drive_get(as_id("11_L1nJH4ltHnqBwEYyxHh2hYLX14SbTD"))
drive_download(url, overwrite = TRUE)
peru_pob <- haven::read_dta("enaho01-2018-200.dta")

unlink(c("enaho01a-2018-500.dta", "enaho01-2018-200.dta"))

peru <- peru_empleo %>%
  left_join(peru_pob, by = c("conglome", "vivienda", "hogar", "codperso"))

peru <- peru %>%
  filter(ocu500 == 1)

rm(peru_empleo, peru_pob)

#Estandarización variables----------
peru <- peru %>%
  mutate(pais = "Perú",
         anio = 2018,
         categoria = as.numeric(car::recode(p507, "1=1; 2=2; 3:7=3")),
         tamano = case_when(p512b <= 5 ~ 1,
                            p512b > 5 ~ 2,
                            TRUE ~ NA_real_),
         rural = ifelse(p506r4 < 500, 1, 0),
         pond = fac500,
         id = paste0(as.character(conglome), as.character(vivienda), as.character(hogar),
                     as.character(codperso)),
         sexo = as.numeric(p207.x),
         edad = as.numeric(p208a.x))

#Construcción CIUO 08 --------------
peru$p505r4 <- str_pad(peru$p505r4, 4, pad = "0")

peru <- peru %>%
  mutate(ciuo4 = as.numeric(p505r4),
         ciuo4 = case_when(ciuo4 == 111 ~ 110,
                           ciuo4 == 112 ~ 110,
                           ciuo4 == 120 ~ 210,
                           ciuo4 == 121 ~ 210,
                           ciuo4 == 131 ~ 310,
                           ciuo4 %in% c(212, 213, 220, 311, 312,
                                        313, 320) ~ 5412,
                           TRUE ~ ciuo4),
         ciuo2 = as.numeric(str_sub(ciuo4, 1, 2)))

#Guardado de base---------
peru <- peru %>%
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

saveRDS(peru, "bases/Bases_AL/Peru/peru.RDS")
