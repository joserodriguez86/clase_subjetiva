#Librerías y bases----------
rm(list = ls())

pacman::p_load(tidyverse, googledrive)

drive_deauth()

url <-  drive_get(as_id("1h57MmqxrqatgQSLZl7U1awWZCfSfX52j"))
drive_download(url, overwrite = TRUE)
ecuador <- read.csv2("201803_EnemduBDD_15anios.csv")

unlink("201803_EnemduBDD_15anios.csv")

ecuador <- ecuador %>%
  filter(condactn %in% c(1:6))

#Estandarización variables----------
ecuador <- ecuador %>%
  mutate(pais = "Ecuador",
         categoria = car::recode(p42, "1:4=3; 5=1; 6=2; 7:10=3"),
         tamano = case_when(p47b <= 5 ~ 1,
                            p47b > 5 | p47a == 2 ~ 2),
         rural = ifelse(p40 < 300, 1, 0),
         pond = fexp,
         id = paste0(as.character(vivienda), as.character(hogar), as.character(p01)),
         sexo = p02,
         anio = 2022,
         edad = p03)

#Construcción CIUO 08 --------------
ecuador$p41 <- str_pad(ecuador$p41, 4, pad = "0")

ecuador <- ecuador %>%
  mutate(ciuo2 = as.numeric(str_sub(p41, 1, 2)),
         ciuo4 = as.numeric(p41))

#Guardado de base---------
ecuador <- ecuador %>%
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

saveRDS(ecuador, "bases/Bases_AL/Ecuador/ecuador.RDS")


