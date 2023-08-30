#Librerías y bases----------
rm(list = ls())

pacman::p_load(tidyverse, googledrive)

drive_deauth()

url <-  drive_get(as_id("1yTWNFdwBJhh8F4sX8Z7ESY7SNxXuUnXM"))
drive_download(url, overwrite = TRUE)
uruguay <- read.csv("ECH_2022.csv")

unlink("ECH_2022.csv")

uruguay <- uruguay %>%
  filter(POBPCOAC == 2)

#Estandarización variables----------
uruguay <- uruguay %>%
  mutate(pais = "Uruguay",
         categoria = car::recode(f73, "1:3=3; 4=1; 9=2; 7:8=3"),
         tamano = case_when(f77 <= 2 ~ 1,
                            f77 > 2 ~ 2),
         rural = ifelse(f72_2 < 300, 1, 0),
         pond = w_ano,
         pond2 = w_ano,
         id = paste0(as.character(ID), as.character(nper)),
         sexo = e26,
         edad = e27)

#Construcción CIUO 08 --------------
uruguay$f71_2 <- str_pad(uruguay$f71_2, 4, pad = "0")

uruguay <- uruguay %>%
  mutate(ciuo2 = as.numeric(str_sub(f71_2, 1, 2)),
         ciuo4 = as.numeric(f71_2))

#Guardado de base---------
uruguay <- uruguay %>%
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

saveRDS(uruguay, "bases/Bases_AL/Uruguay/uruguay.RDS")
