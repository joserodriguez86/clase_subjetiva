#Librerías y bases----------
rm(list = ls())

pacman::p_load(tidyverse, occupationcross, googledrive)

drive_deauth()

url <-  drive_get(as_id("1T3THTbnmNC-htuyA-eYkFYpW7Q1oKnbI"))
drive_download(url, overwrite = TRUE)
mexico_pob <- read.csv("poblacion.csv")

url <-  drive_get(as_id("1yLjcewZp-2WcQhQFQoz0iN47bmO86axW"))
drive_download(url, overwrite = TRUE)
mexico_trab <- read.csv("trabajos.csv")

url <-  drive_get(as_id("1MToV4DZKlWLdX9bKS2X53Jw6wTJzo42k"))
drive_download(url, overwrite = TRUE)
mexico_con <- read.csv("concentradohogar.csv")

unlink(c("poblacion.csv", "trabajos.csv", "concentradohogar.csv"))

mexico <- mexico_trab %>%
  left_join(mexico_pob, by = c("folioviv", "foliohog", "numren")) %>%
  left_join(mexico_con, by = c("folioviv", "foliohog")) %>%
  filter(trabajo_mp == 1, id_trabajo == 1)

rm(mexico_pob, mexico_trab, mexico_con)


#Estandarización variables----------
mexico <- mexico %>%
  mutate(pais = "México",
         anio = 2018,
         categoria = ifelse(subor == 1, 3,
                            ifelse(indep == 2, 3,
                                   ifelse(personal == 1, 1, 2))),
         tamano = case_when(tam_emp <= 2 ~ 1,
                            tam_emp > 2 ~ 2),
         rural = ifelse(scian > 1000 & scian < 2000, 1, 0),
         pond = factor,
         id = paste0(as.character(folioviv), as.character(foliohog), as.character(numren)),
         sexo = sexo,
         edad = edad,
         sinco = ifelse(sinco == 980, 9899, sinco))

#Construcción CIUO 08 --------------

mexico <- sinco2011_to_isco08(mexico, sinco)

mexico <- mexico %>%
  mutate(ciuo4 = as.numeric(case_when(sinco %in% c("1003", "1313", "1613", "1619", "1629",
                                                   "1999") ~ "1000",
                           sinco == 2164 ~ "3439",
                           sinco %in% c("2333", "2334", "2391") ~ "2359",
                           sinco == 2421 ~ "2149",
                           sinco == 2639 ~ "7230",
                           sinco %in% c("2640", "2649") ~ "3110",
                           sinco == 2711 ~ "2359",
                           sinco == 2991 ~ "2000",
                           sinco == 2992 ~ "3000",
                           sinco == 4312 ~ "5223",
                           sinco %in% c("6116", "6117", "6119") ~ "6110",
                           sinco == 6128 ~ "6120",
                           sinco == 6213 ~ "6220",
                           sinco == 6999 ~ "6000",
                           sinco == 8349 ~ "8300",
                           sinco == 9999 ~ "9999",
                           TRUE ~ ISCO.08)),
         ciuo2 = as.numeric(str_sub(ISCO.08, 1, 2)))


#Guardado de base---------
mexico <- mexico %>%
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

saveRDS(mexico, "bases/Bases_AL/Mexico/mexico.RDS")
