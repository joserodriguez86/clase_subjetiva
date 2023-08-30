#Librerías y bases----------
rm(list = ls())

pacman::p_load(tidyverse, occupationcross, googledrive)

drive_deauth()

url <-  drive_get(as_id("1_wcmTSXXSoKBSI-zxnSmp6qLs6I2zmYY"))
drive_download(url, overwrite = TRUE)
chile <- read.csv2("casen_2017.csv")

unlink("casen_2017.csv")

chile <- chile %>%
  filter(activ == 1)

#Estandarización variables----------

chile <- chile %>%
  mutate(pais = "Chile",
         categoria = car::recode(o15, "3:9=3"),
         tamano = case_when(o23 %in% c("A", "B") ~ 1,
                            o23 %in% c("C", "D", "E", "F") ~ 2,
                            o23 == "X" & o15 %in% c(3, 4, 8) ~ 2,
                            TRUE ~ NA_real_),
         rural = ifelse(rama1 == 1, 1, 0),
         pond = expr,
         id = paste0(as.character(folio), as.character(o)),
         anio = 2018)

#Construcción CIUO 08 --------------
chile$oficio4 <- str_pad(chile$oficio4, 4, pad = "0")

chile <- isco88_to_isco08_ndigit(chile, oficio4, digits = 4)

chile <- chile %>%
  mutate(ciuo2 = as.numeric(str_sub(ISCO.08, 1, 2)),
         ciuo4 = as.numeric(ISCO.08),
         domest = ifelse(ciuo4 == 9111, 1, 0))


#Guardado de base---------
chile <- chile %>%
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

saveRDS(chile, "bases/Bases_AL/Chile/chile.RDS")
