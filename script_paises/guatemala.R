#Librerías y bases----------
rm(list = ls())

pacman::p_load(tidyverse)

guatemala <- haven::read_sav("bases/Bases_AL/Guatemala/base_guatemala.sav")

guatemala <- subset(guatemala, OCUPADOS == 1)

#Estandarización variables----------
guatemala <- guatemala %>%
  mutate(pais = "Guatemala",
         anio = 2019,
         categoria = as.numeric(car::recode(P04C06, "1:4=3; 5=2; 7=2; 6=1; 8=1; 9=3")),
         tamano = case_when(P04C05 <= 5 ~ 1,
                            P04C05 > 5 ~ 2),
         rural = ifelse(P04C04B_1D == 1, 1, 0),
         pond = FACTOR,
         id = paste0(as.character(NUM_HOGAR), as.character(ID)),
         sexo = as.numeric(PPA02),
         edad = as.numeric(PPA03))

#Construcción CIUO 08 --------------

guatemala$ciuo2 <- as.numeric(guatemala$P04C02B_2D)



#Guardado de base---------
guatemala <- guatemala %>%
  select(id,
         pais,
         anio,
         ciuo2,
         categoria,
         tamano,
         rural,
         sexo,
         edad,
         pond)

saveRDS(guatemala, "bases/Bases_AL/Guatemala/guatemala.RDS")
