#Librerías y bases----------
rm(list = ls())

pacman::p_load(tidyverse, eph)

argentina <- get_total_urbano(year=2017, type="individual", vars="all", destfile=NULL)

argentina <- subset(argentina, ESTADO == 1)


#Estandarización de variables-----------------
#Agregar digitos 0 al CNO
argentina$PP04D_COD[argentina$PP04D_COD==""] <- NA

argentina$cno <- argentina$PP04D_COD

#Separo dígitos del CNO
argentina$cno12 <- ifelse(nchar(argentina$cno) > 4, str_sub(argentina$cno, 1, 2), str_sub(argentina$cno, 1, 1))
argentina$cno3 <- ifelse(nchar(argentina$cno) > 4, str_sub(argentina$cno, 3, 3), str_sub(argentina$cno, 2, 2))
argentina$cno4 <- ifelse(nchar(argentina$cno) > 4, str_sub(argentina$cno, 4, 4), str_sub(argentina$cno, 3, 3))
argentina$cno5 <- ifelse(nchar(argentina$cno) > 4, str_sub(argentina$cno, 5, 5), str_sub(argentina$cno, 4, 4))

argentina$cno12 <- as.numeric(argentina$cno12)
argentina$cno3 <- as.numeric(argentina$cno3)
argentina$cno4 <- as.numeric(argentina$cno4)
argentina$cno5 <- as.numeric(argentina$cno5)

#actividades agrícolas
argentina$caes <- argentina$PP04B_COD
argentina$caes <- as.numeric(argentina$caes)
argentina$caes <- ifelse(argentina$caes < 100, argentina$caes * 100, argentina$caes)

argentina <- argentina %>%
  mutate(rural = case_when(caes > 0 & caes <= 300 ~ 1,
                          caes >= 500 & caes < 9999 ~ 0))


#Categoría de ocupación
argentina <- argentina %>%
  mutate(categoria = case_when(CAT_OCUP == 1 ~ 1,
                               CAT_OCUP == 2 | cno3 == 1 ~ 2,
                               (CAT_OCUP == 3 | CAT_OCUP == 4) & (cno3 == 2) ~ 3,
                               (CAT_OCUP == 3 | CAT_OCUP == 4) & (cno3 == 3 | cno3 == 9 |  cno3 == 0 | is.na(cno3)) ~ 4))


#Tamaño de actividad
argentina <- argentina %>%
  mutate(tamano = case_when((PP04C > 0 & PP04C <= 5) | (PP04C == 99 & PP04C99 == 1) ~ 1,
                            (PP04C > 5 & PP04C < 99) | (PP04C == 99 & (PP04C99 == 2 | PP04C99 == 3)) ~ 2,
                            ((PP04C == 0 & PP04C99 == 0) | (PP04C == 99 & PP04C99 == 9)) & PP04A == 1 ~ 2,
                            ((PP04C == 0 & PP04C99 == 0) | (PP04C == 99 & PP04C99 == 9)) & PP04B1 == 1 ~ 1,
                            TRUE ~ NA_real_))




argentina <- argentina %>%
  mutate(pais = "Argentina",
         anio = ANO4,
         ciuo4 = NA,
         ciuo2 = NA,
         sv = NA,
         sexo = as.numeric(CH04),
         edad = as.numeric(CH06),
         pond = PONDERA,
         id = CODUSU)



#Guardado de base---------
argentina <- argentina %>%
  select(id,
         pais,
         anio,
         ciuo2,
         ciuo4,
         cno12,
         cno3,
         cno4,
         cno5,
         categoria,
         tamano,
         rural,
         sexo,
         edad,
         pond)

saveRDS(argentina, "bases/Bases_AL/Argentina/argentina_eph.RDS")

