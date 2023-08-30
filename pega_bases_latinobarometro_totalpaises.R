#Pega bases latinobarometro
rm(list = ls())

pacman::p_load(foreign, tidyverse)

latino2011 <- read.dta("C:/Users/josed/OneDrive/Bases/Latinobarometro/Latinobarometro_2011_esp.dta",
                       convert.factors = F)
load("C:/Users/josed/OneDrive/Bases/Latinobarometro/Latinobarometro2013.rdata")
load("C:/Users/josed/OneDrive/Bases/Latinobarometro/Latinobarometro_2015_Esp.rdata")
load("C:/Users/josed/OneDrive/Bases/Latinobarometro/Latinobarometro2017Esp_v20180117.rdata")
latinobarometro2018 <- readRDS("C:/Users/josed/OneDrive/Bases/Latinobarometro/Latinobarometro_2018_Esp_R_v20190303.rds")
load("C:/Users/josed/OneDrive/Bases/Latinobarometro/Latinobarometro_2020_Esp_Rdata_v1_0.rdata")


latinobarometro <- latino2011 %>%
  bind_rows(Latinobarometro2013) %>%
  bind_rows(Latinobarometro_2015_Esp) %>%
  bind_rows(Latinobarometro2017Esp_v20180117) %>%
  bind_rows(latinobarometro2018) %>%
  bind_rows(Latinobarometro_2020_Esp)

latinobarometro <- latinobarometro %>%
  mutate(idenpa = ifelse(is.na(idenpa), IDENPA, idenpa),
         numinves = ifelse(is.na(numinves), NUMINVES, numinves),
         wt = ifelse(is.na(wt), WT, wt))

latinobarometro <- latinobarometro %>%
  mutate(anio = case_when(numinves == 16 ~ 2011,
                          numinves == 17 ~ 2013,
                          numinves == 18 ~ 2015,
                          TRUE ~ numinves),
         clase_sub = ifelse(anio == 2011, S14,
                            ifelse(anio == 2013, S8,
                              ifelse(anio == 2015, S6,
                                   ifelse(anio == 2017 | anio == 2018, S1,
                                          ifelse(anio == 2020, s1, NA)))))) %>%
  select(idenpa, anio, wt, clase_sub)

saveRDS(latinobarometro, "bases/latinobarometro_paises.RDS")

rm(list = ls())
