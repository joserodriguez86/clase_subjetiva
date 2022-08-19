# Cargo librerías y bases--------------------
pacman::p_load(tidyverse, haven)

base1995 <- read_spss("bases/base_1995.sav")
base2003 <- read_spss("bases/base_2003_2004.sav")
base2007 <- read_spss("bases/base_2007.sav")
base2010 <- read_spss("bases/base_2010.sav")
load("bases/base_2021.RData")
base2021 <- base_hogar
rm(base_hogar)

base1960 <- readxl::read_xlsx("bases/germani_sampleA.xlsx", col_names = F)


# Transformación de variables -------------
#1995
base1995 <- base1995 %>%
  mutate(p004a = car::recode(p004a, "99=NA"),
         nivel_ed = factor(p004a, labels = c("Primaria incompleta", "Primaria completa",
                                             "Secundaria incompleta", "Secundaria completa",
                                             "Terciaria incompleta", "Terciaria completa",
                                             "Universitaria incompleta", "Universitaria completa",
                                             "Posgrado incompleto", "Posgrado completo")),
         clasesub_1995 = case_when(p089aa == 1 ~ 3,
                                   p089aa == 2 ~ 2,
                                   p089aa == 3 ~ 1,
                                   p089aa > 3 & p089ab == 1 ~ 3,
                                   p089aa > 3 & p089ab == 2 ~ 2,
                                   p089aa > 3 & p089ab == 3 ~ 1,
                                   TRUE ~ NA_real_),
         clasesub_1995_f = factor(clasesub_1995, labels = c("Clase media-alta", "Clase media",
                                                            "Clase trabajadora")))

#2003/2004
base2003 <- base2003 %>%
  mutate(clasesub_2003 = case_when(p64a_u == 1 ~ 1,
                                   p64a_u == 2 ~ 2,
                                   p64a_u == 3 ~ 3,
                                   p64a_u == 4 ~ 4,
                                   p64a_u == 5 ~ 5,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 1 ~ 1,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 2 ~ 2,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 3 ~ 3,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 4 ~ 4,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 5 ~ 5,
                                   TRUE ~ NA_real_),
         clasesub_2003_f = factor(clasesub_2003, labels = c("Clase baja",
                                                            "Clase obrera",
                                                            "Clase media",
                                                            "Clase media-alta",
                                                            "Clase alta")))


#2007
base2007 <- base2007 %>%
  mutate(clasesub_2007 = case_when(p161 == 1 ~ 1,
                                   p161 == 3 ~ 3,
                                   p161 == 4 ~ 4,
                                   p161 == 5 ~ 5,
                                   p161 == 6 ~ 6,
                                   (p161 > 6 | is.na(p161)) & p161a == 1 ~ 1,
                                   (p161 > 6 | is.na(p161)) & p161a == 3 ~ 3,
                                   (p161 > 6 | is.na(p161)) & p161a == 4 ~ 4,
                                   (p161 > 6 | is.na(p161)) & p161a == 5 ~ 5,
                                   (p161 > 6 | is.na(p161)) & p161a == 6 ~ 6,
                                   TRUE ~ NA_real_),
         clasesub_2007_f = factor(clasesub_2007, labels = c("Clase baja",
                                                            "Clase media-baja",
                                                            "Clase media",
                                                            "Clase media-alta",
                                                            "Clase alta")))


#2009/2010
base2010 <- base2010 %>%
  mutate(clasesub_2010 = case_when(S.83a == 1 ~ 1,
                                   S.83a == 2 ~ 2,
                                   S.83a == 3 ~ 3,
                                   S.83a == 4 ~ 4,
                                   S.83a == 5 ~ 5,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 1 ~ 1,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 2 ~ 2,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 3 ~ 3,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 4 ~ 4,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 5 ~ 5,
                                   TRUE ~ NA_real_),
         clasesub_2010_f = factor(clasesub_2010, labels = c("Clase baja",
                                                            "Clase obrera",
                                                            "Clase media-baja",
                                                            "Clase media",
                                                            "Clase media-alta")))

#2014-2015
base2015 <- base2015 %>%
  mutate(clasesub_2015 = case_when(v260a == 1 ~ 1,
                                   v260a == 2 ~ 2,
                                   v260a == 3 ~ 3,
                                   v260a == 4 ~ 4,
                                   v260a == 5 ~ 5,
                                   v260a == 6 ~ 6,
                                   is.na(v260a) & v261a == 1 ~ 1,
                                   is.na(v260a) & v261a == 2 ~ 2,
                                   is.na(v260a) & v261a == 3 ~ 3,
                                   is.na(v260a) & v261a == 4 ~ 4,
                                   is.na(v260a) & v261a == 5 ~ 5,
                                   is.na(v260a) & v261a == 6 ~ 6,
                                   TRUE ~ NA_real_),
         clasesub_2015_f = factor(clasesub_2015, labels = c("Clase baja",
                                                            "Clase obrera",
                                                            "Clase media-baja",
                                                            "Clase media",
                                                            "Clase media-alta",
                                                            "Clase alta")))

#2021
base2021 <- base2021 %>%
  mutate(clasesub_2021 = car::recode(M11.6, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 99=NA"),
         clasesub_2021_f = factor(clasesub_2021, labels = c("Clase baja",
                                                            "Clase trabajadora",
                                                            "Clase media-baja",
                                                            "Clase media",
                                                            "Clase media-alta",
                                                            "Clase alta")))
