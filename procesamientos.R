# Cargo librerías y bases--------------------
pacman::p_load(tidyverse)

base1995 <- read_spss("bases/base_1995.sav")
base2003 <- read_spss("bases/base_2003_2004.sav")
base2007 <- read_spss("bases/base_2007.sav")
base2010 <- read_spss("bases/base_2010.sav")

base1960 <- readxl::read_xlsx("bases/germani_sampleA.xlsx", col_names = F)


# Transformación de variables -------------
#1995
base1995 <- base1995 %>%
  mutate(nivel_ed = factor(p004a, labels = c("Primaria incompleta", "Primaria completa",
                                             "Secundaria incompleta", "Secundaria completa",
                                             "Terciaria incompleta", "Terciaria completa",
                                             "Universitaria incompleta", "Universitaria completa",
                                             "Posgrado incompleto", "Posgrado completo")))

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
