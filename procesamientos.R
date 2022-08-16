# Cargo librerías y bases--------------------
pacman::p_load(tidyverse)

base1995 <- read_spss("bases/base_1995.sav")

base1960 <- readxl::read_xlsx("bases/germani_sampleA.xlsx", col_names = F)


# Transformación de variables -------------
#1995
base1995 <- base1995 %>%
  mutate(nivel_ed = factor(p004a, labels = c("Primaria incompleta", "Primaria completa",
                                             "Secundaria incompleta", "Secundaria completa",
                                             "Terciaria incompleta", "Terciaria completa",
                                             "Universitaria incompleta", "Universitaria completa",
                                             "Posgrado incompleto", "Posgrado completo")),
         )
