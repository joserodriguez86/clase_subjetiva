# Cargo librerías y bases--------------------
pacman::p_load(tidyverse, haven, gridExtra, grid)

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
  mutate(clasesub_2003 = case_when(p64a_u == 1 ~ 5,
                                   p64a_u == 2 ~ 4,
                                   p64a_u == 3 ~ 3,
                                   p64a_u == 4 ~ 2,
                                   p64a_u == 5 ~ 1,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 1 ~ 5,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 2 ~ 4,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 3 ~ 3,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 4 ~ 2,
                                   (p64a_u > 5 | is.na(p64a_u)) & p64b_u == 5 ~ 1,
                                   TRUE ~ NA_real_),
         clasesub_2003_f = factor(clasesub_2003, labels = c("Clase alta",
                                                             "Clase media-alta",
                                                             "Clase media",
                                                             "Clase obrera",
                                                             "Clase baja")))



#2007
base2007 <- base2007 %>%
  mutate(clasesub_2007 = case_when(p161 == 1 ~ 6,
                                   p161 == 3 ~ 5,
                                   p161 == 4 ~ 4,
                                   p161 == 5 ~ 3,
                                   p161 == 6 ~ 1,
                                   (p161 > 6 | is.na(p161)) & p161a == 1 ~ 6,
                                   (p161 > 6 | is.na(p161)) & p161a == 3 ~ 5,
                                   (p161 > 6 | is.na(p161)) & p161a == 4 ~ 4,
                                   (p161 > 6 | is.na(p161)) & p161a == 5 ~ 3,
                                   (p161 > 6 | is.na(p161)) & p161a == 6 ~ 1,
                                   TRUE ~ NA_real_),
         clasesub_2007_f = factor(clasesub_2007, labels = c("Clase alta",
                                                             "Clase media-alta",
                                                             "Clase media",
                                                             "Clase media-baja",
                                                             "Clase baja")))


#2009/2010
base2010 <- base2010 %>%
  mutate(clasesub_2010 = case_when(S.83a == 1 ~ 5,
                                   S.83a == 2 ~ 4,
                                   S.83a == 3 ~ 3,
                                   S.83a == 4 ~ 2,
                                   S.83a == 5 ~ 1,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 1 ~ 5,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 2 ~ 4,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 3 ~ 3,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 4 ~ 2,
                                   (S.83a > 5 | is.na(S.83a)) & S.83b == 5 ~ 1,
                                   TRUE ~ NA_real_),
         clasesub_2010_f = factor(clasesub_2010, labels = c("Clase media-alta",
                                                            "Clase media",
                                                            "Clase media-baja",
                                                            "Clase obrera",
                                                            "Clase baja")))



#2014-2015
base2015 <- base2015 %>%
  mutate(clasesub_2015 = case_when(v260a == 1 ~ 6,
                                   v260a == 2 ~ 5,
                                   v260a == 3 ~ 4,
                                   v260a == 4 ~ 3,
                                   v260a == 5 ~ 2,
                                   v260a == 6 ~ 1,
                                   is.na(v260a) & v261a == 1 ~ 6,
                                   is.na(v260a) & v261a == 2 ~ 5,
                                   is.na(v260a) & v261a == 3 ~ 4,
                                   is.na(v260a) & v261a == 4 ~ 3,
                                   is.na(v260a) & v261a == 5 ~ 2,
                                   is.na(v260a) & v261a == 6 ~ 1,
                                   TRUE ~ NA_real_),
         clasesub_2015_f = factor(clasesub_2015, labels = c("Clase alta",
                                                            "Clase media-alta",
                                                            "Clase media",
                                                            "Clase media-baja",
                                                            "Clase obrera",
                                                            "Clase baja")))

#2021
base2021 <- base2021 %>%
  mutate(M11.6 = car::recode(M11.6, "99 = NA"),
         clasesub_2021_f = factor(M11.6, labels = c("Clase alta",
                                                     "Clase media-alta",
                                                     "Clase media",
                                                     "Clase media-baja",
                                                     "Clase trabajadora",
                                                     "Clase baja")))


#Parte 1--------------------
barra2003 <- base2003 %>%
  drop_na(clasesub_2003) %>%
  group_by(clasesub_2003_f) %>%
  tally() %>%
  mutate(porcentaje = n/sum(n)) %>%
  ggplot(aes(x=clasesub_2003_f, y=porcentaje)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1),
                y = porcentaje + 0.01),
            position = position_dodge(.9), vjust = 0, size = 2.5) +
  labs(title = "2003/2004") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels=scales::percent, breaks = seq(0,.7, .1), limits = c(0,.6))

barra2007 <- base2007 %>%
  drop_na(clasesub_2007) %>%
  group_by(clasesub_2007_f) %>%
  tally(pond18) %>%
  mutate(porcentaje = n/sum(n)) %>%
  ggplot(aes(x=clasesub_2007_f, y=porcentaje)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1),
                y = porcentaje + 0.01),
            position = position_dodge(.9), vjust = 0, size = 2.5) +
  labs(title = "2007/2008") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels=scales::percent, breaks = seq(0,.7, .1), limits = c(0,.6))

barra2010 <- base2010 %>%
  drop_na(clasesub_2010) %>%
  group_by(clasesub_2010_f) %>%
  tally() %>%
  mutate(porcentaje = n/sum(n)) %>%
  ggplot(aes(x=clasesub_2010_f, y=porcentaje)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1),
                y = porcentaje + 0.01),
            position = position_dodge(.9), vjust = 0, size = 2.5) +
  labs(title = "2009/2010") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(labels=scales::percent, breaks = seq(0,.7, .1), limits = c(0,.6))


barra2015 <- base2015 %>%
  drop_na(clasesub_2015) %>%
  group_by(clasesub_2015_f) %>%
  tally(f_calib3) %>%
  mutate(porcentaje = n/sum(n)) %>%
  ggplot(aes(x=clasesub_2015_f, y=porcentaje)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1),
                y = porcentaje + 0.01),
            position = position_dodge(.9), vjust = 0, size = 2.5) +
  labs(title = "2014/2015") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7)) +
  scale_y_continuous(labels=scales::percent, breaks = seq(0,.7, .1), limits = c(0,.6))


barra2021 <- base2021 %>%
  drop_na(clasesub_2021) %>%
  group_by(clasesub_2021_f) %>%
  tally(POND2R_FIN_n) %>%
  mutate(porcentaje = n/sum(n)) %>%
  ggplot(aes(x=clasesub_2021_f, y=porcentaje)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1),
                y = porcentaje + 0.01),
            position = position_dodge(.9), vjust = 0, size = 2.5) +
  labs(title = "2021") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10),
                   guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels=scales::percent, breaks = seq(0,.7, .1), limits = c(0,.6))


clases_20032021 <- grid.arrange(barra2003, barra2007, barra2010, barra2015, barra2021,
             ncol = 3)

ggsave(filename = "salidas/clases_20032021.png", dpi = 300, type = "cairo", width = 8, height = 6, clases_20032021)
