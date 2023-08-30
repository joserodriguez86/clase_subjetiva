#Librerías y bases----------------------

pacman::p_load(tidyverse, haven, gridExtra, grid, occupar, ggpubr, ggsci, gtsummary,
               survey, jtools, huxtable, nnet, broom, kableExtra, flextable, gridExtra,
               googledrive)

theme_set(theme_bw())

rm(list = ls())

#base 2021
load("bases/base_2021.RData")
base2021 <- base_pirc
rm(base_pirc)

#base wvs
wvs7 <- readRDS("bases/WVS_Cross-National_Wave_7_Rds_v5_0.rds")

wvs <- readRDS("bases/WVS_Trend_v2_0.rds")

#Bases pegadas de encuestas de hogares latinoamericanas
base_AL <- readRDS("bases/Bases_AL/base_al.RDS")


#Construcción de variables-------------

#2021
base2021 <- base2021 %>%
  mutate(M11.6 = car::recode(M11.6, "99 = NA"),
         clasesub_2021_f = factor(M11.6, labels = c("Alta",
                                                    "Media-alta",
                                                    "Media",
                                                    "Media-baja",
                                                    "Trabajadora",
                                                    "Baja")),
         clasesub_recod = car::recode(M11.6, "1:2=1; 3=2; 4=3; 5=4; 6=5"),
         clasesub_recod_f = factor(clasesub_recod, labels = c("Media-alta",
                                                              "Media",
                                                              "Media-baja",
                                                              "Trabajadora",
                                                              "Baja")),
         clasesub_dic = car::recode(clasesub_recod, "1:3=1; 4:5=2"),
         clasesub_dic_f = factor(clasesub_dic, labels = c("C.s medias", "C. obrera / baja")),
         ciuo = CIUO_encuestado,
         sv = car::recode(M3.9, "1:2=1; 3=0; 99=NA"),
         categoria = case_when(M3.5 == 1 ~ 1,
                               M3.5 == 2 & sv == 1 ~ 1,
                               M3.5 == 2 & (sv != 1 | is.na(sv)) ~ 2,
                               M3.5 >= 3 & M3.5 < 99 ~ 3,
                               TRUE ~ NA_real_),
         tamano = case_when(M3.6 <= 2 ~ 1,
                            M3.6 > 2 & M3.6 < 99 ~ 2,
                            TRUE ~ NA_real_),
         rural = case_when(CAES_letra == "A" ~ 1,
                           TRUE ~ 0),
         sexo_f = factor(SEXO, labels = c("Varón", "Mujer")),
         edad_grupo = factor(car::recode(M2.4, "18:29=1; 30:39=2; 40:65=3;
                                    66:hi=4; else=NA"),
                             labels = c("18-29", "30-39", "40-65", "66-")),
         nivel_ed = case_when(M2.9 == 0 | M2.9 == 1 | M2.9 == 2 | M2.9 == 3 |
                                M2.9 == 9 | M2.9 == 10 ~ "Primario",
                              M2.9 == 4 | M2.9 == 5 ~ "Secundario",
                              M2.9 >= 6 & M2.9 <= 8 ~ "Superior",
                              TRUE ~ NA_character_),
         formal = factor(case_when(M3.10 == 1 | M3.10 == 2 | M3.15 == 3 ~ "Formal",
                            TRUE ~ "Informal")),
         rama = case_when(CAES_letra %in% c("A", "B") ~ "Act. primarias",
                          CAES_letra %in% c("C", "D", "E") ~ "Industria",
                          CAES_letra == "F" ~ "Construcción",
                          CAES_letra == "G" ~ "Comercio",
                          CAES_letra %in% c("H", "I", "J", "K", "L",
                                            "M", "N", "P", "Q", "R", "S",
                                            "T") ~ "Servicios",
                          CAES_letra %in% c("O", "U") ~ "Adm. pública"),
         afiliacion = case_when(M3.13.A == 1 ~ "Sindicalizado",
                                TRUE ~ "No sindicalizado"),
         sv_o = car::recode(M12.22, "1:2=1; 3=0; 99=NA"),
         categoria_o = case_when(M12.20 == 1 ~ 1,
                                 M12.20 == 2 & sv_o < 3 ~ 1,
                                 M12.20 == 2 & (sv_o >= 3 | is.na(sv_o)) ~ 2,
                                 M12.20 >= 3 & M12.20 < 99 ~ 3,
                                 TRUE ~ NA_real_),
         tamano_o = case_when(M12.21 <= 2 ~ 1,
                              M12.21 > 2 & M12.21 < 99 ~ 2,
                              TRUE ~ NA_real_),
         rural_o = case_when(M12.19 == 1 ~ 1,
                             TRUE ~ 0),
         ipcf = itf_imp / MIEMBROS,
         log_ipcf = log(ipcf),
         politica = car::recode(M16.1, "1=1; 4=2; 2:3=3; 5=4; 6=5; 7:8=6; else=NA"),
         politica = factor(politica, labels = c("Kirchnerismo", "Peronismo no K", "UCR/PRO", "Izquierda",
                                                "Libertarios", "Ninguno")))

#WVS
wvs7 <- wvs7 %>%
  mutate(clase_sub = car::recode(Q287, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA"),
         clase_sub_f = factor(clase_sub, labels = c("Alta", "Media alta",
                                                    "Media baja", "Trabajadora",
                                                    "Baja")),
         pais_anio = paste(B_COUNTRY_ALPHA, as.character(A_YEAR), sep = "-"),
         sexo = case_when(Q260 == 1 ~ "Varón",
                          Q260 == 2 ~ "Mujer",
                          TRUE ~ NA))

wvs <- wvs %>%
  mutate(clase_sub = car::recode(X045, "1=1; 2=2; 3=3; 4=4; 5=5; else=NA"),
         clase_sub_f = factor(clase_sub, labels = c("Alta", "Media alta",
                                                    "Media baja", "Trabajadora",
                                                    "Baja")),
         pais_anio = paste(COW_ALPHA, as.character(S020), sep = "-"),
         sexo = case_when(X001 == 1 ~ "Varón",
                          X001 == 2 ~ "Mujer",
                          TRUE ~ NA))

#EGP
##Destino
base2021 <- base2021 %>%
  mutate(egp = case_when(
    #Patrones
    categoria == 1 & tamano == 2 ~ 1,
    categoria == 1 & tamano == 1 & rural != 1 ~ 5,
    categoria == 1 & tamano == 1 & rural == 1 ~ 7,

    #Cuenta propia
    categoria == 2 & ((ciuo >= 1000 & ciuo < 1400)) ~ 1,
    categoria == 2 & (ciuo >= 1400 & ciuo < 2000) ~ 2,

    categoria == 2 & ciuo %in% c(2165, 2221, 2222, 2513:2519, 2320, 2330, 2341, 2342,
                                 2351:2359, 2421:2424, 2621, 2622, 2651:2659, 2636) ~ 2,
    categoria == 2 & (ciuo >= 2000 & ciuo < 3000) ~ 1,

    categoria == 2 & ciuo %in% c(3221, 3222, 3311:3314, 3411:3413) ~ 3,
    categoria == 2 & (ciuo >= 3000 & ciuo < 4000) ~ 2,

    categoria == 2 & (ciuo %in% c(5211, 5212, 5413, 5414) |
                        (ciuo >= 9000 & ciuo < 9999) | (ciuo >= 6300 & ciuo < 7000)) ~ 10,
    categoria == 2 & ((ciuo >= 4000 & ciuo < 6000) | (ciuo >= 7000 & ciuo < 9000)) ~ 6,
    categoria == 2 & (ciuo >= 6000 & ciuo < 6300) ~ 7,

    #Empleados
    categoria == 3 & (ciuo >= 1000 & ciuo < 1400) ~ 1,
    categoria == 3 & (ciuo >= 1400 & ciuo < 2000) ~ 2,

    categoria == 3 & ciuo %in% c(2165, 2221, 2222, 2513:2519, 2320, 2330, 2341, 2342,
                                 2351:2359, 2421:2424, 2621, 2622, 2651:2659, 2636) ~ 2,
    categoria == 3 & (ciuo >= 2000 & ciuo < 3000) ~ 1,

    categoria == 3 & ciuo %in% c(3221, 3222, 3311:3314, 3411:3413) & (sv != 1 | is.na(sv)) ~ 3,
    categoria == 3 & (ciuo >= 3000 & ciuo < 4000) ~ 2,

    categoria == 3 & ciuo %in% c(4412, 4419) & (sv != 1 | is.na(sv)) ~ 4,
    categoria == 3 & (ciuo >= 4000 & ciuo < 5000) & (sv != 1 | is.na(sv)) ~ 3,

    categoria == 3 & ciuo %in% c(5111:5113) & (sv != 1 | is.na(sv)) ~ 3,
    categoria == 3 & ciuo %in% c(5120, 5141, 5142, 5153, 5163, 5164, 5165, 5169) & (sv != 1 | is.na(sv)) ~ 9,
    categoria == 3 & ciuo %in% c(5211, 5212, 5413, 5414, 5419) ~ 10,
    categoria == 3 & ciuo %in% c(5411, 5412) ~ 9,
    categoria == 3 & (ciuo >= 5000 & ciuo < 6000) & (sv != 1 | is.na(sv)) ~ 4,

    categoria == 3 & (ciuo >= 3000 & ciuo < 6000) & sv == 1 ~ 2, #supervisores NM

    categoria == 3 & (ciuo >= 6000 & ciuo < 7000) ~ 11,

    categoria == 3 & (ciuo >= 7000 & ciuo < 9000) & (sv != 1 | is.na(sv)) ~ 9,
    categoria == 3 & (ciuo >= 9000 & ciuo < 9999) & (sv != 1 | is.na(sv)) ~ 10,
    categoria == 3 & (ciuo >= 7000 & ciuo < 9999) & sv == 1 ~ 8, #supervisores M
    ciuo == 110 ~ 2,
    ciuo == 210 | ciuo == 310 ~ 8,
    TRUE ~ NA_real_
  ))

base2021$egp_f <- factor(base2021$egp, labels = c("I", "II", "IIIa", "IIIb",
                                                  "IVa", "IVb", "IVc", "V",
                                                  "VI", "VIIa", "VIIb"))

base2021$egp5 <- car::recode(base2021$egp, "1:2=1; 3:4=2; 5:7=3; 8:9=4; 10:11=5")
base2021$egp5_f <- factor(base2021$egp5, labels = c("I+II", "III", "IV", "V+VI", "VII"))

base2021$egp6 <- car::recode(base2021$egp, "1:2=1; 3:4=2; c(5, 7)=3; 6=4; 8:9=5; 10:11=6")
base2021$egp5_f <- factor(base2021$egp6, labels = c("I+II", "III", "IVac", "IVb", "V+VI", "VII"))


##Origen
base2021 <- base2021 %>%
  mutate(egp_origen = case_when(
    #Patrones
    categoria_o == 1 & tamano_o == 2 ~ 1,
    categoria_o == 1 & tamano_o == 1 & rural_o != 1 ~ 5,
    categoria_o == 1 & tamano_o == 1 & rural_o == 1 ~ 7,

    #Cuenta propia
    categoria_o == 2 & ((CIUO_origen >= 1000 & CIUO_origen < 1400)) ~ 1,
    categoria_o == 2 & (CIUO_origen >= 1400 & CIUO_origen < 2000) ~ 2,

    categoria_o == 2 & CIUO_origen %in% c(2165, 2221, 2222, 2513:2519, 2320, 2330, 2341, 2342,
                                          2351:2359, 2421:2424, 2621, 2622, 2651:2659, 2636) ~ 2,
    categoria_o == 2 & (CIUO_origen >= 2000 & CIUO_origen < 3000) ~ 1,

    categoria_o == 2 & CIUO_origen %in% c(3221, 3222, 3311:3314, 3411:3413) ~ 3,
    categoria_o == 2 & (CIUO_origen >= 3000 & CIUO_origen < 4000) ~ 2,

    categoria_o == 2 & (CIUO_origen %in% c(5211, 5212, 5413, 5414) |
                          (CIUO_origen >= 9000 & CIUO_origen < 9999) | (CIUO_origen >= 6300 & CIUO_origen < 7000)) ~ 10,
    categoria_o == 2 & ((CIUO_origen >= 4000 & CIUO_origen < 6000) | (CIUO_origen >= 7000 & CIUO_origen < 9000)) ~ 6,
    categoria_o == 2 & (CIUO_origen >= 6000 & CIUO_origen < 6300) ~ 7,

    #Empleados
    categoria_o == 3 & (CIUO_origen >= 1000 & CIUO_origen < 1400) ~ 1,
    categoria_o == 3 & (CIUO_origen >= 1400 & CIUO_origen < 2000) ~ 2,

    categoria_o == 3 & CIUO_origen %in% c(2165, 2221, 2222, 2513:2519, 2320, 2330, 2341, 2342,
                                          2351:2359, 2421:2424, 2621, 2622, 2651:2659, 2636) ~ 2,
    categoria_o == 3 & (CIUO_origen >= 2000 & CIUO_origen < 3000) ~ 1,

    categoria_o == 3 & CIUO_origen %in% c(3221, 3222, 3311:3314, 3411:3413) & (sv_o != 1 | is.na(sv_o)) ~ 3,
    categoria_o == 3 & (CIUO_origen >= 3000 & CIUO_origen < 4000) ~ 2,

    categoria_o == 3 & CIUO_origen %in% c(4412, 4419) & (sv_o != 1 | is.na(sv_o)) ~ 4,
    categoria_o == 3 & (CIUO_origen >= 4000 & CIUO_origen < 5000) & (sv_o != 1 | is.na(sv_o)) ~ 3,

    categoria_o == 3 & CIUO_origen %in% c(5111:5113) & (sv_o != 1 | is.na(sv_o)) ~ 3,
    categoria_o == 3 & CIUO_origen %in% c(5120, 5141, 5142, 5153, 5163, 5164, 5165, 5169) & (sv_o != 1 | is.na(sv_o)) ~ 9,
    categoria_o == 3 & CIUO_origen %in% c(5211, 5212, 5413, 5414, 5419) ~ 10,
    categoria_o == 3 & CIUO_origen %in% c(5411, 5412) ~ 9,
    categoria_o == 3 & (CIUO_origen >= 5000 & CIUO_origen < 6000) & (sv_o != 1 | is.na(sv_o)) ~ 4,

    categoria_o == 3 & (CIUO_origen >= 3000 & CIUO_origen < 6000) & sv_o == 1 ~ 2, #supervisores NM

    categoria_o == 3 & (CIUO_origen >= 6000 & CIUO_origen < 7000) ~ 11,

    categoria_o == 3 & (CIUO_origen >= 7000 & CIUO_origen < 9000) & (sv_o != 1 | is.na(sv_o)) ~ 9,
    categoria_o == 3 & (CIUO_origen >= 9000 & CIUO_origen < 9999) & (sv_o != 1 | is.na(sv_o)) ~ 10,
    categoria_o == 3 & (CIUO_origen >= 7000 & CIUO_origen < 9999) & sv_o == 1 ~ 8, #supervisores M
    CIUO_origen == 110 ~ 2,
    CIUO_origen == 210 | CIUO_origen == 310 ~ 8,
    TRUE ~ NA_real_
  ))

base2021$egp_origen_f <- factor(base2021$egp_origen, labels = c("I", "II", "IIIa",
                                                                "IIIb",
                                                         "IVa", "IVb", "IVc", "V",
                                                         "VI", "VIIa", "VIIb"))

base2021$egp5_origen <- car::recode(base2021$egp_origen, "1:2=1; 3:4=2; 5:7=3; 8:9=4; 10:11=5")
base2021$egp5_origen_f <- factor(base2021$egp5_origen,
                                 labels = c("I+II", "III", "IV", "V+VI", "VII"))

base2021$egp6_origen <- car::recode(base2021$egp_origen, "1:2=1; 3:4=2; c(5, 7)=3; 6=4; 8:9=5; 10:11=6")
base2021$egp5_origen_f <- factor(base2021$egp6_origen, labels = c("I+II", "III", "IVac", "IVb", "V+VI", "VII"))

#Tendencias--------------------

#Argentina
tabla <- wvs %>%
  filter(X003 >= 18, S003 == 32) %>%
  group_by(X025R, S020) %>%
  tally(S017) %>%
  pivot_wider(names_from = S020, values_from = n)


wvs %>%
  filter(S020 >= 1995, X003 >= 18, S003 == 32) %>%
  drop_na(clase_sub_f) %>%
  group_by(S020, clase_sub_f) %>%
  tally(S017) %>%
  mutate(porcentaje = n/sum(n)) %>%
  ggplot(aes(x=clase_sub_f, y=porcentaje)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1),
                y = porcentaje + 0.01),
            position = position_dodge(.9), vjust = 0, size = 2.5) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, vjust = .7),
        axis.text.y = element_text(size = 8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  scale_y_continuous(labels=scales::percent, breaks = seq(0,.7, .1), limits = c(0,.6)) +
  facet_wrap(~S020, ncol = 3)

ggsave(filename = "salidas_articulo/barras_wvs_arg.jpg", dpi = 300, width = 8, height = 6)

graf_arg_sub <- wvs %>%
  filter(S020 >= 1995, X003 >= 18, S003 == 32) %>%
  drop_na(clase_sub_f) %>%
  group_by(pais_anio, sexo, clase_sub_f) %>%
  tally(S017) %>%
  mutate(porcentaje = ifelse(sexo == "Varón", -1*(n/sum(n)), n/sum(n)))

graf_arg_sub %>%
  ggplot(aes(x=clase_sub_f, y=porcentaje, fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_fill_grey(name = "Sexo", start = 0.3, end = 0.6) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7.5, vjust = .7),
        axis.text.y = element_text(size = 10)) +
  scale_x_discrete(limits = rev(levels(wvs$clase_sub_f))) +
  scale_y_continuous(labels= function(z) paste0(abs(z*100), "%"),
                     # breaks = seq(-.6, .6, .2),
                     limits = c(-.6, .6)) +
  coord_flip() +
  facet_wrap(~pais_anio, ncol = 3)

ggsave(filename = "salidas_articulo/piramide_wvs_arg.jpg", dpi = 300, width = 8, height = 5)


#Países 2017-2022 subjetiva

graf_paises_sub <- wvs7 %>%
  filter(B_COUNTRY %in% c(32, 76, 152, 218, 484, 604, 858)) %>%
  drop_na(clase_sub_f) %>%
  group_by(pais_anio, sexo, clase_sub_f) %>%
  tally(W_WEIGHT) %>%
  mutate(porcentaje = ifelse(sexo == "Varón", -1*(n/sum(n)), n/sum(n)))

graf_paises_sub %>%
  ggplot(aes(x=clase_sub_f, y=porcentaje, fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_fill_grey(name = "Sexo", start = 0.3, end = 0.6) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7.5, vjust = .7),
        axis.text.y = element_text(size = 10)) +
  scale_x_discrete(limits = rev(levels(wvs$clase_sub_f))) +
  scale_y_continuous(labels= function(z) paste0(abs(z*100), "%"),
                     # breaks = seq(-.55, .55, .2),
                     limits = c(-.6, .6)) +
  coord_flip() +
  facet_wrap(~pais_anio, ncol = 3)

ggsave(filename = "salidas_articulo/piramides_wvs_paises.jpg", dpi = 300, width = 8, height = 5)

#Países 2017-2022 EGP
graf_paises_egp <- base_AL %>%
  drop_na(egp) %>%
  group_by(pais_anio, sexo_f, egp_f) %>%
  tally(pond) %>%
  mutate(porcentaje = ifelse(sexo_f == "Varón", -1*(n/sum(n)), n/sum(n)))

graf_paises_egp %>%
  ggplot(aes(x=egp_f, y=porcentaje, fill = sexo_f)) +
  geom_bar(stat = "identity") +
  scale_fill_grey(name = "Sexo", start = 0.3, end = 0.6) +
  # geom_label(aes(label = scales::percent(abs(porcentaje), accuracy = 0.1),
  #               y = porcentaje + 0.01),
  #           position = position_dodge(0), size = 2.3, fill = "white") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7.5, vjust = .7),
        axis.text.y = element_text(size = 10)) +
  scale_x_discrete(limits = rev(levels(base_AL$egp_f))) +
  scale_y_continuous(labels= function(z) paste0(abs(z*100), "%"),
                     # breaks = seq(-.5, .5, .15),
                     limits = c(-.5, .5)) +
  coord_flip() +
  facet_wrap(~pais_anio, ncol = 3)

ggsave(filename = "salidas_articulo/piramides_egp_paises.jpg", dpi = 300, width = 8, height = 5)

#Unión gráficos
subjetiva <- wvs7 %>%
  filter(B_COUNTRY %in% c(32, 76, 152, 218, 484, 604, 858)) %>%
  drop_na(clase_sub_f) %>%
  group_by(pais_anio, clase_sub_f) %>%
  tally(W_WEIGHT) %>%
  mutate(porcentaje = n/sum(n)) %>%
  ggplot(aes(x=clase_sub_f, y=porcentaje)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1),
                y = porcentaje + 0.01),
            position = position_dodge(.9), vjust = 0, size = 2.3) +
  labs(title = "Clase subjetiva") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7.5, vjust = .7),
        axis.text.y = element_text(size = 8)) +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(labels=scales::percent, breaks = seq(0,.7, .20), limits = c(0,.6)) +
  facet_wrap(~pais_anio, ncol = 1)

objetiva <- base_AL %>%
  drop_na(egp) %>%
  group_by(pais_anio, egp_f) %>%
  tally(pond) %>%
  mutate(porcentaje = n/sum(n)) %>%
  ggplot(aes(x=egp_f, y=porcentaje)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(porcentaje, accuracy = 0.1),
                y = porcentaje + 0.01),
            position = position_dodge(.9), vjust = 0, size = 2.3) +
  labs(title = "Clase objetiva") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12),
        axis.text.x = element_text(size = 7.5, vjust = .7),
        axis.text.y = element_text(size = 8)) +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(labels=scales::percent, breaks = seq(0,.7, .20), limits = c(0,.6)) +
  facet_wrap(~pais_anio, ncol = 1)

grafico_subjobj <- grid.arrange(subjetiva, objetiva, ncol = 2)

ggsave(grafico_subjobj, filename = "salidas_articulo/subjetiva_objetiva.jpg", dpi = 300, width = 5.5, height = 8)


#Regresiones----
##Descriptivos----
theme_gtsummary_language("es", decimal.mark = ",", big.mark = ".")

base2021 %>%
  filter(M2.12 <= 2) %>%
  svydesign(data = ., ids = ~ 1, weights = ~POND2R_FIN_n) %>%
  tbl_svysummary(include = c(clasesub_2021_f, egp5_f, egp5_origen_f, nivel_ed_f, sexo_f,
                             formal, afiliacion, rama, politica, M2.4, ipcf),
                 label = c(clasesub_2021_f ~ "Clase subjetiva",
                           egp5_f ~ "Clase encuestado",
                           egp5_origen_f ~ "Clase origen",
                           nivel_ed_f ~ "Nivel educativo",
                           sexo_f ~ "Sexo",
                           M2.4 ~ "Edad",
                           formal ~ "Condición laboral",
                           afiliacion ~ "Afiliación",
                           rama ~ "Rama de actividad",
                           politica ~ "Posicionamiento político",
                           ipcf ~ "Ingreso per cápita familiar"),
                 statistic = list(
                   all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} ({p}%)"),
                 digits = list(all_categorical() ~ c(0, 1),
                               all_continuous() ~ c(0, 1)),
                 missing = "no")  %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "salidas_articulo/tabla_descriptiva.docx")


##Regresión binomial--------
base2021 <- base2021 %>%
  filter(M2.12 <= 2)

base2021$clasesub_dic[base2021$clasesub_dic == 2] <- 0
base2021$egp5_f <- relevel(base2021$egp5_f, ref = "VII")
base2021$egp5_origen_f <- relevel(base2021$egp5_origen_f, ref = "VII")
base2021$rama <- relevel(as.factor(base2021$rama), ref = "Construcción")
base2021$politica <- relevel(base2021$politica, ref = "Ninguno")
base2021$log_ipcf <- ifelse(base2021$log_ipcf == -Inf, NA, base2021$log_ipcf)
base2021$formal <- relevel(base2021$formal, ref = "Informal")

binomial1 <- glm(clasesub_dic ~ egp5_f + sexo_f + M2.4 + nivel_ed_f + log_ipcf,
                 data = base2021, weights = POND2R_FIN_n, family = binomial(link = "logit"))

binomial2 <- glm(clasesub_dic ~ egp5_f + sexo_f + M2.4 + nivel_ed_f + log_ipcf + formal + rama,
                 data = base2021, weights = POND2R_FIN_n, family = binomial(link = "logit"))

binomial3 <- glm(clasesub_dic ~ egp5_f + sexo_f + M2.4 + nivel_ed_f + log_ipcf + formal + rama +
                   afiliacion + politica,
                 data = base2021, weights = POND2R_FIN_n, family = binomial(link = "logit"))

binomial4 <- glm(clasesub_dic ~ egp5_f + sexo_f + M2.4 + nivel_ed_f + log_ipcf + formal + rama +
                   afiliacion + politica + egp5_origen_f,
                 data = base2021, weights = POND2R_FIN_n, family = binomial(link = "logit"))

# binomial5 <- glm(clasesub_dic ~ sexo_f + edad_grupo + nivel_ed_f + formal + rama +
#                    afiliacion + politica + egp5_f * egp5_origen_f,
#                  data = base2021, weights = POND2R_FIN_n, family = binomial(link = "logit"))

car::vif(binomial4)

export_summs(binomial1, binomial2, binomial3, binomial4, exp = T,
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             coefs = c("I+II" = "egp5_fI+II",
                       "III" = "egp5_fIII",
                       "IVac" = "egp5_fIVac",
                       "IVb" = "egp5_fIVb",
                       "V+VI" = "egp5_fV+VI",
                       "Mujer" = "sexo_fMujer",
                       "Edad" = "M2.4",
                       "Secundario" = "nivel_ed_fSecundario",
                       "Superior" = "nivel_ed_fSuperior",
                       "IPCF(log)" = "log_ipcf",
                       "Formal" = "formalFormal",
                       "Act. Primarias" = "ramaAct. primarias",
                       "Adm. Pública" = "ramaAdm. pública",
                       "Comercio" = "ramaComercio",
                       "Servicios" = "ramaServicios",
                       "Industria" = "ramaIndustria",
                       "Sindicalizado" = "afiliacionSindicalizado",
                       "Kirchnerismo" = "politicaKirchnerismo",
                       "Peronismo no K" = "politicaPeronismo no K",
                       "UCR/PRO" = "politicaUCR/PRO",
                       "Izquierda" = "politicaIzquierda",
                       "Libertarios" = "politicaLibertarios",
                       "Origen: I+II" = "egp5_origen_fI+II",
                       "Origen: III" = "egp5_origen_fIII",
                       "Origen: IVac" = "egp5_origen_fIVac",
                       "Origen: IVb" = "egp5_origen_fIVb",
                       "Origen: V+VI" = "egp5_origen_fV+VI"),
             to.file = "docx", file.name = "salidas_articulo/reg_binomial.docx")

##Regresión multinomial----
base2021 <- base2021 %>%
  mutate(clasesub4 = factor(car::recode(M11.6, "1:3 = 1; 4=2; 5=3; 6=4"),
                            labels = c("Clase media", "Clase media-baja", "Clase trabajadora",
                                       "Clase baja")),
         clasesub4 = relevel(clasesub4, ref = "Clase media-baja"))

multinomial <- multinom(clasesub4 ~ egp5_f + sexo_f + M2.4 + nivel_ed_f + log_ipcf + formal + rama +
                          afiliacion + politica + egp5_origen_f, data = base2021, weights = POND2R_FIN_n,
                        trace = F)

tidied <- tidy(multinomial)
tidied$estimate <- exp(tidied$estimate)

models <- list()
models[["Media vs media-baja"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "Clase media", ])
models[["Trabajadora vs media-baja"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "Clase trabajadora", ])
models[["Baja vs media-baja"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "Clase baja", ])

export_summs(models,
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             coefs = c("I+II" = "egp5_fI+II",
                       "III" = "egp5_fIII",
                       "IVac" = "egp5_fIVac",
                       "IVb" = "egp5_fIVb",
                       "V+VI" = "egp5_fV+VI",
                       "Mujer" = "sexo_fMujer",
                       "Edad" = "M2.4",
                       "Secundario" = "nivel_ed_fSecundario",
                       "Superior" = "nivel_ed_fSuperior",
                       "IPCF(log)" = "log_ipcf",
                       "Formal" = "formalFormal",
                       "Act. Primarias" = "ramaAct. primarias",
                       "Adm. Pública" = "ramaAdm. pública",
                       "Comercio" = "ramaComercio",
                       "Servicios" = "ramaServicios",
                       "Industria" = "ramaIndustria",
                       "Sindicalizado" = "afiliacionSindicalizado",
                       "Kirchnerismo" = "politicaKirchnerismo",
                       "Peronismo no K" = "politicaPeronismo no K",
                       "UCR/PRO" = "politicaUCR/PRO",
                       "Izquierda" = "politicaIzquierda",
                       "Libertarios" = "politicaLibertarios",
                       "Origen: I+II" = "egp5_origen_fI+II",
                       "Origen: III" = "egp5_origen_fIII",
                       "Origen: IVac" = "egp5_origen_fIVac",
                       "Origen: IVb" = "egp5_origen_fIVb",
                       "Origen: V+VI" = "egp5_origen_fV+VI"),
             to.file = "docx", file.name = "salidas_articulo/reg_multi.docx")

DescTools::PseudoR2(multinomial)
