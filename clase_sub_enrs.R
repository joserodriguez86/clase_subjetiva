#Librerías---------------
pacman::p_load(tidyverse, haven)


#Bases------------
enrs <- read_sav("bases/Base de datos ENRS PISAC.sav")


#Variables-----

enrs <- enrs %>%
  mutate(
    across(c(P10k1, P10k2, P10k3, P10k4, P10k5),
           ~ case_when(
             . == 1 ~ 0,
             . == 2 ~ -1,
             . == 3 ~ 1,
             TRUE ~ NA_real_
           )),
    n_validos = rowSums(!is.na(across(c(P10k1, P10k2, P10k3, P10k4, P10k5)))),
    indice_pos_rel = if_else(
      n_validos > 0,
      rowSums(across(c(P10k1, P10k2, P10k3, P10k4, P10k5)), na.rm = TRUE) / n_validos,
      NA_real_
    ),
    ocupacion = haven::as_factor(P50e, levels = "labels"),
    ingresos = haven::as_factor(P51, levels = "labels"),
    posicion_amigo1 = as.factor(case_when(
      P10k1 == 0 ~ "Igual amigo1",
      P10k1 == -1 ~ "Más Alta amigo1",
      P10k1 == 1 ~ "Más baja amigo1",
      TRUE ~ NA_character_
    )),
  )


#Pruebas---------

enrs %>%
  group_by(ocupacion) %>%
  summarise(promedio = round(weighted.mean(indice_pos_rel, w = pond_final, na.rm = T), digits = 2))

enrs %>%
  group_by(ingresos) %>%
  summarise(promedio = round(weighted.mean(indice_pos_rel, w = pond_final, na.rm = T), digits = 2))

tabla <- enrs %>%
  filter(!is.na(posicion_amigo1)) %>%
  count(ocupacion, posicion_amigo1, wt = pond_final) %>%
  group_by(ocupacion) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

tabla_tot <- enrs %>%
  filter(!is.na(posicion_amigo1)) %>%
  count(posicion_amigo1, wt = pond_final) %>%
  mutate(prop = n / sum(n),
         ocupacion = "Total") %>%
  select(ocupacion, everything())

tabla <- tabla %>%
  add_row(tabla_tot) %>%
  filter(!is.na(ocupacion))

tabla %>%
  filter(!is.na(ocupacion)) %>%
  ggplot(aes(x = ocupacion, y = prop)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), hjust = 0.3,
            size = 3) +
  facet_wrap(~posicion_amigo1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))

ggsave("graficos/posicion_amigo1.png", width = 8, height = 5, dpi = 300)

# Solo iguales a amigos1
tabla <- enrs %>%
  filter(!is.na(posicion_amigo1)) %>%
  count(ocupacion, posicion_amigo1, wt = pond_final) %>%
  group_by(ocupacion) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  filter(posicion_amigo1 == "Igual amigo1")

tabla %>%
  filter(!is.na(ocupacion)) %>%
  ggplot(aes(x = fct_rev(ocupacion), y = prop)) +
  geom_col(alpha = .7) +
  coord_flip() +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), hjust = 1,
            size = 2.8) +
  labs(title = "Proporción de personas que se consideran de igual \nposición que su amigo 1",
       subtitle = "Por ocupación",
       caption = "Fuente: ENRS 2019-2020") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))

ggsave("graficos/posicion_amigo1_igual.png", width = 8, height = 5, dpi = 300)

tabla <- enrs %>%
  filter(!is.na(posicion_amigo1)) %>%
  count(ocupacion, posicion_amigo1, wt = pond_final) %>%
  group_by(ocupacion) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  filter(posicion_amigo1 == "Más Alta amigo1")

tabla %>%
  filter(!is.na(ocupacion)) %>%
  ggplot(aes(x = fct_rev(ocupacion), y = prop)) +
  geom_col(alpha = .7) +
  coord_flip() +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), hjust = 1,
            size = 2.8) +
  labs(title = "Proporción de personas que se consideran en una \nposición más baja que su amigo 1",
       subtitle = "Por ocupación",
       caption = "Fuente: ENRS 2019-2020") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))

ggsave("graficos/posicion_amigo1_alta.png", width = 8, height = 5, dpi = 300)


tabla <- enrs %>%
  filter(!is.na(posicion_amigo1)) %>%
  count(ocupacion, posicion_amigo1, wt = pond_final) %>%
  group_by(ocupacion) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  filter(posicion_amigo1 == "Más baja amigo1")

tabla %>%
  filter(!is.na(ocupacion)) %>%
  ggplot(aes(x = fct_rev(ocupacion), y = prop)) +
  geom_col(alpha = .7) +
  coord_flip() +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), hjust = 1,
            size = 2.8) +
  labs(title = "Proporción de personas que se consideran en una \nposición más alta que su amigo 1",
       subtitle = "Por ocupación",
       caption = "Fuente: ENRS 2019-2020") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))

ggsave("graficos/posicion_amigo1_baja.png", width = 8, height = 5, dpi = 300)

# Amigos barras apiladas
tabla <- enrs %>%
  filter(!is.na(posicion_amigo1)) %>%
  count(ocupacion, posicion_amigo1, wt = pond_final) %>%
  group_by(ocupacion) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

tabla %>%
  filter(!is.na(ocupacion)) %>%
  ggplot(aes(x = fct_rev(ocupacion), y = prop, fill = posicion_amigo1)) +
  geom_col(alpha = .7) +
  coord_flip() +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), hjust = 1,
            size = 2.5, position = position_stack(vjust = 0.5)) +
  labs(title = "Proporción de personas que se consideran en una \nposición más alta, igual o más baja que su amigo 1",
       subtitle = "Por ocupación",
       caption = "Fuente: ENRS 2019-2020") +
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40))

ggsave("graficos/posicion_amigo1_barras_apiladas.png", width = 8, height = 5, dpi = 300)

# Indice promedio por ocupacion
enrs %>%
  filter(!is.na(ocupacion), ocupacion != "Ns/Nr") %>%
  ggplot(aes(x = indice_pos_rel, weight = pond_final)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  facet_wrap(~ocupacion, labeller = as_labeller(function(x) str_trunc(x, width = 30))) +
  theme(strip.text = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Distribución del índice de posición relativa por ocupación",
       caption = "Fuente: ENRS 2019-2020")

ggsave("graficos/indice_posicion_relativa_ocupacion.png", width = 8, height = 5, dpi = 300)
