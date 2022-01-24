###########################################################################
###########################################################################
##############    Talleres Aprendiendo Metodología x Vale    ##############
############## Taller de Visualización de Datos Panel con R  ##############           
##############   Cristóbal Ortiz, Asistente de Datos OLES    ##############
##############              Update: 24/01/2022               ##############
###########################################################################
###########################################################################

# Librerías ---------------------------------------------------------------

library(tidyverse) #manejo y visualización de datos
library(sjlabelled) #manejo de etiquetas
library(sjmisc) # manejo de datos (sólo usaremos frq)
library(panelr) # manejo de datos panel

# Cargar dataset ----------------------------------------------------------

## Cargar base de datos
remove(list = ls())

load(url("https://dataverse.harvard.edu/api/access/datafile/4606527"))
  
frq(elsoc_wide_2016_2019$tipo_atricion)
frq(elsoc_wide_2016_2019$tipo_caso)

elsoc_wide <- elsoc_wide_2016_2019 %>% 
  filter(tipo_atricion == 1 & tipo_caso != 2) #filtrar atrición entre 2016-19 y casos c/inconsistencias mayores

## Transformar base de datos de wide a long

elsoc_long <- long_panel(data = elsoc_wide, #base de datos formato wide
                         prefix = "_w0", #caracteres antes de la etiqueta de cada ola
                         begin = 1, #etiqueta de la primera ola
                         end = 4, #etiqueta de la última ola
                         label_location = "end", #indica donde se localiza la etiqueta asociada a la ola 
                         id = "idencuesta", #indica identificador individual
                         wave = "ola") #nombre que tomará la variable que indica periodo. 

elsoc_long[elsoc_long == -999 | elsoc_long == -888] <- NA #recodificar No sabe y No responde en NA

elsoc_long <- elsoc_long %>% 
  mutate(ola = factor(ola, labels = c("2016", "2017", "2018", "2019")))

save(elsoc_long, file = "datos/elsoc-long.RData")

load("datos/elsoc-long.RData") #alternativamente cargar esta base que ya contiene las transformaciones antes descritas

# Estructura de ggplot2 ---------------------------------------------------

ggplot(data = DATOS, aes(MAPEOS)) +
  GEOM_FUNCION(stat = ESTADÍSTICAS,
               position = POSICIÓN) +
  FUNCIÓN_COORDENADAS +
  FUNCIÓN_FACETAS

# Gráfico de barra --------------------------------------------------------

elsoc_long %>% 
  ungroup() %>% # desagrupa si es que hay alguna variable de desagrupación
  count(ola, c10_01) %>% # contar combinaciones entre variables de interés
  drop_na() %>% # desecha NA para mejorar visualización
  group_by(ola) %>% # agrupa por ola
  mutate(freq = (n/sum(n))) %>% # calcula frecuencia por ola
  as_label(c10_01, ola) %>% # variables toman los valores de su etiqueta 
  ggplot(aes(x = c10_01, 
             y = freq,
             fill = ola,
             label = as.character(scales::percent(freq, accuracy = .1)))) +
  geom_col(position = 'dodge2') + 
  geom_text(position = position_dodge(width  = .9),
            size = 2.5,
            vjust = -0.5) +
  labs(title = get_label(elsoc_long$c10_01),
       x = "",
       y = "") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  theme(legend.position = 'top',
        legend.title = element_blank())


# Gráfico de barra apilada ------------------------------------------------

elsoc_long %>% 
  ungroup() %>% 
  count(ola, c10_03) %>% 
  drop_na() %>% 
  group_by(ola) %>% 
  mutate(freq = (n/sum(n))) %>% 
  as_label(c10_03, ola) %>%  
  ggplot(aes(x = ola, 
             y = freq,
             fill = c10_03,
             label = as.character(scales::percent(freq, accuracy = .1)))) +
  geom_col(position = 'stack') + 
  geom_text(position = position_stack(vjust = .5),
            size = 3) +
  labs(title = get_label(elsoc_long$c10_03),
       x = "",
       y = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'top',
        legend.title = element_blank())


# Gráfico barra apilada resumido ------------------------------------------

elsoc_long %>% 
  ungroup() %>% 
  mutate(c10_03 = factor(car::recode(c10_03, "c(1,2) = 1; c(3) = 2; c(4,5) = 3"),
                         labels = c("Totalmente en desacuerdo\no en desacuerdo",
                                    "Ni de acuerdo\nni en desacuerdo",
                                    "Totalmente de acuerdo\no de acuerdo"))) %>% 
  count(ola, c10_03) %>% 
  drop_na() %>% 
  group_by(ola) %>% 
  mutate(freq = (n/sum(n))) %>% 
  as_label(ola) %>%  
  ggplot(aes(x = ola, 
             y = freq,
             fill = c10_03,
             label = as.character(scales::percent(freq, accuracy = .1)))) +
  geom_col(position = 'stack') + 
  geom_text(position = position_stack(vjust = .5),
            size = 3) +
  labs(title = get_label(elsoc_long$c10_03),
       x = "",
       y = "") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'top',
        legend.title = element_blank())

# Gráfico de barra (varios ítems) -----------------------------------------

elsoc_long %>% 
  ungroup() %>% 
  select(ola, starts_with("c10_")) %>% 
  pivot_longer(cols = starts_with("c10_")) %>%
  mutate(name = factor(name, labels = c("Votar es mi deber como ciudadano",
                                        "Mi voto influye en el resultado",
                                        "Votar permite expresar mis ideas")),
         value = factor(car::recode(value, "c(1,2) = 1; c(3) = 2; c(4,5) = 3"),
                        labels = c("Totalmente en desacuerdo\no en desacuerdo",
                                   "Ni de acuerdo\nni en desacuerdo",
                                   "Totalmente de acuerdo\no de acuerdo"))) %>%
  count(ola, name, value) %>% 
  group_by(ola, name) %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(value == "Totalmente de acuerdo\no de acuerdo") %>% 
  ggplot(aes(x = name, y = freq, fill = ola,
           label = scales::percent(freq, accuracy = .1))) + 
  geom_col(position = 'dodge2') + 
  geom_text(position = position_dodge(width  = .9),
            size = 2.5,
            vjust = -0.6) + 
  labs(title = 'Grado de acuerdo con las siguientes afirmaciones',
       subtitle = "Porcentaje de personas que responde 'de acuerdo o totalmente de acuerdo'",
       x = "",
       y = "") + 
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  theme(legend.position = 'top',
        legend.title = element_blank())

# Gráfico de lineas (varios ítems) -----------------------------------------

elsoc_long %>% 
  ungroup() %>% 
  select(ola, c05_02, c05_03, c05_07, c05_08) %>% 
  pivot_longer(cols = c(c05_02, c05_03, c05_07, c05_08)) %>% 
  mutate(name = factor(name, labels = c("Partidos políticos",
                                        "Carabineros",
                                        "Presidente",
                                        "Congreso nacional")),
         value = factor(car::recode(value, "c(1,2) = 1; c(3) = 2; c(4,5) = 3"),
                      labels = c("Nada o poca",
                                 "Algo",
                                 "Bastante o mucha"))) %>% 
  count(ola, name, value) %>% 
  drop_na() %>% 
  group_by(ola, name) %>% 
  mutate(freq = (n/sum(n))) %>% 
  filter(value %in% c("Nada o poca")) %>% 
  ggplot(aes(x = ola, y = freq, color = name, group = name,
             label = as.character(scales::percent(freq, accuracy = .1)))) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  geom_text(size = 3, color = 'black',
            vjust = -0.8) +
  labs(title = "Grado de confianza en instituciones",
       subtitle = "Porcentaje que responde 'Nada o poca' confianza",
       x = "",
       y = "") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  theme(legend.position = 'top',
        legend.title = element_blank())
  

# Gráfico alluvial --------------------------------------------------------

datos <- elsoc_long %>% 
  ungroup() %>% 
  count(idencuesta, ola, c12_01) %>% 
  drop_na() %>% 
  group_by(ola) %>% 
  mutate(freq = (n/sum(n))) %>% 
  as_label(c12_01)

etiquetas <- elsoc_long %>% 
  ungroup() %>% 
  count(ola, c12_01) %>% 
  drop_na() %>% 
  group_by(ola) %>% 
  mutate(freq = (n/sum(n)), 
         idencuesta = 1) %>% 
  as_label(c12_01)

ggplot(datos, aes(x = ola, 
                  y = freq,
                  fill = c12_01,
                  stratum = c12_01,
                  alluvium = idencuesta)) +
  ggalluvial::geom_flow(alpha = .7) + 
  ggalluvial::geom_stratum(linetype = 0) +
  geom_text(data = etiquetas,
            aes(label = scales::percent(freq, accuracy = .1)),
            position = position_stack(vjust = 0.5),
            size = 3,
            vjust = .2) +
  labs(title = get_label(elsoc_long$c12_01),
       x = NULL,
       y = NULL) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'top',
        legend.title = element_blank())


# Gráfico alluvial con facet --------------------------------------------

datos_2 <- elsoc_long %>% 
  ungroup() %>% 
  count(idencuesta, ola, c12_01, m0_sexo) %>% 
  drop_na() %>% 
  group_by(ola, m0_sexo) %>% 
  mutate(freq = (n/sum(n))) %>% 
  as_label(c12_01, m0_sexo)

etiquetas_2 <- elsoc_long %>% 
  ungroup() %>% 
  count(ola, c12_01, m0_sexo) %>% 
  drop_na() %>% 
  group_by(ola) %>% 
  mutate(freq = (n/sum(n)), 
         idencuesta = 1) %>% 
  as_label(c12_01, m0_sexo)

ggplot(datos_2, aes(x = ola, 
                  y = freq,
                  fill = c12_01,
                  stratum = c12_01,
                  alluvium = idencuesta)) +
  ggalluvial::geom_flow(alpha = .7) + 
  ggalluvial::geom_stratum(linetype = 0) +
  geom_text(data = etiquetas_2,
            aes(label = scales::percent(freq, accuracy = .1)),
            position = position_stack(vjust = .5),
            size = 3,
            vjust = -10) +
  labs(title = get_label(elsoc_long$c12_01),
       x = NULL,
       y = NULL) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  facet_wrap(.~m0_sexo) 

