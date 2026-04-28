########################################
###          Antonia Arce            ###
###               y                  ###
###       Florencia Pastenes A.      ###
###          Estadistca I            ###
###           Practico I             ###
########################################

# 1. CARGAR PAQUETES ---------------------------------------------------------------------------------
library(tidyverse)
library(haven)
library(dplyr)
library(car)
library(readxl)

# 2. CARGAR BASE DE DATOS ----------------------------------------------------------------------------
bbdd_pc6  <- read_xls("../input/instrumento n6_cp2_limpio.xlsx")


# 3. SELECCIONAR VARIABLES RELEVANTES -----------------------------------------------------------------
bbdd_pc_limpia <- bbdd_pc6 %>%
  select(
    Sexo,
    anios_cuidado   = CP_2_1,     # 2.1 Años cuidando (razón)
    horas_dia       = CP_2_2,     # 2.2 Horas diarias de cuidado (razón)
    ingresos_propios = CP_2_8,    # 2.8 Desarrolla alguna otra actividad constante fuera de la casa 0=No, 1=Sí (nominal)
    falta_tiempo    = `CP. 3.2`,   # 3.1 Falta de tiempo personal (ordinal)
    
    # Sección 3 completa : Variables para índice de sobrecarga 
    sobre_3_1 = `CP. 3.1`,
    sobre_3_2 = `CP. 3.2`,
    sobre_3_3 = `CP. 3.3`,
    sobre_3_4 = `CP. 3.4`,
    sobre_3_5 = `CP. 3.5`,
    sobre_3_6 = `CP. 3.6`,
    sobre_3_7 = `CP. 3.7`)

# 4. LIMPIEZA DE VARIABLES -------------------------------------------------------------------------------

bbdd_pc_limpia <- bbdd_pc_limpia %>%
  mutate(
    # Sexo: factor con etiquetas
    Sexo = factor(Sexo, levels = c(1, 2),
                  labels = c("Hombre", "Mujer")),
    
    # Años cuidando: '>1' -> 1, convertir a numérico
    anios_cuidado = as.numeric(case_when(
      anios_cuidado == ">1" ~ "1",
      TRUE ~ as.character(anios_cuidado)
    )),
    
    # Horas diarias
    horas_dia = as.numeric(horas_dia),
    
    # Actividad constante afuera del hogar
    ingresos_propios = factor(
      na_if(as.numeric(ingresos_propios), 99),
      levels = c(0, 1), labels = c('No', 'Si')),
    
    # Falta de tiempo: factor ordenado
    falta_tiempo = factor(falta_tiempo, levels = 1:5,
                          labels = c("Nunca","Rara vez","Algunas veces",
                                     "Bastantes veces","Casi siempre"),
                          ordered = TRUE))

#  5 . ETIQUETAS DE VARIABLES ------------------------------------------------------------------------------------
attr(bbdd_pc_limpia$anios_cuidado,    "label") <- "Años que lleva cuidando a la persona dependiente"
attr(bbdd_pc_limpia$horas_dia,        "label") <- "Horas diarias dedicadas al cuidado"
attr(bbdd_pc_limpia$ingresos_propios, "label") <- "¿Desarrolla alguna otra actividad constante fuera de la casa? (0=No, 1=Sí)"
attr(bbdd_pc_limpia$falta_tiempo,     "label") <- "¿Siente que no tiene tiempo para usted? (1=Nunca, 5=Casi siempre)"
attr(bbdd_pc_limpia$indice_sobrecarga,"label") <- "Índice de sobrecarga: suma ítems 3.1 a 3.7 (rango 17-35)"

# 6. Gráficos ------------------------------------------------------------------------------------------------------

## Gráfico 1: población en general

media_horas   <- mean(bbdd_pc_limpia$horas_dia, na.rm = TRUE)
mediana_horas <- median(bbdd_pc_limpia$horas_dia, na.rm = TRUE)

grafico1 <- ggplot(bbdd_pc_limpia %>% filter(!is.na(horas_dia)),
                   aes(x = factor(horas_dia))) +
  geom_bar(fill = "#7F77DD", color = "white", alpha = 0.9) +
  geom_vline(xintercept = which(levels(factor(bbdd_pc_limpia$horas_dia)) == as.character(media_horas)),
             linetype = "dashed", color = "#26215C", linewidth = 0.9) +
  geom_vline(xintercept = which(levels(factor(bbdd_pc_limpia$horas_dia)) == as.character(mediana_horas)),
             linetype = "dotted", color = "#AFA9EC", linewidth = 0.9) +
  annotate("text",
           x = which(levels(factor(bbdd_pc_limpia$horas_dia)) == as.character(round(media_horas))) + 0.3,
           y = 18,
           label = paste0("Media = ", round(media_horas, 1), " hrs"),
           color = "#26215C", size = 3.5, hjust = 0) +
  annotate("text",
           x = which(levels(factor(bbdd_pc_limpia$horas_dia)) == as.character(mediana_horas)) + 0.3,
           y = 16,
           label = paste0("Mediana = ", mediana_horas, " hrs"),
           color = "#3C3489", size = 3.5, hjust = 0) +
  labs(
    title    = "Gráfico 1. Distribución de horas diarias dedicadas al cuidado",
    subtitle = "Cuidadoras/es del PRLAC, comuna de Independencia",
    x        = "Horas diarias",
    y        = "Frecuencia",
    caption  = "Fuente: Instrumento PC N°06, PRLAC"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey50", size = 10),
    legend.position = "bottom")
grafico1

## Gráfico 2 : cruce de variable sexo y falta de tiempo para el autocuidado

g2 <- bbdd_pc_limpia %>%
  filter(!is.na(Sexo), !is.na(falta_tiempo)) %>%
  count(Sexo, falta_tiempo) %>%
  group_by(Sexo) %>%
  mutate(pct = n / sum(n) * 100)

paleta_likert <- c(
  "Nunca"           = "#EEEDFE",
  "Rara vez"        = "#AFA9EC",
  "Algunas veces"   = "#7F77DD",
  "Bastantes veces" = "#534AB7",
  "Casi siempre"    = "#3C3489")

grafico2 <- ggplot(g2,
                   aes(x = Sexo, y = pct, fill = falta_tiempo)) +
  geom_bar(stat = "identity", position = "stack", width = 0.55) +
  geom_text(aes(label = ifelse(pct >= 6, paste0(round(pct, 0), "%"), "")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_manual(values = paleta_likert, name = "Frecuencia") +
  labs(
    title    = "Gráfico 2. Distribución de horas diarias dedicadas al cuidado por sexo",
    subtitle = "Cuidadoras/es del PRLAC, comuna de Independencia",
    x        = "Horas diarias",
    y        = "Frecuencia",
    caption  = "Fuente: Instrumento PC N°06, PRLAC") +
  theme_minimal() 
grafico2

## Media sobrecarga de personas cuidadoras
media_sobrecarga <- mean(bbdd_pc_limpia$indice_sobrecarga)

## Media sobrecarga de mujeres cuidadoras
media_sobrecarga_mujeres <- bbdd_pc_limpia %>% 
  filter(Sexo == "Mujer") %>%           
  summarise(media = mean(indice_sobrecarga, na.rm = TRUE)) 
print(media_sobrecarga_mujeres)

# Media de sobrecarga de hombres cuidadores
media_sobrecarga_mujeres <- bbdd_pc_limpia %>% 
  filter(Sexo == "Hombre") %>%           
  summarise(media = mean(indice_sobrecarga, na.rm = TRUE)) 
print(media_sobrecarga_mujeres)
