# ================================
# 1. CARGA DE PAQUETES Y DATOS
# ================================
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, naniar, ggplot2, reshape2, janitor,
               lme4, lmerTest, nlme)

datos <- read_csv("datos_practica_final.csv")

# Formatear fecha
datos$`Calendar Date (Local)` <- as.Date(datos$`Calendar Date (Local)`)

# Limpieza de nombres
datos <- datos %>% clean_names()

# ================================
# 2. SELECCIÓN DE VARIABLES Y FECHAS
# ================================
# Variables clave
col_usuario <- "user_id"
col_fecha <- "calendar_date_local"
variables_fisio <- names(datos)[29:56]

# Crear índice temporal por usuario
datos <- datos %>%
  group_by(user_id) %>%
  arrange(calendar_date_local, .by_group = TRUE) %>%
  mutate(tiempo = row_number()) %>%
  ungroup()

# Filtrar desde ~10 nov 2024 (inicio de registros consistentes)
datos_filtrados <- datos %>%
  filter(calendar_date_local >= as.Date("2024-11-10"))

# ================================
# 3. DEFINIR FASES DEL ESTUDIO
# ================================
antes <- datos_filtrados %>%
  filter(calendar_date_local >= as.Date("2024-12-05") &
           calendar_date_local <  as.Date("2025-02-02"))

durante <- datos_filtrados %>%
  filter(calendar_date_local >= as.Date("2025-02-02") &
           calendar_date_local <= as.Date("2025-02-15"))

despues <- datos_filtrados %>%
  filter(calendar_date_local >  as.Date("2025-02-15") &
           calendar_date_local <= as.Date("2025-03-18"))

# Unir fases
df_all <- bind_rows(
  antes   %>% mutate(fase = "antes"),
  durante %>% mutate(fase = "durante"),
  despues %>% mutate(fase = "despues")
) %>%
  mutate(
    fase = factor(fase, levels = c("antes", "durante", "despues")),
    id = factor(user_id)
  )

periodos <- cut(
  df_all$calendar_date_local,
  breaks = as.Date(c("2024-01-01", "2025-01-18", "2025-02-01", "2025-02-15", "2025-03-01", "2026-03-31")),
  labels = c("antes", "2_previas", "durante", "2_posteriores", "despues"),
  right = FALSE
)
df_all$periodo <- periodos

# Variables de interés para análisis
var_interes <- c(
  "body_battery_avg", "body_battery_min", "bbi_avg", "dev_50ms",
  "heart_rate_avg", "heart_rate_min", "high_stress_s",
  "rmssd", "sdnn", "rest_stress_s", "stress_avg"
)

# ================================
# 4. ANÁLISIS DE NAs
# ================================

# NA por individuo
na_por_individuo <- df_all %>%
  select(id, all_of(var_interes)) %>%
  pivot_longer(-id, names_to = "variable", values_to = "valor") %>%
  group_by(id, variable) %>%
  summarise(n_na = sum(is.na(valor)), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = n_na)

print(na_por_individuo, n = 200)
names(df_all)
# NA por fase e individuo
na_por_id_fase <- df_all %>%dplyr::select(id, fase, all_of(var_interes)) %>%
  pivot_longer(-c(id, fase), names_to = "variable", values_to = "valor") %>%
  group_by(id, fase, variable) %>%
  summarise(n_na = sum(is.na(valor)), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = n_na) %>%
  arrange(id, fase)

print(na_por_id_fase, n = 200)

# ================================
# 5. VISUALIZACIÓN DE NAs (Mapa de calor)
# ================================

na_matrix <- df_all %>%
  dplyr::select(user_id, all_of(variables_fisio)) %>%
  group_by(user_id) %>%
  summarise_all(~ mean(is.na(.))) %>%
  pivot_longer(-user_id, names_to = "Variable", values_to = "Porcentaje_NA")

ggplot(na_matrix, aes(x = Variable, y = user_id, fill = Porcentaje_NA)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Mapa de calor de porcentajes de NAs",
       x = "Variable Fisiológica", y = "Individuo")

# ================================
# 6. DETECCIÓN DE OUTLIERS (±3 SD)
# ================================
outlier_summary <- df_all %>%
  dplyr::select(all_of(var_interes)) %>%
  summarise_all(~ sum(. > mean(., na.rm = TRUE) + 3 * sd(., na.rm = TRUE) |
                        . < mean(., na.rm = TRUE) - 3 * sd(., na.rm = TRUE),
                      na.rm = TRUE))

print("Conteo de outliers por variable:")
print(outlier_summary)

# ================================
# 7. BOXPLOTS DE SUEÑO
# ================================
datos_sueño <- datos_filtrados %>%
  dplyr::select(user_id, calendar_date_local,
         light_sleep_duration_s, deep_sleep_duration_s, rem_sleep_duration_s) %>%
  pivot_longer(cols = -c(user_id, calendar_date_local),
               names_to = "fase_sueno", values_to = "duracion")

ggplot(datos_sueño, aes(x = fase_sueno, y = duracion / 3600)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Boxplots de duración del sueño (horas)",
       y = "Duración (horas)", x = "Fase del sueño") +
  theme_minimal()



##################################
######## ENFOQUE A) ##############
##################################

# Seleccionar IDs con datos completos
ids_a_filtrar <- c("id_001", "id_005", "id_007", "id_009")

df_all_filtr_id <- df_all %>%
  filter(id %in% ids_a_filtrar) %>%
  droplevels()
# ================================
# 3. FUNCIÓN GENERAL PARA ANALIZAR UNA VARIABLE
# ================================

# 3 etapas

library(fda)
library(fda.usc)

analisis_funcional_variable <- function(df, variable) {
  cat("\n\n==== Análisis funcional de:", variable, "====\n")
  
  fases <- levels(df$fase)
  par(mfrow = c(1, length(fases)))
  
  for (fase_i in fases) {
    df_fase <- df %>%
      filter(fase == fase_i) %>%
      dplyr::select(id, calendar_date_local, all_of(variable))
    
    df_wide <- df_fase %>%
      pivot_wider(names_from = calendar_date_local, values_from = all_of(variable)) %>%
      column_to_rownames("id")
    
    mat <- as.matrix(df_wide)
    mat <- mat[rowSums(is.na(mat)) <= 3, ]
    if (nrow(mat) < 2) {
      cat("No hay suficientes datos en fase", fase_i, "\n")
      next
    }
    
    for (j in 1:ncol(mat)) {
      if (any(is.na(mat[, j]))) {
        mat[is.na(mat[, j]), j] <- mean(mat[, j], na.rm = TRUE)
      }
    }
    
    fdatos <- fdata(mat)
    
    # Mostrar las curvas funcionales
    plot(fdatos,
         main = paste("Curvas funcionales -", fase_i),
         xlab = "Día relativo", ylab = variable)
    
    # Convertir a fd y graficar la media
    fdobj <- fdata2fd(fdatos)
    lines(mean.fd(fdobj), col = "blue", lwd = 3)
  }
  
  par(mfrow = c(1, 1))
}


# ================================
# 4. APLICAR A VARIABLES DE INTERÉS
# ================================
var_interes <- c(
  "body_battery_avg", "heart_rate_avg", "rmssd", "sdnn", "stress_avg"
)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, janitor, fda.usc)
# Ejecutar análisis para cada variable
for (var in var_interes) {
  analisis_funcional_variable(df_all_filtr_id, var)
}



# 5 etapas


library(fda)
library(fda.usc)

analisis_funcional_variable <- function(df, variable) {
  cat("\n\n==== Análisis funcional de:", variable, "====\n")
  
  periodos <- levels(df$periodo)
  par(mfrow = c(1, length(periodos)))
  
  for (periodo_i in periodos) {
    df_periodo <- df %>%
      filter(periodo == periodo_i) %>%
      dplyr::select(id, calendar_date_local, all_of(variable))
    
    df_wide <- df_periodo %>%
      pivot_wider(names_from = calendar_date_local, values_from = all_of(variable)) %>%
      column_to_rownames("id")
    
    mat <- as.matrix(df_wide)
    mat <- mat[rowSums(is.na(mat)) <= 3, ]
    if (nrow(mat) < 2) {
      cat("No hay suficientes datos en periodo", periodo_i, "\n")
      next
    }
    
    for (j in 1:ncol(mat)) {
      if (any(is.na(mat[, j]))) {
        mat[is.na(mat[, j]), j] <- mean(mat[, j], na.rm = TRUE)
      }
    }
    
    fdatos <- fdata(mat)
    
    # Mostrar las curvas funcionales
    global_vals <- df %>% pull(all_of(variable))
    global_min <- min(global_vals, na.rm = TRUE)
    global_max <- max(global_vals, na.rm = TRUE)
    
    plot(fdatos,
         main = paste("Curvas funcionales -", periodo_i),
         xlab = "Día relativo", ylab = variable,
         ylim = c(global_min, global_max))
    
    # Convertir a fd y graficar la media
    fdobj <- fdata2fd(fdatos)
    lines(mean.fd(fdobj), col = "blue", lwd = 3)
  }
  
  par(mfrow = c(1, 1))
}


# ================================
# 4. APLICAR A VARIABLES DE INTERÉS
# ================================
var_interes <- c(
  "body_battery_avg", "heart_rate_avg", "rmssd", "sdnn", "stress_avg"
)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, janitor, fda.usc)
# Ejecutar análisis para cada variable
for (var in var_interes) {
  analisis_funcional_variable(df_all_filtr_id, var)
}

