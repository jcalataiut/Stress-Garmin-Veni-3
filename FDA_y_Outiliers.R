# Cargar librerías necesarias
library(naniar)     # Visualización de datos faltantes (NA)
library(readxl)     # Lectura de archivos Excel
library(dplyr)      # Manipulación de datos
library(fda)        # Análisis de Datos Funcionales (FDA)
library(ggplot2)    # Visualización
library(tidyr)      # Completar series de tiempo, pivoteo
library(qtl2)
# Cargar datos
# ATENCIÓN: El script asume que el objeto 'datos' ya está cargado.
# Asegúrate de haber cargado 'datos_practica_final.csv' antes de ejecutar el script.
datos <- read_csv("datos_practica_final.csv")

#############################
##### Limpieza de datos #####
#############################

# Filtrar variables numéricas para análisis exploratorio
numeric_vars <- sapply(datos, is.numeric)
hist_data <- datos[, numeric_vars]

# Generar histogramas para todas las variables numéricas
for (var in names(hist_data)) {
  hist(hist_data[[var]], main = paste("Histograma de", var),
       xlab = var, col = "lightblue", border = "white")
}

# Eliminar filas donde el sujeto no usó el reloj (Wearing Time <= 0)
datos_limpios <- datos[datos$`Wearing Time (s)` > 0, ]

# Reemplazar valores atípicos por NA
datos_limpios <- datos_limpios %>%
  mutate(
    across(
      c(`Body Battery (min)`, `Body Battery (avg)`, `Body Battery (max)`),
      ~ ifelse(. > 100, NA, .)
    ),
    `Heart Rate (min)` = ifelse(`Heart Rate (min)` < 10 | `Heart Rate (min)` > 150, NA, `Heart Rate (min)`),
    `Heart Rate (avg)` = ifelse(`Heart Rate (avg)` < 15 | `Heart Rate (avg)` > 180, NA, `Heart Rate (avg)`),
    `Heart Rate (max)` = ifelse(`Heart Rate (max)` < 20 | `Heart Rate (max)` > 250, NA, `Heart Rate (max)`),
    `Resting Heart Rate (avg)` = ifelse(`Resting Heart Rate (avg)` < 10 | `Resting Heart Rate (avg)` > 150, NA, `Resting Heart Rate (avg)`)
  )


# Asegúrate de tener cargadas las librerías necesarias
library(dplyr)
library(lubridate) # ¡Esta es la clave para usar days()!

# Definir las fechas de inicio y fin del período de estrés
estres_inicio <- as.Date("2025-02-02")
estres_fin <- as.Date("2025-02-16")

# Calcular las fechas para las nuevas etapas usando lubridate::days()
pre_estres_inicio <- estres_inicio - days(14) # Dos semanas antes del inicio del estrés
post_estres_fin <- estres_fin + days(14)     # Dos semanas después del fin del estrés

# Clasificar los datos en las nuevas etapas temporale

datos_limpios$etapa <- cut(
  datos_limpios$`Calendar Date (Local)`,
  breaks = as.Date(c("2024-01-01", "2025-02-01", "2025-02-15", "2026-03-31")),
  labels = c("antes", "durante", "despues"),
  right = FALSE
)

#########################
##### Outliers check ####
#########################

# Valores extremos de frecuencia respiratoria
outliers_resp <- datos_limpios %>% filter(`Respiration-Rate (max)` > 200)
print(outliers_resp)
tabla_outliers <- outliers_resp %>%
  group_by(`User Id`, etapa) %>%
  summarise(dias_outlier = n(), .groups = "drop")

# Contar días totales con medición por usuario y etapa
tabla_total <- datos_limpios %>%
  group_by(`User Id`, etapa) %>%
  summarise(dias_totales = n(), .groups = "drop")

# Unir ambas tablas y calcular porcentaje
tabla_porcentual <- left_join(tabla_total, tabla_outliers,
                              by = c("User Id", "etapa")) %>%
  mutate(dias_outlier = ifelse(is.na(dias_outlier), 0, dias_outlier),
         porcentaje = round(100 * dias_outlier / dias_totales, 2)) %>%
  arrange(desc(porcentaje))

# Mostrar tabla
print(tabla_porcentual)

##########################################
##### Análisis de valores faltantes #####
##########################################

# Por variable
na_por_variable <- datos_limpios %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NAs") %>%
  arrange(desc(NAs)) %>%
  mutate(Porcentaje = 100 * NAs / nrow(datos_limpios))

print(na_por_variable, n = nrow(na_por_variable))

# Por usuario
na_por_usuario <- datos_limpios %>%
  group_by(`User Id`) %>%
  summarise(across(where(is.numeric), ~ sum(is.na(.)))) %>%
  mutate(Total_NA = rowSums(across(where(is.numeric))))

print(head(na_por_usuario[order(-na_por_usuario$Total_NA), ], 10))



########################
##### Análisis FDA #####
########################


periodos <- cut(
  datos_limpios$`Calendar Date (Local)`,
  breaks = as.Date(c("2025-01-01", "2025-01-18", "2025-02-01", "2025-02-15", "2025-03-01", "2025-03-31")),
  labels = c("antes", "2_previas", "durante", "2_posteriores", "despues"),
  right = FALSE
)
datos_limpios$periodo <- periodos

# Ejemplo para una variable y un individuo:
tiempo <- as.numeric(datos_limpios$`Calendar Date (Local)`) # Día como variable numérica
valor <- datos_limpios$`Heart Rate (avg)`[datos_limpios$`User Id` == 1]

# Crea base de splines
basis <- create.bspline.basis(rangeval = range(tiempo), nbasis = 10)
curva <- smooth.basis(tiempo, valor, basis)$fd
plot(curva)


ggplot(datos_limpios, aes(x = `Calendar Date (Local)`, y = `Heart Rate (avg)`, color = factor(`User Id`))) +
  geom_line() 

library(dplyr)
datos_media <- datos_limpios %>%
  group_by(periodo, `Calendar Date (Local)`) %>%
  summarise(media = mean(`Heart Rate (avg)`, na.rm = TRUE))

ggplot(datos_media, aes(x = `Calendar Date (Local)`, y = media, color = periodo)) +
  geom_line(size = 1.2)


# curbas suavizadas
library(purrr)

# Prepara una función para suavizar cada sujeto
suaviza_funcion <- function(x, y) {
  basis <- create.bspline.basis(rangeval = range(x, na.rm = TRUE), nbasis = 8)
  smooth.basis(x, y, basis)$fd
}

# Para cada individuo y periodo
curvas <- datos_limpios %>%
  group_by(`User Id`, periodo) %>%
  filter(n() > 8) %>%
  nest() %>%
  mutate(
    fd_hr_avg = map(data, ~{
      suaviza_funcion(as.numeric(.x$`Calendar Date (Local)`), .x$`Heart Rate (avg)`)
    })
  )

# Calcular la media por periodo para heart_rate_avg
media_por_periodo <- datos_limpios %>%
  group_by(periodo, `Calendar Date (Local)`) %>%
  summarise(`Heart Rate (avg)` = mean(`Heart Rate (avg)`, na.rm = TRUE), .groups = 'drop')

ggplot(media_por_periodo, aes(x = `Calendar Date (Local)`, y = `Heart Rate (avg)`, color = periodo)) +
  geom_line(size = 1.2) +
  labs(title = "Curva media de Heart Rate (avg) por periodo")

library(fda.usc)
# Suponiendo que tienes una lista de objetos fd (uno por periodo)
anova.onefactor(fdlist, group = periodo_vector)

## bbi
ggplot(datos_limpios, aes(x = `Calendar Date (Local)`, y = `BBI (avg)`, color = factor(`User Id`))) +
  geom_line() +
  labs(title = "Evolución de BBI (avg) por individuo", y = "BBI (avg)")






# boxplots 

# heart rate
library(ggpubr)
library(rstatix)

ggbox_plot_stats <- function(dt = datos_limpios, subtitul = "Heart Rate", titul = "Diferencias entre etapas") {
  
  # Selecciona las variables requeridas
  dt_temp <- dt %>% 
    dplyr::select(`Heart Rate (avg)`, `Heart Rate (max)`, `Heart Rate (min)`, grupo = etapa)
  
  # Datos en formato largo
  df_long <- dt_temp %>%
    tidyr::pivot_longer(cols = starts_with("Heart Rate"), names_to = "Variable", values_to = "Valor")
  
  # Prueba t para cada variable
  stat.test <- df_long %>%
    group_by(Variable) %>%
    t_test(Valor ~ grupo) %>%
    adjust_pvalue() %>%
    add_significance("p") %>%
    add_xy_position(x = "grupo")
  
  # Boxplot con anotaciones de p-valor
  p <-  ggpubr::ggboxplot(
    df_long, 
    x = "grupo", 
    y = "Valor", 
    fill = "grupo", 
    facet.by = "Variable", 
    outlier.shape = NA
  ) +
    stat_pvalue_manual(stat.test, label = "p.signif", tip.length = 0.01) + 
    labs(
      title = titul,
      x = "Periodo",
      y = "Heart Rate",
      fill = "Periodo",
      subtitle = subtitul,
      caption = "* p < 0.05, ** p < 0.01, *** p < 0.001, ns: No significativo"
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  print(p)
}

# Ejemplo de ejecución:
ggbox_plot_stats(dt = datos_limpios, subtitul = "", titul = "Heart Rate (avg, max, min) por etapas")



