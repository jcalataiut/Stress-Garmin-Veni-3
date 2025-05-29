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
datos <- datos %>% clean_names()

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

# resperation rate con muchos valores elevados
#latidos por minuto por debajo de 10 descartados
# valores de sd1_sd2 bajos?



# Eliminar filas donde el sujeto no usó el reloj (Wearing Time <= 0)
datos_limpios <- datos[datos$wearing_time_s > 0, ]

# Reemplazar valores atípicos por NA
datos_limpios <- datos_limpios %>%
  mutate(
    across(
      c(body_battery_min, body_battery_avg, body_battery_max),
      ~ ifelse(. > 100, NA, .)
    ),
    heart_rate_min = ifelse(heart_rate_min < 10 | heart_rate_min > 150, NA, heart_rate_min),
    heart_rate_avg = ifelse(heart_rate_avg < 15 | heart_rate_avg > 180, NA, heart_rate_avg),
    heart_rate_max = ifelse(heart_rate_max < 20 | heart_rate_max > 250, NA, heart_rate_max),
    resting_heart_rate_avg = ifelse(resting_heart_rate_avg < 10 | resting_heart_rate_avg > 150, NA, resting_heart_rate_avg)
  )

numeric_vars <- sapply(datos_limpios, is.numeric)
hist_data <- datos_limpios[, numeric_vars]
pairs(hist_data[,c(8,9,15,19,20,22,23,29,32,33,34,37,41,40,45,52)])
# Asegúrate de tener cargadas las librerías necesarias
library(dplyr)
library(lubridate) # ¡Esta es la clave para usar days()!

# 3 etapas
datos_limpios$etapa <- cut(
  datos_limpios$calendar_date_local,
  breaks = as.Date(c("2024-01-01", "2025-02-01", "2025-02-15", "2026-03-31")),
  labels = c("antes", "durante", "despues"),
  right = FALSE
)

# 5 etapas
periodos <- cut(
  datos_limpios$calendar_date_local,
  breaks = as.Date(c("2025-01-01", "2025-01-18", "2025-02-01", "2025-02-15", "2025-03-01", "2025-03-31")),
  labels = c("antes", "2_previas", "durante", "2_posteriores", "despues"),
  right = FALSE
)
datos_limpios$periodo <- periodos


#########################
##### Outliers check ####
#########################
variable <- datos_limpios$heart_rate_max
# Valores extremos de frecuencia respiratoria
outliers_resp <- datos_limpios %>% filter(variable > 200)
print(outliers_resp)
tabla_outliers <- outliers_resp %>%
  group_by(user_id, etapa) %>%
  summarise(dias_outlier = n(), .groups = "drop")

# Contar días totales con medición por usuario y etapa
tabla_total <- datos_limpios %>%
  group_by(user_id, etapa) %>%
  summarise(dias_totales = n(), .groups = "drop")

# Unir ambas tablas y calcular porcentaje
tabla_porcentual <- left_join(tabla_total, tabla_outliers,
                              by = c("user_id", "etapa")) %>%
  mutate(dias_outlier = ifelse(is.na(dias_outlier), 0, dias_outlier),
         porcentaje = round(100 * dias_outlier / dias_totales, 2)) 

# Mostrar tabla
print(tabla_porcentual)

library(dplyr)
library(ggplot2)
library(rlang)

boxplot_prop_outliers <- function(datos, variable, 
                                  id_col = "user_id", 
                                  etapa_col = "etapa",
                                  tipo_outlier = c("ambos", "superior", "inferior")) {
  tipo_outlier <- match.arg(tipo_outlier)
  var_quo <- enquo(variable)
  
  stats <- datos %>%
    summarise(
      Q1 = quantile(!!var_quo, 0.25, na.rm = TRUE),
      Q3 = quantile(!!var_quo, 0.75, na.rm = TRUE)
    )
  Q1 <- stats$Q1
  Q3 <- stats$Q3
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  
  # Marcar outliers según el tipo seleccionado
  datos <- datos %>%
    mutate(es_outlier = case_when(
      tipo_outlier == "ambos" ~ (!!var_quo < lim_inf | !!var_quo > lim_sup),
      tipo_outlier == "superior" ~ (!!var_quo > lim_sup),
      tipo_outlier == "inferior" ~ (!!var_quo < lim_inf)
    ))
  
  tabla_outliers <- datos %>%
    filter(es_outlier) %>%
    group_by(across(all_of(c(id_col, etapa_col)))) %>%
    summarise(dias_outlier = n(), .groups = "drop")
  
  tabla_total <- datos %>%
    group_by(across(all_of(c(id_col, etapa_col)))) %>%
    summarise(dias_totales = n(), .groups = "drop")
  
  tabla_porcentual <- left_join(tabla_total, tabla_outliers,
                                by = c(id_col, etapa_col)) %>%
    mutate(dias_outlier = ifelse(is.na(dias_outlier), 0, dias_outlier),
           porcentaje = round(100 * dias_outlier / dias_totales, 2))
  
  # Boxplot
  ggplot(tabla_porcentual, aes_string(x = etapa_col, y = "porcentaje")) +
    geom_boxplot(fill = "skyblue", outlier.colour = "red", outlier.shape = 8) +
    labs(title = paste0("Distribución % días outlier (", tipo_outlier, ") para '", 
                        deparse(substitute(variable)), "'"),
         x = etapa_col,
         y = "% días outlier") +
    theme_minimal()
}


# Ejemplo de uso:
# boxplot_prop_outliers(datos_limpios, respiration_rate_max)
# boxplot_prop_outliers(datos_limpios, heart_rate_max)
boxplot_prop_outliers(datos= datos_limpios,variable= respiration_rate_avg, tipo_outlier = "superior")

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
  group_by(user_id) %>%
  summarise(across(where(is.numeric), ~ sum(is.na(.)))) %>%
  mutate(Total_NA = rowSums(across(where(is.numeric))))

print(head(na_por_usuario[order(-na_por_usuario$Total_NA), ], 10))



########################
##### Análisis FDA #####
########################


periodos <- cut(
  datos_limpios$calendar_date_local,
  breaks = as.Date(c("2025-01-01", "2025-01-18", "2025-02-01", "2025-02-15", "2025-03-01", "2025-03-31")),
  labels = c("antes", "2_previas", "durante", "2_posteriores", "despues"),
  right = FALSE
)
datos_limpios$periodo <- periodos

# Ejemplo para una variable y un individuo:
tiempo <- as.numeric(datos_limpios$calendar_date_local) # Día como variable numérica
valor <- datos_limpios$heart_rate_avg[datos_limpios$user_id == 1]

# Crea base de splines
basis <- create.bspline.basis(rangeval = range(tiempo), nbasis = 10)
curva <- smooth.basis(tiempo, valor, basis)$fd
plot(curva)


ggplot(datos_limpios, aes(x = calendar_date_local, y = heart_rate_avg, color = factor(user_id))) +
  geom_line() 

library(dplyr)
datos_media <- datos_limpios %>%
  group_by(periodo, calendar_date_local) %>%
  summarise(media = mean(heart_rate_avg, na.rm = TRUE))

ggplot(datos_media, aes(x = calendar_date_local, y = media, color = periodo)) +
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
  group_by(user_id, periodo) %>%
  filter(n() > 8) %>%
  nest() %>%
  mutate(
    fd_hr_avg = map(data, ~{
      suaviza_funcion(as.numeric(.x$calendar_date_local), .x$heart_rate_avg)
    })
  )

# Calcular la media por periodo para heart_rate_avg
media_por_periodo <- datos_limpios %>%
  group_by(periodo, calendar_date_local) %>%
  summarise(heart_rate_avg = mean(heart_rate_avg, na.rm = TRUE), .groups = 'drop')

ggplot(media_por_periodo, aes(x = calendar_date_local, y = heart_rate_avg, color = periodo)) +
  geom_line(size = 1.2) +
  labs(title = "Curva media de Heart Rate (avg) por periodo")

library(fda.usc)
# Suponiendo que tienes una lista de objetos fd (uno por periodo)
anova.onefactor(fdlist, group = periodo_vector)

## bbi
ggplot(datos_limpios, aes(x = calendar_date_local, y = `BBI _avg, color = factor(user_id))) +
  geom_line() +
  labs(title = "Evolución de BBI (avg) por individuo", y = "BBI (avg)")






# boxplots 

# heart rate
library(ggpubr)
library(rstatix)

ggbox_plot_stats <- function(dt = datos_limpios, subtitul = "Heart Rate", titul = "Diferencias entre etapas") {
  
  # Selecciona las variables requeridas
  dt_temp <- dt %>% 
    dplyr::select(heart_rate_avg, heart_rate_max, heart_rate_min, grupo = etapa)
  
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



