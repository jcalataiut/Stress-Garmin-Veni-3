source("Code.R")
library(naniar)  # Para visualización de NA
library(readxl)
library(dplyr)
library(fda)
library(ggplot2)
#############################
##### Limpieza de datos #####
#############################
numeric_vars <- sapply(datos, is.numeric)
hist_data <- datos[, numeric_vars]


for (var in names(hist_data)) {
  hist(hist_data[[var]], main = paste("Histograma de", var),
       xlab = var, col = "lightblue", border = "white")
}


# descartar los datos en los que el sujeto no uso el reloj ese dia
datos_limpios <- datos[datos$`Wearing Time (s)`>0,]

datos_limpios <- datos_limpios %>%
  mutate(
    `Body Battery (min)` = ifelse(`Body Battery (min)` > 100, NA, `Body Battery (min)`),
    `Body Battery (avg)` = ifelse(`Body Battery (avg)` > 100, NA, `Body Battery (avg)`),
    `Body Battery (max)` = ifelse(`Body Battery (max)` > 100, NA, `Body Battery (max)`),
    `Heart Rate (min)` = ifelse(`Heart Rate (min)` < 10 | `Heart Rate (min)` > 150, NA, `Heart Rate (min)`),
    `Heart Rate (avg)` = ifelse(`Heart Rate (avg)` < 15 | `Heart Rate (avg)` > 180, NA, `Heart Rate (avg)`),
    `Heart Rate (max)` = ifelse(`Heart Rate (max)` < 20 | `Heart Rate (max)` > 250, NA, `Heart Rate (max)`),
    `Resting Heart Rate (avg)` = ifelse(`Resting Heart Rate (avg)` < 10 | `Resting Heart Rate (avg)` > 150, NA, `Resting Heart Rate (avg)`)
    
  )

estres_inicio <- as.Date("2025-02-02")
estres_fin <- as.Date("2025-02-16")

datos_limpios <- datos_limpios %>%
  mutate(
    etapa = case_when(
      `Calendar Date (Local)` < estres_inicio ~ "antes",
      `Calendar Date (Local)` >= estres_inicio & `Calendar Date (Local)` <= estres_fin ~ "durante",
      `Calendar Date (Local)` > estres_fin ~ "despues"
    )
  )
datos_limpios$etapa <- factor(datos_limpios$etapa, levels = c("antes", "durante", "despues"))



## outliers
datos_limpios %>% 
  filter(`Respiration-Rate (max)`>200)

table(datos_limpios$`User Id`[datos_limpios$`Respiration-Rate (max)`>200],datos_limpios$etapa[datos_limpios$`Respiration-Rate (max)`>200])

table(datos_limpios$`User Id`,datos_limpios$etapa)


# analisi NA
na_por_variable <- datos_limpios %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NAs") %>%
  arrange(desc(NAs)) %>%
  mutate(Porcentaje = 100 * NAs / nrow(datos_limpios))

print(na_por_variable, n=68)


na_por_usuario <- datos_limpios %>%
  group_by(`User Id`) %>%
  summarise(across(where(is.numeric), ~sum(is.na(.)))) %>%
  mutate(Total_NA = rowSums(across(where(is.numeric))))

print(na_por_usuario)  # Top 10 sujetos con más NA


## Buscar outliers de dias, dia de mucho estres acompañado de otros marcaderes extremos




#########
## FDA ##
#########
datos_limpios$`Calendar Date (Local)` <- as.Date(datos_limpios$`Calendar Date (Local)`)

# Elegimos una variable: Heart Rate (avg)
df_hr <- datos_limpios %>%
  dplyr::select(`User Id`, `Calendar Date (Local)`, `Heart Rate (avg)`) %>%
  rename(user = `User Id`, date = `Calendar Date (Local)`, hr = `Heart Rate (avg)`)

# Completar series de tiempo: una fila por día por usuario
library(tidyr)
df_hr <- df_hr %>%
  group_by(user) %>%
  tidyr::complete(date = seq(min(date), max(date), by = "day"))

# Convertir a matriz para análisis funcional
df_mat <- df_hr %>%
  pivot_wider(names_from = user, values_from = hr) %>%
  arrange(date)

# Extraer matriz de datos funcionales
mat_values <- as.matrix(df_mat[,-1])
days <- as.numeric(df_mat$date - min(df_mat$date))

# Crear base de funciones (por ejemplo, splines)
rangeval <- c(min(days), max(days))
basis <- create.bspline.basis(rangeval = rangeval, nbasis = 15)
fd_obj <- Data2fd(argvals = days, y = mat_values, basisobj = basis)

plot(fd_obj, col = rainbow(ncol(mat_values)), xlab = "Días", ylab = "Heart Rate (avg)", main = "Curvas HR por individuo")




