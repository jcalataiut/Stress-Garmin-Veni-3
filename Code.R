library(dplyr)
library(tidyr)

rm(list=ls())
pacman::p_load(tidyverse)
datos <- read_csv("datos_practica_final.csv")


datos<-datos%>%
  mutate(across(where(is.character),as.factor))%>%
  mutate(across(where(is.logical),as.factor))

summary(datos)

# Convertir la columna a Date si no lo es aún
datos$`Calendar Date (Local)` <- as.Date(datos$`Calendar Date (Local)`)

# Filtrar por un rango de fechas del enunciado
critic <- datos[datos$`Calendar Date (Local)` >= as.Date("2025-02-02") &
                  datos$`Calendar Date (Local)` <= as.Date("2025-02-15"), ]

######################################
## Encontrar periodo  coincidentes ###
######################################

db_var<-datos%>%
  select(1:10,29:56)
summary(db_var)


# Encontrar la cantidad de NA's por fechas segun la variable que nos interesa

db_var$`Stress Qualifier`

fechas_NA<-db_var%>%
  select(`User Id`,`Calendar Date (Local)`,`Stress (avg)`,`Stress Qualifier`)%>%
  arrange(`Calendar Date (Local)`,`User Id`) %>%
  group_by(`Calendar Date (Local)`) %>%
  summarise(Recuento_NA_Stress = sum(is.na(`Stress (avg)`)),
            Recuenta_Qualifier=sum(is.na(`Stress Qualifier`)))

print(fechas_NA,n=200)
# 2024-11-07 cambio drastico en NA's, varia entre 2 y 3 y luego entre 2 e 1
# 2024-12-05 1 NA 

before_v1<-db_var[db_var$`Calendar Date (Local)` >= as.Date("2024-12-05") &
              db_var$`Calendar Date (Local)` <= as.Date("2025-02-02"), ]

# 2024-11-19  2 NA's
before_v2<-db_var[db_var$`Calendar Date (Local)` >= as.Date("2024-11-19") &
              db_var$`Calendar Date (Local)` <= as.Date("2025-02-02"), ]



# 2025-03-18  empieza a haber muchos NA's otra vez. 
# Considerar fecha para despues  periodo de estress.
after<-db_var[db_var$`Calendar Date (Local)` >= as.Date("2025-02-15") &
                db_var$`Calendar Date (Local)` <= as.Date("2025-03-18"),]


## Cuantos NA's hay por individuo, decidir si eliminar individuos

after%>%
  group_by(`User Id`)%>%
  summarise(Recuento_NA_Stress = sum(is.na(`Stress (avg)`)))

critic%>%
  group_by(`User Id`)%>%
  summarise(Recuento_NA_Stress = sum(is.na(`Stress (avg)`)))

before_v1%>%
  group_by(`User Id`)%>%
  summarise(Recuento_NA_Stress = sum(is.na(`Stress (avg)`)))

before_v2%>%
  group_by(`User Id`)%>%
  summarise(Recuento_NA_Stress = sum(is.na(`Stress (avg)`)))


aux<-db_var[db_var$`Calendar Date (Local)` >= as.Date("2024-11-19") &
              db_var$`Calendar Date (Local)` <= as.Date("2025-03-18"), ]

## Cantidad de dias en todo el periodo
longitud_periodo_total<-length(unique(aux$`Calendar Date (Local)`))




# Casos anomalos y irreales
# Definir rangos como lista nombrada
rangos <- list(
  "Heart Rate (min)" = list(normal = c(30, 60), anomalo = c(20, 100)),
  "Heart Rate (avg)" = list(normal = c(40, 110), anomalo = c(30, 130)),
  "Heart Rate (max)" = list(normal = c(100, 190), anomalo = c(80, 210)),
  "Stress (avg)" = list(normal = c(0, 60), anomalo = c(0, 90)),
  "Stress (max)" = list(normal = c(0, 80), anomalo = c(80, 100)),
  "Total Steps" = list(normal = c(2000, 25000), anomalo = c(0, 50000)),
  "Skin Temperature (min)" = list(normal = c(31, 36), anomalo = c(27, 37)),
  "Skin Temperature (max)" = list(normal = c(33, 38), anomalo = c(31, 41)),
  "Skin Temperature (avg)" = list(normal = c(32, 37), anomalo = c(30, 39)),
  "Pulse Ox (min)"= list(normal = c(93, 100), anomalo = c(88, 100)),
  "Pulse Ox (max)"= list(normal = c(95, 100), anomalo = c(90, 100)),
  "Pulse Ox (avg)" = list(normal = c(96, 100), anomalo = c(91, 100)),
  "Respiration-Rate (min)" = list(normal = c(8, 14), anomalo = c(6, 18)),
  "Respiration-Rate (avg)" = list(normal = c(10-20), anomalo = c(8, 25)),
  "Respiration-Rate (max)" = list(normal = c(20, 35), anomalo = c(15, 50))
  
)

resultados <- list()

for (var in names(rangos)) {
  if (var %in% names(critic)) {
    r <- rangos[[var]]
    valores <- critic[[var]]
    
    irreales_idx <- which(valores < r$anomalo[1] | valores > r$anomalo[2])
    anomalo_idx <- which((valores < r$normal[1] | valores > r$normal[2]) & !(valores < r$anomalo[1] | valores > r$anomalo[2]))
    
    resultados[[var]] <- list(
      n_irreales = length(irreales_idx),
      n_anomalo = length(anomalo_idx),
      irreales_casos = critic[irreales_idx, c("User Id", "Calendar Date (Local)", var)],
      anomalo_casos = critic[anomalo_idx, c("User Id", "Calendar Date (Local)", var)]
    )
  }
}

# Ejemplo: mostrar resumen para "Heart Rate (min)"
res <- resultados[["Heart Rate (min)"]]
print(paste("Valores irreales:", res$n_irreales))
print(paste("Valores anómalos:", res$n_anomalo))
print("Casos irreales:")
print(res$irreales_casos)
print("Casos anómalos:")
print(res$anomalo_casos)

# Ejemplo: mostrar resumen para "stress"  # para el maxi hay mucho valor anomalo
res <- resultados[["Stress (max)"]]
print(paste("Valores irreales:", res$n_irreales))
print(paste("Valores anómalos:", res$n_anomalo))
print("Casos irreales:")
print(res$irreales_casos)
print("Casos anómalos:")
print(res$anomalo_casos)

# Ejemplo: mostrar resumen para "ox" # nada fuera de rango
res <- resultados[["Pulse Ox (max)"]]
print(paste("Valores irreales:", res$n_irreales))
print(paste("Valores anómalos:", res$n_anomalo))
print("Casos irreales:")
print(res$irreales_casos)
print("Casos anómalos:")
print(res$anomalo_casos)

# Ejemplo: mostrar resumen para "temp" # nada fuera de rango # todo normal
res <- resultados[["Skin Temperature (avg)"]]
print(paste("Valores irreales:", res$n_irreales))
print(paste("Valores anómalos:", res$n_anomalo))
print("Casos irreales:")
print(res$irreales_casos)
print("Casos anómalos:")
print(res$anomalo_casos)


# Ejemplo: mostrar resumen para "ox" # mucho valor anomalo
res <- resultados[["Respiration-Rate (max)"]]
print(paste("Valores irreales:", res$n_irreales))
print(paste("Valores anómalos:", res$n_anomalo))
print("Casos irreales:")
print(res$irreales_casos)
print("Casos anómalos:")
print(res$anomalo_casos)



## casos por variable y por sujeto
rangos <- list(
  "Heart Rate (min)"       = c(normal = 30, anomalo_max = 100),
  "Heart Rate (avg)"       = c(normal = 40, anomalo_max = 130),
  "Heart Rate (max)"       = c(normal = 100, anomalo_max = 210),
  "Stress (avg)"           = c(normal = 0, anomalo_max = 90),
  "Stress (max)"           = c(normal = 0, anomalo_max = 100),
  "Total Steps"            = c(normal = 2000, anomalo_max = 50000),
  "Respiration-Rate (min)" = c(normal = 8, anomalo_max = 18),
  "Respiration-Rate (avg)" = c(normal = 10, anomalo_max = 25),
  "Respiration-Rate (max)" = c(normal = 20, anomalo_max = 50),
  "Skin Temperature (min)" = c(normal = 31, anomalo_max = 37),
  "Skin Temperature (avg)" = c(normal = 32, anomalo_max = 39),
  "Skin Temperature (max)" = c(normal = 33, anomalo_max = 40),
  "Pulse Ox (min)"         = c(normal = 93, anomalo_max = 100),
  "Pulse Ox (avg)"         = c(normal = 95, anomalo_max = 100),
  "Pulse Ox (max)"         = c(normal = 96, anomalo_max = 100)
)


clasificar_valor <- function(valor, var) {
  if (is.na(valor)) return(NA)
  lims <- rangos[[var]]
  if (valor < 0 || valor > 250) return("irreal")  # regla general de descarte
  if (valor < lims["normal"] || valor > lims["anomalo_max"]) return("anómalo")
  return("normal")
}
df <- critic

for (var in names(rangos)) {
  df[[paste0(var, "_estado")]] <- sapply(df[[var]], clasificar_valor, var = var)
}

# Reunir datos para análisis
df_long <- df %>%
  pivot_longer(cols = ends_with("_estado"), names_to = "Variable", values_to = "Estado") %>%
  mutate(Variable = gsub("_estado", "", Variable))

# Conteo de anomalías por fecha
anomalias_por_dia <- df_long %>%
  filter(Estado == "anómalo") %>%
  group_by(`Calendar Date (Local)`) %>%
  summarise(n_anomalías = n()) %>%
  arrange(desc(n_anomalías))

# Conteo de anomalías por sujeto y variable
anomalias_por_usuario <- df_long %>%
  filter(Estado == "anómalo") %>%
  group_by(`User Id`, Variable) %>%
  summarise(n_anomalías = n()) %>%
  arrange(desc(n_anomalías))

# Mostrar resultados
print("Días con más anomalías:")
print(head(anomalias_por_dia, 10))

print("Anomalías por sujeto y variable:")
print(head(anomalias_por_usuario, 10))


# vemos que el sujeto dos es el unico que no tiene valores anomalos