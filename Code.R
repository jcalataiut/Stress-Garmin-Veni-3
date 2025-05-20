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
