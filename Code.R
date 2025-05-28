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



######
# Apartado d) Análisis univariado
######


# Definimos con que valores vamos a trabajar como periodos de antes, durante, despues
# de los data (seleccionamos los que creamos como apropiados aunque aquí solo 
# proponemos estos que no tienen en cuenta las anomalias -> cambiarlos por los que sí)

antes <- before_v1
durante <- critic
despues <- after




# 0) Preparar paquetes ---------------------------------------------------
# install.packages(c("dplyr","tidyr","car","FSA","rstatix"))
library(dplyr)
library(tidyr)
library(car)       # Levene’s test
library(rstatix)   # Shapiro por grupo, ANOVA, Kruskal, post-hoc
library(FSA)       # Dunn test

# 1) Unir data.frames y etiquetar fases ---------------------------------
# Suponemos que tus data.frames se llaman df_before, df_during, df_after
df_all <- bind_rows(
  antes  %>% mutate(fase = "antes"),
  durante  %>% mutate(fase = "durante"),
  despues   %>% mutate(fase = "despues")
)

# Convertir fase a factor con orden deseado
df_all <- df_all %>%
  mutate(
    fase = factor(fase, levels = c("antes","durante","despues")),
    id   = factor(`User Id`)
  )

# 2) Pasar a formato “largo” ---------------------------------------------
vars <- df_all %>% select(where(is.numeric), -id) %>% names()

df_long <- df_all %>%
  pivot_longer(all_of(vars), names_to = "variable", values_to = "valor")

# 3) Resumen descriptivo por individuo y fase ---------------------------
desc_indiv <- df_long %>%
  group_by(variable, id, fase) %>%
  summarise(
    valores = list(valor),     
    mean   = mean(valor, na.rm = TRUE),
    median = median(valor,na.rm = TRUE),
    .groups="drop"
  )
  

print(desc_indiv)

## Si observamos la tabla resumen veremos que esto nos ayuda a detectar que variables 
## presenta NA en periodos de antes y despues, e incluso durante para variable - individuo 
# (durante no tiene NA porque estuvo controlado el experimento para llevar los relojes) 
# pero en algunos casos durante también presenta NA y por tanto impossible realizar 
## comparativas

# 1) Filtrar las filas donde mean o sd están vacíos (NA ó NaN)
na_summary <- desc_indiv %>%
  filter(is.na(mean) | is.nan(mean) | is.na(median)    | is.nan(median))

# Ver qué combinación variable–id (y fase) está fallando
print(na_summary)

# 2) Extraer solo id y variable (si no te importa la fase)
na_indiv_var <- na_summary %>%
  distinct(id, variable)

print(na_indiv_var)


# 3) Para tener, por cada id, el listado de variables problemáticas
na_vars_by_id <- na_indiv_var %>%
  group_by(id) %>%
  summarise(
    vars_with_NA = paste(variable, collapse = ","), # llistat de variables que presenten NA en algun antes/durante/despues
    .groups = "drop"
  )

print(na_vars_by_id)


# Primero, reconstruimos el tibble de pares id–variable
na_indiv_var <- na_vars_by_id %>%
  # separar la columna vars_with_NA en múltiples filas
  separate_rows(vars_with_NA, sep = ",") %>%
  rename(variable = vars_with_NA)

# Ahora excluimos esas combinaciones de desc_indiv
desc_indiv_clean <- desc_indiv %>%
  anti_join(na_indiv_var, by = c("id", "variable"))
print(desc_indiv_clean)

#Si además necesitas quitar esos pares de tu df_long antes de los análisis posteriores, 
#sería un proceso análogo:

df_long_clean <- df_long %>%
  anti_join(na_indiv_var, by = c("id", "variable"))

#De este modo garantizas que todas las fases de análisis (ANOVA de medidas repetidas, Friedman, post-hoc…) 
#solo incluyan los individuos y variables con resúmenes completos.


########
# Analisis
########

#El flujo general es:
  
### ANOVA de medidas repetidas (o Friedman si no se cumplen supuestos) sobre las medias.
### ANOVA de medidas repetidas (o Friedman) sobre las medianas.
### Comparaciones post‐hoc pareadas (t‐test o Wilcoxon, según corresponda).

flag <- function(vector){
  # si hi ha menys de 3 valors no podem fer anova, ni shapiro 
  vector.uni <- unique(vector)
  if(sum(!is.na(vector.uni)) < 3 | sum(!is.nan(vector.uni)) < 3){
    return(1)
  } else(return(0))
}


## Comprovar normalitat
# 2.1 Normalidad por grupo (Shapiro–Wilk)

deteccio <- desc_indiv_clean %>%
  mutate("flag" = map_dbl(valores, ~ flag(..1))) %>% 
  filter(flag == 1) %>% 
  dplyr::select(id, variable)

desc_indiv_clean <- desc_indiv_clean %>%
  anti_join(deteccio, by = c("id", "variable")) 



shapiro_res <- desc_indiv_clean %>%
  mutate("shapiro_test" = map(valores, ~ shapiro_test(..1))) %>% 
  mutate("pvalor" = map_dbl(shapiro_test, ~ ..1$p.value))

# en que hi hagui només una de les fases sense normalitat ja no es pot fer anova
delete_NOnormal <- shapiro_res %>% filter(pvalor < 0.05) %>%
  dplyr::select(variable, id) %>% unique 


shapiro_res_clean <- shapiro_res %>% 
  anti_join(delete_NOnormal, by = c("id", "variable"))

### Aquestes son les que poden fer-se anova, en la resta hi ha que recurrir a un altre tipus d'anàlisi -> Friedman
### Ara queda veure que complisquen les condicions d'esfericitat

aux <- shapiro_res_clean %>% dplyr::select(variable,id) %>% unique
pval.leven <- numeric(nrow(aux))
for(i in 1:nrow(aux)){
  vari <- aux$variable[i]
  indiv <- aux$id[i]
  df.aux <- shapiro_res_clean %>% filter(variable == vari, id==indiv) %>% dplyr::select(variable, id, fase, valores)
  df_expandido <- df.aux %>%
    unnest(cols = valores) %>%        # “despliega” cada lista en filas
    rename(valor = valores) 
  leven.test <- leveneTest(valor ~ fase, data = df_expandido)# si p<0.05 rebutja homogenietat de variàncies. ANOVA necessita que esto siga cert
  pval.leven[i] <- leven.test$`Pr(>F)`
}

aux$pval.leven <- pval.leven # todas aquellas que no cumplan levenne no se puede aplicar anova

delete.NOleven <- aux %>% filter(pval.leven < 0.05) %>%
  dplyr::select(variable, id) %>% unique 

shapiro_leven_res_clean <- shapiro_res_clean %>% 
  anti_join(delete.NOleven, by = c("id", "variable")) # totes aquestes verifiquen

shapiro_leven_res_clean_expandido <- shapiro_leven_res_clean %>%
  unnest(cols = valores) %>%        
  rename(valor = valores) %>% dplyr::select(variable, id, fase,  valor)

anova_by_id <- shapiro_leven_res_clean_expandido %>%
  group_by(variable, id) %>%
  do({
    fit <- aov(valor ~ fase, data = .)
    broom::tidy(fit)
  }) %>%
  ungroup() %>%
  # Nos quedamos solo con el término “fase”
  filter(term == "fase") %>%
  select(variable, id,
         df = df, 
         F = statistic, 
         p.value)

anova_by_id$method <- rep("ANOVA", nrow(anova_by_id))

print(anova_by_id) # si p<0.05 rebutje la igualtat de mitjanes entre fases, almenys una es diferents (problema -> que no sabem quina)

# CONCLUSIÓ ANOVA!!!
# independement d'això, si no es rebutja la igualtat de mitjanes, este mètode ens ajuda a detectar quines variables 
# son tenen un comportament no significativament diferent entre fases.

# Endemés, en els casos en que la ANOVA ha donat significatiu i, per tant, hi ha diferències significatives entre 
# almeny un dels grups respecte de la resta. Aleshores apliquem el test de Tuckey:


# 1) Filtrar sólo los combos con ANOVA significativa
sig_anova <- anova_by_id %>%
  filter(p.value < 0.05) %>%
  select(variable, id)

# 2) Para cada uno, hacer Tukey HSD
tukey_by_id <- shapiro_leven_res_clean_expandido %>%
  semi_join(sig_anova, by = c("variable", "id")) %>%  # quedarnos sólo con casos sign.
  group_by(variable, id) %>%
  do({
    fit <- aov(valor ~ fase, data = .)
    th  <- TukeyHSD(fit, "fase")
    df  <- as.data.frame(th$fase) %>%
      tibble::rownames_to_column("comparison") %>%
      transmute(
        comparison,
        adj.p.value = `p adj`
      )
    # añadimos cols de clave
    mutate(df,
           variable = unique(.$variable),
           id       = unique(.$id))
  }) %>%
  ungroup() %>%
  select(variable, id, comparison, adj.p.value)

print(tukey_by_id)



# A aquells que no hem aplicar anova apliquem krustal-wallis: que no presenta suposicioons de normalitat ni homogenietat de variancia


desc_indiv_clean_krutal <- desc_indiv_clean %>%
  anti_join(anova_by_id %>% dplyr::select(variable, id),  # elimine aquelles que ja els he aplicat anova
            by = c("id", "variable")) 


desc_indiv_clean_krutal_expandido <- desc_indiv_clean_krutal %>%
  unnest(cols = valores) %>%        
  rename(valor = valores) %>% dplyr::select(variable, id, fase,  valor)


kruskal_by_id <- desc_indiv_clean_krutal_expandido %>%
  group_by(variable, id) %>%
  kruskal_test(valor ~ fase) %>%
  ungroup()

print(kruskal_by_id)

# Els casos en que els krustal dona significatiu podem aplicar post-hoc per veure entre 
# quines fases hi ha diferències mitjançant el post-hoc dunn


detec.Krustal.signif <- kruskal_by_id %>% filter(p>0.05) %>%
  dplyr::select(variable, id) # agafe els que no son significatius 

desc_indiv_clean_krutal_signif_expandido_ <- desc_indiv_clean_krutal_expandido %>%
  anti_join(detec.Krustal.signif,  # elimine aquelles que krustal no dona significatiu
            by = c("id", "variable")) 


posthoc_dunn_by_id <- desc_indiv_clean_krutal_signif_expandido_ %>%
  group_by(variable, id) %>%
  dunn_test(
    valor ~ fase,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup()

print(posthoc_dunn_by_id)



