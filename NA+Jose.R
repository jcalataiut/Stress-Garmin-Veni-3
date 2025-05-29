rm(list=ls())

library(tidyverse)   # dplyr, ggplot2, tidyr, etc.
library(lubridate)   # manejo de fechas
library(janitor)     # limpieza de nombres de columnas
library(naniar)      # visualización de datos faltantes
# Cargar los datos
datos <- read_csv("datos_practica_final.csv") %>% 
  clean_names()           # Limpiar nombres de columnas para manejo fácil
glimpse(df)
var_fisio<-names(datos)[29:56]

# Transformar datos 
datos <- datos %>%
  filter(calendar_date_local >= as.Date("2024-11-10"))%>%
  dplyr::select(user_id,calendar_date_local,all_of(var_fisio))%>%
  mutate(across(where(is.character),as.factor))

# Encontrar Periodos de estudio 
#donde hay NA's de estress (nuestra variable explicativa)

fechas_NA<-datos%>%
  dplyr::select(user_id,calendar_date_local,stress_qualifier,stress_avg)%>%
  arrange(calendar_date_local,user_id) %>%
  group_by(calendar_date_local) %>%
  summarise(Recuento_NA_Stress = sum(is.na(stress_avg)),
            Recuenta_Qualifier=sum(is.na(stress_qualifier)))
print(fechas_NA,n=200)

# Semana Critica (del 2 al 15 de febrero)
## Semanas antes de critica 2024-12-05 es fecha buena, tomar como limite 
## Semanas despues 2025-03-18 empieza a haber muchos NA's otra vez
critic <- datos%>%
  filter(calendar_date_local>= as.Date("2025-02-02") &
           calendar_date_local<= as.Date("2025-02-15"))%>%
  mutate(fase="Critica")

before<-datos%>%
  filter(calendar_date_local>= as.Date("2025-01-19") &
           calendar_date_local<= as.Date("2025-02-01"))%>%
  mutate(fase="Before")

pre_before<-datos%>%
  filter(calendar_date_local>= as.Date("2025-01-05") &
           calendar_date_local<= as.Date("2025-01-18"))%>%
  mutate(fase="PreBefore")

after<-datos%>%
  filter(calendar_date_local>= as.Date("2025-02-16") &
           calendar_date_local<= as.Date("2025-03-01"))%>%
  mutate(fase="After")

post_after<-datos%>%
  filter(calendar_date_local>= as.Date("2025-03-02") &
           calendar_date_local<= as.Date("2025-03-15"))%>%
  mutate(fase="PostAfter")


datos_periodo<-rbind(
  pre_before,
  before,
  critic,
  after,
  post_after
)
## Cantidad de dias en todo el periodo
longitud_periodo_total<-length(unique(datos_periodo$calendar_date_local))


# Seleccionar las variables de interés (columnas 29 a 56) para análisis de NA
vars_interes <- datos_periodo %>% 
  dplyr::select(-user_id,-calendar_date_local,fase)
# Calcular porcentaje de NA por variable
na_por_variable <- vars_interes %>%
  summarize(across(everything(), ~ mean(is.na(.)) * 100 ))
# Transponer resultado para legibilidad
na_por_variable <- gather(as.data.frame(na_por_variable), "variable", "pct_na")
na_por_variable <- na_por_variable %>% arrange(desc(pct_na))
print(na_por_variable)



### NA's por individuo
na_por_individuo <- datos_periodo%>%
  dplyr::select(user_id, where(is.numeric)) %>%
  pivot_longer(-user_id, names_to = "variable", values_to = "valor") %>%
  group_by(user_id, variable) %>%
  summarise(n_na = sum(is.na(valor)), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = n_na)

###NA's por individuo y fase
na_por_id_fase <- datos_periodo%>%
  dplyr::select(user_id,fase, where(is.numeric)) %>%
  pivot_longer(-c(user_id, fase), names_to = "variable", values_to = "valor") %>%
  group_by(user_id, fase, variable) %>%
  summarise(n_na = sum(is.na(valor)), .groups = "drop") %>%
  pivot_wider(names_from = variable, values_from = n_na) %>%
  arrange(user_id, fase)
print(na_por_id_fase,n=100)

# ================================
# VISUALIZACIÓN DE NAs (Mapa de calor)
# ================================
na_matrix <- datos_periodo %>%
  dplyr::select(user_id, where(is.numeric))%>%
  group_by(user_id) %>%
  summarise_all(~ mean(is.na(.))) %>%
  pivot_longer(-user_id, names_to = "Variable", values_to = "Porcentaje_NA")

ggplot(na_matrix, aes(x = Variable, y = user_id, fill = Porcentaje_NA)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Mapa de calor de porcentajes de NAs",
       x = "Variable Fisiológica", y = "Individuo")

###############################################
################ Codigo JOSE ##################
################ TURBO POCHO ##################
###############################################


# 2) Pasar a formato “largo” ---------------------------------------------
vars <- datos_periodo %>% dplyr::select(where(is.numeric), -user_id) %>% 
  names()

df_long <- datos_periodo %>%
  pivot_longer(all_of(vars), names_to = "variable", values_to = "valor")

# 3) Resumen descriptivo por individuo y fase ---------------------------
desc_indiv <- df_long %>%
  group_by(variable, user_id, fase) %>%
  summarise(
    valores = list(valor),     
    mean   = mean(valor, na.rm = TRUE),
    median = median(valor,na.rm = TRUE),
    .groups="drop"
  )


print(desc_indiv,n=2000)

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
  distinct(user_id, variable)

print(na_indiv_var)


# 3) Para tener, por cada id, el listado de variables problemáticas
na_vars_by_id <- na_indiv_var %>%
  group_by(user_id) %>%
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
  anti_join(na_indiv_var, by = c("user_id", "variable"))
print(desc_indiv_clean)

#Si además necesitas quitar esos pares de tu df_long antes de los análisis posteriores, 
#sería un proceso análogo:

df_long_clean <- df_long %>%
  anti_join(na_indiv_var, by = c("user_id", "variable"))

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
  dplyr::select(user_id, variable)

desc_indiv_clean <- desc_indiv_clean %>%
  anti_join(deteccio, by = c("user_id", "variable")) 




shapiro_res <- desc_indiv_clean %>%
  mutate("shapiro_test" = map(valores, ~ shapiro_test(..1))) %>% 
  mutate("pvalor" = map_dbl(shapiro_test, ~ ..1$p.value))

# en que hi hagui només una de les fases sense normalitat ja no es pot fer anova
delete_NOnormal <- shapiro_res %>% filter(pvalor < 0.05) %>%
  dplyr::select(variable, user_id) %>% unique 


shapiro_res_clean <- shapiro_res %>% 
  anti_join(delete_NOnormal, by = c("user_id", "variable"))

### Aquestes son les que poden fer-se anova, en la resta hi ha que recurrir a un altre tipus d'anàlisi -> Friedman
### Ara queda veure que complisquen les condicions d'esfericitat

aux <- shapiro_res_clean %>% dplyr::select(variable,user_id) %>% unique
pval.leven <- numeric(nrow(aux))
for(i in 1:nrow(aux)){
  vari <- aux$variable[i]
  indiv <- aux$user_id[i]
  df.aux <- shapiro_res_clean %>% filter(variable == vari, user_id==indiv) %>% dplyr::select(variable, user_id, fase, valores)
  df_expandido <- df.aux %>%
    unnest(cols = valores) %>%        # “despliega” cada lista en filas
    rename(valor = valores) 
  leven.test <- leveneTest(valor ~ fase, data = df_expandido)# si p<0.05 rebutja homogenietat de variàncies. ANOVA necessita que esto siga cert
  pval.leven[i] <- leven.test$`Pr(>F)`
}

aux$pval.leven <- pval.leven # todas aquellas que no cumplan levenne no se puede aplicar anova

delete.NOleven <- aux %>% filter(pval.leven < 0.05) %>%
  dplyr::select(variable, user_id) %>% unique 

shapiro_leven_res_clean <- shapiro_res_clean %>% 
  anti_join(delete.NOleven, by = c("user_id", "variable")) # totes aquestes verifiquen

shapiro_leven_res_clean_expandido <- shapiro_leven_res_clean %>%
  unnest(cols = valores) %>%        
  rename(valor = valores) %>% dplyr::select(variable, user_id, fase,  valor)

anova_by_id <- shapiro_leven_res_clean_expandido %>%
  group_by(variable, user_id) %>%
  do({
    fit <- aov(valor ~ fase, data = .)
    broom::tidy(fit)
  }) %>%
  ungroup() %>%
  # Nos quedamos solo con el término “fase”
  filter(term == "fase") %>%
  dplyr::select(variable, user_id,
                df = df, 
                F = statistic, 
                p.value)

anova_by_id$method <- rep("ANOVA", nrow(anova_by_id))

print(anova_by_id) # si p<0.05 rebutje la igualtat de mitjanes entre fases, almenys una es diferents (problema -> que no sabem quina)


anova_by_id_modif <- anova_by_id %>% dplyr::select(variable, user_id, p.value, method)

# CONCLUSIÓ ANOVA!!!
# independement d'això, si no es rebutja la igualtat de mitjanes, este mètode ens ajuda a detectar quines variables 
# son tenen un comportament no significativament diferent entre fases.

# Endemés, en els casos en que la ANOVA ha donat significatiu i, per tant, hi ha diferències significatives entre 
# almeny un dels grups respecte de la resta. Aleshores apliquem el test de Tuckey:


# 1) Filtrar sólo los combos con ANOVA significativa
sig_anova <- anova_by_id %>%
  filter(p.value < 0.05) %>%
  dplyr::select(variable, user_id)

# 2) Para cada uno, hacer Tukey HSD
tukey_by_id <- shapiro_leven_res_clean_expandido %>%
  semi_join(sig_anova, by = c("variable", "user_id")) %>%  # quedarnos sólo con casos sign.
  group_by(variable,user_id) %>%
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
           user_id  = unique(.$user_id))
  }) %>%
  ungroup() %>%
  dplyr::select(variable, user_id, comparison, adj.p.value)

print(tukey_by_id)



# A aquells que no hem aplicar anova apliquem krustal-wallis: que no presenta suposicioons de normalitat ni homogenietat de variancia


desc_indiv_clean_krutal <- desc_indiv_clean %>%
  anti_join(anova_by_id %>% dplyr::select(variable, user_id),  # elimine aquelles que ja els he aplicat anova
            by = c("user_id", "variable")) 


desc_indiv_clean_krutal_expandido <- desc_indiv_clean_krutal %>%
  unnest(cols = valores) %>%        
  rename(valor = valores) %>% dplyr::select(variable, user_id, fase,  valor)


kruskal_by_id <- desc_indiv_clean_krutal_expandido %>%
  group_by(variable, user_id) %>%
  kruskal_test(valor ~ fase) %>%
  ungroup()

print(kruskal_by_id)

# Els casos en que els krustal dona significatiu podem aplicar post-hoc per veure entre 
# quines fases hi ha diferències mitjançant el post-hoc dunn


detec.Krustal.signif <- kruskal_by_id %>% filter(p>0.05) %>%
  dplyr::select(variable, user_id) # agafe els que no son significatius 

desc_indiv_clean_krutal_signif_expandido_ <- desc_indiv_clean_krutal_expandido %>%
  anti_join(detec.Krustal.signif,  # elimine aquelles que krustal no dona significatiu
            by = c("user_id", "variable")) 


posthoc_dunn_by_id <- desc_indiv_clean_krutal_signif_expandido_ %>%
  group_by(variable, user_id) %>%
  dunn_test(
    valor ~ fase,
    p.adjust.method = "bonferroni"
  ) %>%
  ungroup()

print(posthoc_dunn_by_id)


names(anova_by_id_modif) <- c("variable", "user_id", "p", "method")
kruskal_by_id_modif <- kruskal_by_id %>% dplyr::select(variable, user_id, p, method)

var_sig <- rbind(anova_by_id_modif, kruskal_by_id_modif)%>% filter(p<0.05) %>% arrange(variable)%>%
  dplyr::select("user_id",everything())

#####################################
#### VERSION CARLOS ################
####################################

# ============================
# CONFIGURACIÓN INICIAL
# ============================
library(tidyverse)
library(janitor)
library(rstatix)
library(car)
library(broom)
# Obtener columnas numéricas que NO sean calendar_date_local
cols_largos <- datos_periodo %>%
  dplyr::select(where(is.numeric)) %>%
  names()
cols_largos <- setdiff(cols_largos, "calendar_date_local")  # quitar si está incluida

# Pivotear
df_long <- datos_periodo %>%
  pivot_longer(cols = all_of(cols_largos),
               names_to = "variable",
               values_to = "valor")

# 2. RESUMEN POR INDIVIDUO Y VARIABLE
desc_indiv <- df_long %>%
  group_by(user_id, variable, fase) %>%
  summarise(valores = list(valor), .groups = "drop")

# 3. DESCARTAR COMBINACIONES CON <3 VALORES ÚNICOS
validos <- desc_indiv %>%
  mutate(val_ok = map_lgl(valores, ~ sum(!is.na(unique(.))) >= 3)) %>%
  filter(val_ok) %>%
  dplyr::select(-val_ok)

# 4. TEST SHAPIRO POR GRUPO
shapiro_res <- validos %>%
  mutate(pvalor = map_dbl(valores, ~ shapiro_test(.)$p.value)) %>%
  group_by(user_id, variable) %>%
  summarise(normal = all(pvalor > 0.05), .groups = "drop")


# 5. FILTRAR COMBOS NORMALES
df_normal <- validos %>%
  semi_join(shapiro_res %>% filter(normal), by = c("user_id", "variable"))

# 6. LEVENE TEST (SÓLO PARA NORMALES)
levene_check <- df_normal %>%
  unnest(cols = valores) %>%
  rename(valor = valores) %>%
  group_by(user_id, variable) %>%
  do({
    df <- .
    pval <- tryCatch({
      leveneTest(valor ~ fase, data = df)$`Pr(>F)`[1]
    }, error = function(e) NA)
    tibble(p_levene = pval)
  }) %>%
  ungroup() %>%
  mutate(homogeneo = p_levene > 0.05)

# 7. ANOVA PARA LOS QUE SON NORMALES Y HOMOGÉNEOS
df_anova <- df_normal %>%
  unnest(cols = valores) %>%
  rename(valor = valores) %>%
  semi_join(levene_check %>% filter(homogeneo), by = c("user_id", "variable"))

anova_res <- df_anova %>%
  group_by(user_id, variable) %>%
  do(tidy(aov(valor ~ fase, data = .))) %>%
  filter(term == "fase") %>%
  ungroup() %>%
  mutate(method = "ANOVA")

# 8. TUKEY POST-HOC
sig_anova <- anova_res %>% 
  filter(p.value < 0.05) %>% 
  dplyr::select(user_id, variable)

tukey_res <- df_anova %>%
  semi_join(sig_anova, by = c("user_id", "variable")) %>%
  group_by(user_id, variable) %>%
  do({
    fit <- aov(valor ~ fase, data = .)
    tukey <- TukeyHSD(fit)$fase
    tibble(
      comparison = rownames(tukey),
      adj.p.value = tukey[, "p adj"]
    )
  }) %>%
  ungroup()

# 9. KRUSKAL PARA LOS DEMÁS
df_kruskal <- desc_indiv %>%
  anti_join(anova_res %>% dplyr::select(user_id, variable), by = c("user_id", "variable")) %>%
  unnest(cols = valores) %>%
  rename(valor = valores)

df_kruskal_filtrado <- df_kruskal %>%
  group_by(user_id, variable) %>%
  filter(n_distinct(fase[!is.na(valor)]) >= 2) %>%
  ungroup()


kruskal_res <- df_kruskal_filtrado  %>%
  group_by(user_id, variable) %>%
  kruskal_test(valor ~ fase) %>%
  ungroup() %>%
  mutate(method = "Kruskal")

# 10. DUNN POST-HOC
dunn_res <- df_kruskal %>%
  semi_join(kruskal_res %>% filter(p < 0.05), by = c("user_id", "variable")) %>%
  group_by(user_id, variable) %>%
  dunn_test(valor ~ fase, p.adjust.method = "bonferroni") %>%
  ungroup()

# ============================
# RESULTADOS FINALES
# ============================

# Combinar resultados
final_stats <- bind_rows(
  anova_res %>% dplyr::select(user_id, variable, p.value, method),
  kruskal_res %>% dplyr::select(user_id, variable, p = p, method)
) %>%
  filter(p.value < 0.05 | p < 0.05)

# Mostrar
print(final_stats)
print(tukey_res)
print(dunn_res)

final_stats_2 <- bind_rows(
  anova_res %>% dplyr::select(user_id, variable, p.value, method),
  kruskal_res %>% dplyr::select(user_id, variable, p.value = p, method)
) %>%
  filter(p.value < 0.05)%>%
  arrange(variable)

anti_join(var_sig, final_stats_2, by = c("user_id", "variable"))
anti_join(final_stats_2, var_sig, by = c("user_id", "variable"))


#######################################################
############### VARIABLES A TRATAR  ###################
#######################################################

names_var_sig<-unique(var_sig$variable)
ids_a_filtrar <- c("id_001", "id_005", "id_007", "id_009")
var_pocos_NAs<-na_matrix%>%
  filter(Porcentaje_NA<=0.2)%>%
  dplyr::select(Variable)%>%
  unique()%>%
  pull()


datos_periodo_sig <- datos_periodo %>%
  filter(user_id %in% ids_a_filtrar) %>%
  droplevels()%>%
  dplyr::select(user_id,calendar_date_local,all_of(var_pocos_NAs))

## Imputar por MEDIANA

datos_periodo_sig_imputado <- datos_periodo_sig %>%
  mutate(across(
    .cols = all_of(var_pocos_NAs),
    .fns = ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

colSums(is.na(datos_periodo_sig_imputado)) ## COMPROBAR NA's, se han imputado correctamente






