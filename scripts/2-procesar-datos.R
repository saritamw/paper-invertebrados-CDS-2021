################ DATOS INVERTEBRADOS CDS #######################

## Exploración de datos de invertebrados y variables ambientales CDS

## Limpieza de datos, buscar NAs y preparar datos para el análisis
## Resumir datos para estimar medias de temperatura y humedad en los periodos de monitoreo
### ver los datos de invertebrados

## Primero hay que limpiar los datos actualizqdos:
trap_data_raw <- read_csv("data/raw/datos_trampas_2017_21.csv")

## Crear una lista taxonómica de todas las especies:
taxa_list <- select(trap_data_raw, species=especie, family=familia, order=orden) %>% 
  arrange(species, family, order) %>% 
  distinct(species, .keep_all=T)

# Verificar que los nombres están bien escritos:
sort(unique(taxa_list$order))


monitor_data <- mutate(trap_data_raw,
                       # corregir tipos de columna y crear nuevos si es necesario
                       fecha_recogida = mdy(fecha_recogida),
                       fecha_puesta = mdy(fecha_puesta),
                       dias_monitoreo = fecha_recogida - fecha_puesta,
                       trampa_ID = paste(periodo_monitoreo, tipo_trampa, ubicacion, num_trampa_monitoreo, sep=".")) %>% 
  # "left-join" la lista de taxones para completar familias y ordenes que falten
  left_join(taxa_list, by=c("especie"="species")) %>% 
  select(-familia, -orden) %>% 
  rename(familia = family, orden = order) %>% 
  # eliminamos las columnas que ya no requerimos:
  select(-(especies_monitoreo:abundancia_monitoreo)) %>% 
  mutate(monitoreo = as.numeric(as.character(monitoreo)),
         monitoreo = monitoreo-1,
         monitoreo = paste0("interv", monitoreo))

## Actualizaciones de formato al archivo de datos ambientales:
env_data <- read_csv("data/raw/datos_temp_hum_2012-2021.csv", 
                     col_types = cols(`Temperatura (zona ampliación)` = col_number(),
                                      `Humedad (zona ampliación)` = col_number())) %>% 
  rename(fecha = Fecha, dia = Dia, Temp_entrada = `Temperatura (entrada)`,
         Temp_ZA = `Temperatura (zona ampliación)`, Humedad_entrada = `Humedad (entrada)`,
         Humedad_ZA = `Humedad (zona ampliación)`) %>% 
  mutate(fecha = dmy(fecha)) %>% 
  # añadir columnas nuevas:
  mutate(meses = month(fecha),
         meses_nombre = month(fecha, label=T),
         dias_num = day(fecha),
         year = year(fecha))

# comprobar archivos
names(env_data)
head(env_data)
str(env_data)

names(monitor_data)
head(monitor_data)
str(monitor_data)

# cambiar tipo de variables a factores
env_data <- env_data %>% mutate_at(vars(meses, meses_nombre, year), list(factor))
monitor_data <- monitor_data %>% mutate_at(vars(periodo_monitoreo,
                                                monitoreo,
                                                tipo_trampa,
                                                marca_trampa,
                                                trampa_ID_unico,
                                                ubicacion,
                                                orden,
                                                familia,
                                                especie), list(factor))

## Crear una tabla con una columna de invertebrados que pueden ser plagas
pest_table <- tibble(species = unique(monitor_data$especie), pest = NA)
write.csv(pest_table, "data/raw/pest_table.csv", row.names=F)

pest_table <- read_csv("data/raw/pest_table_filled.csv")

monitor_data <- left_join(monitor_data, pest_table, by=c("especie"="species"))

# num_trampa_monitoreo es un trampa ID por monitoreo.

### Para estimar estadísticas por mes parecidas a los intervalos en los datos de insectos

# Establecer intervalos de periodos de monitoreo
interv0 <- interval(ymd("2017-05-11"), ymd("2017-12-11"))
interv1 <- interval(ymd("2017-12-12"), ymd("2018-07-11"))
interv2 <- interval(ymd("2018-07-12"), ymd("2019-02-11"))
interv3 <- interval(ymd("2019-02-12"), ymd("2019-09-11"))
interv4 <- interval(ymd("2019-02-12"), ymd("2020-01-20"))
interv5 <- interval(ymd("2020-01-21"), ymd("2020-04-19"))
interv6 <- interval(ymd("2020-04-20"), ymd("2020-07-22"))
interv7 <- interval(ymd("2020-07-23"), ymd("2020-10-22"))
interv8 <- interval(ymd("2020-10-23"), ymd("2021-01-22"))
interv9 <- interval(ymd("2021-01-23"), ymd("2021-04-23"))


# Crear todos los intervalos de monitoreo para los datos ambientales:
env_data_summ <- mutate(env_data, 
                        monitoring_interval = case_when(fecha %within% interv0 ~ "interv0",
                                                        fecha %within% interv1 ~ "interv1",
                                                        fecha %within% interv2 ~ "interv2",
                                                        fecha %within% interv3 ~ "interv3",
                                                        fecha %within% interv4 ~ "interv4",
                                                        fecha %within% interv5 ~ "interv5",
                                                        fecha %within% interv6 ~ "interv6",
                                                        fecha %within% interv7 ~ "interv7",
                                                        fecha %within% interv8 ~ "interv8",
                                                        fecha %within% interv9 ~ "interv9")) %>% 
# Filtrar los valores NA que son los que no pertenecen a ningún intervalo
  filter(!is.na(monitoring_interval)) %>% 
  mutate(temp_avg = (Temp_entrada + Temp_ZA)/2,
         humi_avg = (Humedad_entrada + Humedad_ZA)/2,
         # añadir excepción para los NAs:
         temp_avg = ifelse(is.na(Temp_entrada), Temp_ZA, temp_avg),
         temp_avg = ifelse(is.na(Temp_ZA), Temp_entrada, temp_avg),
         humi_avg = ifelse(is.na(Humedad_entrada), Humedad_ZA, humi_avg),
         humi_avg = ifelse(is.na(Humedad_ZA), Humedad_entrada, humi_avg)) %>% 
  {. ->> env_data_clean} %>% 
  group_by(monitoring_interval) %>%
  summarize(mean_temp = mean(temp_avg, na.rm=T), 
            sd_temp = sd(temp_avg, na.rm=T), 
            se_temp = std.error(temp_avg, na.rm=T),
            mean_hum = mean(humi_avg, na.rm=T), 
            sd_hum = sd(humi_avg, na.rm=T),
            se_hum = std.error(humi_avg, na.rm=T),
            max_temp = max(temp_avg, na.rm=T),
            min_temp = min(temp_avg, na.rm=T),
            max_humi = max(humi_avg, na.rm=T),
            min_humi = min(humi_avg, na.rm=T),
            days_above_50_humi = sum(humi_avg > 50, na.rm=T),
            days_above_24_temp = sum(temp_avg > 24, na.rm=T))

env_data_summ$days_above_50_humi
env_data_summ$days_above_24_temp

## Unir los resúmenes ambientales a los datos de monitoreo:
insect_env_full_data <- left_join(monitor_data, env_data_summ, by = c("monitoreo" = "monitoring_interval"))


## Datos procesados con los resúmenes ##
write_csv(insect_env_full_data, "data/processed/datos de insectos y temperatura final.csv")
write_csv(env_data_clean, "data/processed/enviro_data_clean.csv")


## Preparación de datos para  el análisis de RDA:
##### Mediciones inividuales por TRAMPA y ABUNDANCIA de especies
# TRAMPA sería cada fila y especies por columna con su abundancia

# Por ahora seleccionaré TRAMPA ID, ESPECIE y ABUNDANCIA para armar una matriz

all_data <- read_csv("data/processed/datos de insectos y temperatura final.csv")

# Filtrar columnas

abundance <- select(all_data, trampa_ID_unico, especie, abundancia_trampa)

# Cambiar especies a una serie de columnas con su abundancia respectiva por trampa

species <- abundance %>% group_by(trampa_ID_unico) %>% dplyr::mutate(i1 = row_number()) %>% spread(especie, abundancia_trampa) %>% select(-i1)
species[is.na(species)] = 0

# Combinar tablas y filtrar columnas. REMOVER: 
species_all <- bind_cols(species, all_data)
species_all <- rename(species_all, trampa_ID_unico = trampa_ID_unico...1)
species_col <- species_all[-c(57:59,61:73)]

str(species_col)
species_col <- species_col %>% mutate_at(vars(c(52:57)), list(factor))


species_sum <- species_col %>% group_by(trampa_ID_unico) %>%
  summarize(across(c(1:48), sum))

species_info <- select(species_col, (c(1,50:57)))
species_info <- species_info %>% group_by(trampa_ID_unico) %>% filter(row_number()==1) 

species_final <- bind_cols(species_sum, species_info)
species_final <- rename(species_final, trampa_ID_unico = trampa_ID_unico...1)
species_final <- species_final[-c(50)]

# Matrices finales, creadas para la RDA. Matriz de especies:

write_csv(species_final, "data/processed/Matrix especies.csv")

# Hacer una matriz de temperatura y humedad por trampa

temp_hum_col <- select(all_data, trampa_ID_unico, mean_temp, mean_hum, fecha_puesta, 
                       fecha_recogida, dias_monitoreo, periodo_monitoreo,
                       monitoreo, tipo_trampa, marca_trampa, ubicacion)
temp_hum_col <- temp_hum_col %>% group_by(trampa_ID_unico) %>% filter(row_number()==1)

# Matrices finales, para la RDA. Matriz de parametros ambientales:

write_csv(temp_hum_col, "data/processed/Matrix temperatura humedad.csv")


