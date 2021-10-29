
################# ANÁLISIS  INVERTEBRADOS CDS ##############


### 1. Cargar paquetes y funciones
source("scripts/1-paquetes.R")

### 2. Procesar los datos ambientales (solo correr esto una vez,
# ### ya que  los datos procesados se guardan en un csv nuevo) 
source("scripts/2-procesar-datos.R")

### 3. Cargar datos
pest_data <- read_csv("data/processed/datos de insectos y temperatura final.csv")

### Ajustar variables como factores
pest_data <- pest_data %>% mutate_at(vars(periodo_monitoreo,
                                          monitoreo,
                                          tipo_trampa,
                                          marca_trampa,
                                          trampa_ID_unico,
                                          ubicacion,
                                          orden,
                                          familia,
                                          especie), list(factor))

###### Comenzamos con el análisis de las 3 hipótesis propuestas para el paper

### HIPÓTESIS 1 ###

# Hay una asociación significativa entre la abundancia de invertebrados
#### y la temperatura o humedad en el herbario? 

### Primero testeamos el efecto de temperatura y humedad máxima en la abundancia de invertebrados
full_model_max <- glmer(abundancia_trampa ~ scale(max_humi) + scale(max_temp)
                        + scale(dias_monitoreo)
                        + (1|orden)
                        + (1|trampa_ID_unico),
                        family=poisson,
                        data=pest_data)

summary(full_model_max)
reduced_model_temp <- glmer(abundancia_trampa ~ 
                              scale(max_humi) + 
                              scale(dias_monitoreo) +
                              (1|orden) +
                              (1|trampa_ID_unico),
                            family=poisson,
                            data=pest_data)
reduced_model_humid <- glmer(abundancia_trampa ~ 
                               scale(max_temp) + 
                               scale(dias_monitoreo) +
                               (1|orden) +
                               (1|trampa_ID_unico),
                             family=poisson,
                             data=pest_data)


# testear efecto de temperatura:
anova(full_model_max, reduced_model_temp)

# testear efecto de humedad:
anova(full_model_max, reduced_model_humid)

# Desescalar los resultados para ver el efecto de temperatura en los inverts
mean(pest_data$max_temp, na.rm=T)
#24.74459
sd(pest_data$max_temp, na.rm=T)
#1.164569
(1*1.164569+24.74459)*0.280872
# por cada 1ºC más, hay aproximadamente 7.277157 invertebrados más
(1*1.164569+24.74459)*0.119955
# 3.107933 S.E.


### Scatterplots de abundancia de invertebrados y temp/hum

# función para hacer colores transparentes
t_col <- function(color, opacity = 0.5) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], max = 255, alpha = (opacity)*255)
  invisible(t.col)
}

#### Temperatura

#quartz(w=5,h=4)
pdf(file="results/scatterplot-all-inv-maxtemp.pdf",width=5,height=4)
par(mar=c(4,5,2,2))
plot(data=pest_data, jitter(abundancia_trampa) ~ jitter(max_temp), log="y",
     xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.5,
     col=t_col("black",0.5))
#añadir ejes
axis(side=1, seq(0, max(pest_data$max_temp)+2, 1), padj=-0.8)
axis(side=2, at=c(1,2,5,10,20,50,100), hadj=0.8, las=2)
#axis(side=2, at=seq(0, 140, 20), hadj=0.8, las=2)
#añadir etiquetas para ejes 
mtext(side=1, "Maximum Temperature  (ºC)", line=2.8, cex=1, font=2)
mtext(side=2, "Number of Invertebrates\n(log scaled)", line=2.8, cex=1, font=2)
## en la leyenda, añadir que los datos fueron hechos con "jitter"
dev.off()

#### Humedad

#quartz(w=5,h=4)
pdf(file="results/scatterplot-all-inv-maxrelhum.pdf",width=5,height=4)
par(mar=c(4,5,2,2))
plot(data=pest_data, jitter(abundancia_trampa) ~ (max_humi), log="y",
     xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.5,
     col=t_col("black",0.5))
#añadir ejes 
axis(side=1, at=seq(0, 70, 5), padj=-0.8)
axis(side=2, at=c(1,2,5,10,20,50), hadj=0.8, las=2)
#añadir etiquetas para ejes  
mtext(side=1, "Maximum Relative Humidity (%)", line=2.8, cex=1, font=2)
mtext(side=2, "Number of Invertebrates\n(log scaled)", line=2.8, cex=1, font=2)
## en la leyenda, añadir que los datos fueron hechos con "jitter"
dev.off()

## Ahora modelar solo con especies de posibles plagas
pest_data_onlypests = filter(pest_data, pest == "potential") # solo posibles plagas

pest_full_model_max <- glmer(abundancia_trampa ~ scale(max_humi) + scale(max_temp)
                        + scale(dias_monitoreo)
                        + (1|orden)
                        + (1|trampa_ID_unico),
                        family=poisson,
                        data=pest_data_onlypests)
summary(full_model_max)

# eliminar variable de temperatura para testear el efecto de temperatura en plagas
pest_reduced_model_temp <- glmer(abundancia_trampa ~ 
                              scale(max_humi) + 
                                scale(dias_monitoreo) +
                              (1|orden) +
                              (1|trampa_ID_unico),
                            family=poisson,
                            data=pest_data_onlypests)

# Comparar modelos con LRT
anova(pest_full_model_max, pest_reduced_model_temp)

# testear efecto de humedad en solo plagas

pest_reduced_model_humid <- glmer(abundancia_trampa ~ 
                               scale(max_temp) + 
                                 scale(dias_monitoreo) +
                               (1|orden) +
                               (1|trampa_ID_unico),
                             family=poisson,
                             data=pest_data_onlypests)

anova(pest_full_model_max, pest_reduced_model_humid)

dev.off()
print(plot(1))
#### Scatterplot de temperatura y humedad relativa
plot(data=pest_data, max_humi ~ max_temp)

plot(data=pest_data_onlypests, abundancia_trampa ~ max_temp, log="y")
plot(data=pest_data_onlypests, abundancia_trampa ~ max_humi, log="y")

#### Temperatura

#quartz(w=5,h=4)
pdf(file="results/scatterplot-pests-maxtemp.pdf",width=5,height=4)
par(mar=c(4,5,2,2))
plot(data=pest_data_onlypests, jitter(abundancia_trampa) ~ jitter(max_temp), log="y",
     xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.5,
     col=t_col("black",0.5))
#añadir ejes 
axis(side=1, seq(0, max(pest_data_onlypests$max_temp)+2, 1), padj=-0.8)
axis(side=2, at=c(1,2,5,10,20,50), hadj=0.8, las=2)
#añadir etiquetas de ejes a
mtext(side=1, "Maximum Temperature  (ºC)", line=2.8, cex=1.1, font=2)
mtext(side=2, "Number of Pests\n(log scaled)", line=2.8, cex=1, font=2)
## en la leyenda, añadir que los datos fueron hechos con "jitter"
dev.off()

#### Humedad

#quartz(w=5,h=4)
pdf(file="results/scatterplot-pests-maxrelhum.pdf",width=5,height=4)
par(mar=c(4,5,2,2))
plot(data=pest_data_onlypests, jitter(abundancia_trampa) ~ (max_humi), log="y",
     xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.5,
     col=t_col("black",0.5))
 #añadir ejes
axis(side=1, at=seq(0, 70, 5), padj=-0.8)
axis(side=2, at=c(1,2,5,10,20,50), hadj=0.8, las=2)
 #añadir etiquetas para ejes 
mtext(side=1, "Maximum Relative Humidity (%)", line=2.8, cex=1.1, font=2)
mtext(side=2, "Number of Pests\n(log scaled)", line=2.8, cex=1, font=2)
## en la leyenda, añadir que los datos fueron hechos con "jitter"
dev.off()


#### HIPÓTESIS 2 ####
## El número de días en los que temp o hum fue mayor de las recomendaciones en un 
## periodo de monitoreo tuvo algún efecto significativo en el número de invertebrados en
## aquel periodo de monitoreo?

### El objetivo es analizar la eficacia de los límites de temp y hum recomendados
## A continuación testearemos el efecto de largos periodos de temp o hum mayor al límite de temp de 24ºC
## y de humedad del 50% ya que un herbario en los trópicos en donde existen más invertebrados en la naturaleza
## podría reiqerur una humedad más baja de la recomendación general de 60% para mantenerse sin plagas

## En primer lugar debemos calcular las franjas temporales en los que se superó 50% hum o 24ºC en cada monitoreo
## Esto se realizó en el script de "dataclean"
## Ahora se realizará básicamente el mismo análisis que el objetivo anterior


full_model_days <- glmer(abundancia_trampa ~ scale(days_above_50_humi) + scale(days_above_24_temp)
                         + scale(dias_monitoreo)
                         + (1|orden)
                         + (1|trampa_ID_unico),
                         family=poisson,
                         data=pest_data)

summary(full_model_days)

plot(data=pest_data, days_above_50_humi ~ days_above_24_temp, cex=abundancia_trampa/10)


reduced_model_days_temp <- glmer(abundancia_trampa ~ scale(days_above_50_humi) 
                                 + scale(dias_monitoreo)
                                 + (1|orden)
                                 + (1|trampa_ID_unico),
                                 family=poisson,
                                 data=pest_data)
summary(reduced_model_days_temp)

anova(full_model_days, reduced_model_days_temp)

reduced_model_days_hum <- glmer(abundancia_trampa ~ scale(days_above_24_temp) 
                                + scale(dias_monitoreo)
                                + (1|orden)
                                + (1|trampa_ID_unico),
                                family=poisson,
                                data=pest_data)
summary(reduced_model_days_hum)

anova(full_model_days, reduced_model_days_hum)

##########
#  Realizamos el mismo análisis pero solo para las potenciales plagas

  # Modelo completo
full_model_days <- glmer(abundancia_trampa ~ scale(days_above_50_humi) + scale(days_above_24_temp)
                         + scale(dias_monitoreo)
                         + (1|orden)
                         + (1|trampa_ID_unico),
                         family=poisson,
                         data=pest_data_onlypests)

summary(full_model_days)

plot(data=pest_data_onlypests, days_above_50_humi ~ days_above_24_temp, cex=abundancia_trampa/10)

  # Modelo reducido -  temperatura
reduced_model_days_temp <- glmer(abundancia_trampa ~ scale(days_above_50_humi) 
                                 + scale(dias_monitoreo)
                                 + (1|orden)
                                 + (1|trampa_ID_unico),
                                 family=poisson,
                                 data=pest_data_onlypests)
summary(reduced_model_days_temp)


anova(full_model_days, reduced_model_days_temp)


# Modelo reducido -  humedad

reduced_model_days_hum <- glmer(abundancia_trampa ~ scale(days_above_24_temp) 
                                + scale(dias_monitoreo)
                                + (1|orden)
                                + (1|trampa_ID_unico),
                                family=poisson,
                                data=pest_data_onlypests)
summary(reduced_model_days_hum)

anova(full_model_days, reduced_model_days_hum)


#####################

### HIPÓTESIS NÚMERO 3: Cuánta correlación hay entre el clima externo con el interno dentro del herbario? 
######## A esta correlación le afecta el número de des-humidificadores o sistemas de aire  prendidos en el herbario?

clim_dat <- read_csv("data/raw/climate_puerto-ayora.csv")
# remover los años iniciales:
clim_dat <- clim_dat[-c(1:15000),]

# cargar datos de herbario procesados:
herb_data <- read_csv("data/processed/enviro_data_clean.csv") %>% 
  select(fecha, year, monitoring_interval, herbarium_temp_avg = temp_avg, herbarium_humi_avg = humi_avg)

# combinar con datos ambientales externos:
all_env_data <- left_join(herb_data, clim_dat, by=c("fecha" = "observation_date"))

### Para los gráficos:
names(all_env_data)

## temperatura:
plot(herbarium_temp_avg ~ mean_air_temp, data=all_env_data, asp=1)
abline(b=1, a=0)
## humedad:
plot(herbarium_humi_avg ~ humidity, data=all_env_data, asp=1)
# quitar valores atípicos de cero para la humedad externa
all_env_data <- filter(all_env_data, humidity > 0)
plot(herbarium_humi_avg ~ humidity, data=all_env_data, asp=1, xlim=c(70,100))
abline(b=1, a=0)

## Gráficos ## parte 2

 ## Temperatura
ggscatter(all_env_data, x = "mean_air_temp", y = "herbarium_temp_avg", 
          add = "reg.line", add.params= list(color="blue"), conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Air Temperature (ºC)", ylab = "Mean Herbarium Temperature (ºC)")

## Humedad relativa
ggscatter(all_env_data, x = "humidity", y = "herbarium_humi_avg", 
          add = "reg.line", add.params= list(color="blue"), conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Air Relative Humidity (%)", ylab = "Mean Herbarium Relative Humidity (%)")


# cor-test para temp

res_temp <- cor.test(all_env_data$mean_air_temp, all_env_data$herbarium_temp_avg, 
                method = "pearson")
res_temp

 # cor-test para humedad

res_hum <- cor.test(all_env_data$humidity, all_env_data$herbarium_humi_avg, 
                method = "pearson")
res_hum

### resumen del número total de invertebrados por especie 
pest_data_summ <- group_by(pest_data, especie) %>% 
  summarize(total_bugs = sum(abundancia_trampa, na.rm=T))

pest_data_summ <- group_by(pest_data_onlypests, especie) %>% 
  summarize(total_bugs = sum(abundancia_trampa, na.rm=T))

write.csv(pest_data_summ, file="data/processed/summary_pest_table.csv")
