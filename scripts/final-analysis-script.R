
# https://bodo-winter.net/tutorial/bw_LME_tutorial2.pdf

### First load the packages and functions
source("scripts/00_packages.R")

# ### Next, clean the environmental data (only need to run this once because it 
# ### saves the clean data to a new csv
source("scripts/01_dataclean.R")

## Load data

pest_data <- read_csv("data/processed/datos de insectos y temperatura final.csv")

# Ajustar variables como factores
pest_data <- pest_data %>% mutate_at(vars(periodo_monitoreo,
                                          monitoreo,
                                          tipo_trampa,
                                          marca_trampa,
                                          trampa_ID_unico,
                                          ubicacion,
                                          orden,
                                          familia,
                                          especie), list(factor))

### HYPOTHESIS 1 ###

# Is there a significant association of insect abundance with temperature or RH in the herbarium?

### First, test the effect of maximum temperature and humidity on insect abundance.
full_model_max <- glmer(abundancia_trampa ~ scale(max_humi) + scale(max_temp)
                        + scale(dias_monitoreo)
                        + (1|orden)
                        + (1|trampa_ID_unico),
                        family=poisson,
                        data=pest_data)

# Unscaling results for effect of temperature  on invertebrates
mean(pest_data$max_temp, na.rm=T)
sd(pest_data$max_temp, na.rm=T)
(1*1.164569+24.74459)*0.280872
# for every 1ºC increase, there are on average 7.277157 more insects
(1*1.164569+24.74459)*0.119955
# 3.107933 is the S.E.

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


# test for effect of temperature:
anova(full_model_max, reduced_model_temp)
# Result: NO significant effect of temperature

# test for effect of humidity:
anova(full_model_max, reduced_model_humid)
# Result: NO significant effect of humidity 

### Scatterplots of invertebrate abundance and temp/hum

### Making better scatterplots
# https://www.rforecology.com/post/how-to-make-a-quality-scatterplot-in-r/

# transparent colours function
t_col <- function(color, opacity = 0.5) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], max = 255, alpha = (opacity)*255)
  invisible(t.col)
}

#### Temperature

#quartz(w=5,h=4)
pdf(file="results/scatterplot-all-inv-maxtemp.pdf",width=5,height=4)
par(mar=c(4,5,2,2))
plot(data=pest_data, jitter(abundancia_trampa) ~ jitter(max_temp), log="y",
     xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.5,
     col=t_col("black",0.5))
#add axis
axis(side=1, seq(0, max(pest_data$max_temp)+2, 1), padj=-0.8)
axis(side=2, at=c(1,2,5,10,20,50,100), hadj=0.8, las=2)
#axis(side=2, at=seq(0, 140, 20), hadj=0.8, las=2)
#add axis labels
mtext(side=1, "Maximum Temperature  (ºC)", line=2.8, cex=1, font=2)
mtext(side=2, "Number of Invertebrates\n(log scaled)", line=2.8, cex=1, font=2)
## in the caption, add that the pest data was jittered
dev.off()

#### Humidity

#quartz(w=5,h=4)
pdf(file="results/scatterplot-all-inv-maxrelhum.pdf",width=5,height=4)
par(mar=c(4,5,2,2))
plot(data=pest_data_onlypests, jitter(abundancia_trampa) ~ (max_humi), log="y",
     xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.5,
     col=t_col("black",0.5))
#add axis
axis(side=1, at=seq(0, 70, 5), padj=-0.8)
axis(side=2, at=c(1,2,5,10,20,50), hadj=0.8, las=2)
#add axis labels
mtext(side=1, "Maximum Relative Humidity (%)", line=2.8, cex=1, font=2)
mtext(side=2, "Number of Invertebrates\n(log scaled)", line=2.8, cex=1, font=2)
## in the caption, add that the pest data was jittered
dev.off()

## Now model with ONLY pest species

pest_data_onlypests = filter(pest_data, pest == "potential") # just the pests


pest_full_model_max <- glmer(abundancia_trampa ~ scale(max_humi) + scale(max_temp)
                        + scale(dias_monitoreo)
                        + (1|orden)
                        + (1|trampa_ID_unico),
                        family=poisson,
                        data=pest_data_onlypests)
summary(full_model_max)

# remove temperature variable to test for effect of temperature on only pests

pest_reduced_model_temp <- glmer(abundancia_trampa ~ 
                              scale(max_humi) + 
                                scale(dias_monitoreo) +
                              (1|orden) +
                              (1|trampa_ID_unico),
                            family=poisson,
                            data=pest_data_onlypests)
# Compare models with LRT
anova(pest_full_model_max, pest_reduced_model_temp)
 # Result: Temp does not have a signif. effect on pest abundance (χ2 (1)=1.105, p=0.293)

# test for effect of humidity on only pests

pest_reduced_model_humid <- glmer(abundancia_trampa ~ 
                               scale(max_temp) + 
                                 scale(dias_monitoreo) +
                               (1|orden) +
                               (1|trampa_ID_unico),
                             family=poisson,
                             data=pest_data_onlypests)

anova(pest_full_model_max, pest_reduced_model_humid)
# humidity also does not have an effect on pest abundance


### Result: maximum humidity and maximum temperature were both not significant
# predictors of insect abundance in the herbarium, and they are not even that closely correlated:

#### Scatterplot of temperature and RH
plot(data=pest_data, max_humi ~ max_temp)

plot(data=pest_data_onlypests, abundancia_trampa ~ max_temp, log="y")
plot(data=pest_data_onlypests, abundancia_trampa ~ max_humi, log="y")

# transparent colours function
#t_col <- function(color, opacity = 0.5) {
#  rgb.val <- col2rgb(color)
 # t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], max = 255, alpha = (opacity)*255)
  #invisible(t.col)
#}

#### Temperature

#quartz(w=5,h=4)
pdf(file="results/scatterplot-pests-maxtemp.pdf",width=5,height=4)
par(mar=c(4,5,2,2))
plot(data=pest_data_onlypests, jitter(abundancia_trampa) ~ jitter(max_temp), log="y",
     xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.5,
     col=t_col("black",0.5))
#add axis
axis(side=1, seq(0, max(pest_data_onlypests$max_temp)+2, 1), padj=-0.8)
axis(side=2, at=c(1,2,5,10,20,50), hadj=0.8, las=2)
#add axis labels
mtext(side=1, "Maximum Temperature  (ºC)", line=2.8, cex=1.1, font=2)
mtext(side=2, "Number of Pests\n(log scaled)", line=2.8, cex=1, font=2)
## in the caption, add that the pest data was jittered
dev.off()

#### Humidity

#quartz(w=5,h=4)
pdf(file="results/scatterplot-pests-maxrelhum.pdf",width=5,height=4)
par(mar=c(4,5,2,2))
plot(data=pest_data_onlypests, jitter(abundancia_trampa) ~ (max_humi), log="y",
     xaxt="n", yaxt="n", ylab="", xlab="", pch=16, cex=1.5,
     col=t_col("black",0.5))
 #add axis
axis(side=1, at=seq(0, 70, 5), padj=-0.8)
axis(side=2, at=c(1,2,5,10,20,50), hadj=0.8, las=2)
 #add axis labels
mtext(side=1, "Maximum Relative Humidity (%)", line=2.8, cex=1.1, font=2)
mtext(side=2, "Number of Pests\n(log scaled)", line=2.8, cex=1, font=2)
      ## in the caption, add that the pest data was jittered
dev.off()


### HYPOTHESIS 2 ###
## Did the number of days where temp or RH was higher than the recommendation during a particular
# monitoring period have a significant effect on the number of insects in that monit. period?

### Test the efficacy of the recommended thresholds.
### Next test the effect of sustained periods of temperature or humidity above the recommended maximum of 24ºC
### temperature and above the midpoint recommended value of 50% humidity since tropical herbaria with more 
### insect abundance outdoors in general might need a lower humidity to stay safe than the recommended max of 60%.

# First need to calculate the periods of time above 50% or 24ºC for each monitoring period.
# Do this in the data cleaning script. DONE. So now do basically the exact same analysis as above.


full_model_days <- glmer(abundancia_trampa ~ scale(days_above_50_humi) + scale(days_above_24_temp)
                         + scale(dias_monitoreo)
                         + (1|orden)
                         + (1|trampa_ID_unico),
                         family=poisson,
                         data=pest_data)

summary(full_model_days)
### Result: the total number of days above humidity of 50% and 24C were both not significant
# predictors of insect abundance in the herbarium, and they are not even closely correlated:
plot(data=pest_data, days_above_50_humi ~ days_above_24_temp, cex=abundancia_trampa/10)


reduced_model_days_temp <- glmer(abundancia_trampa ~ scale(days_above_50_humi) 
                                 + scale(dias_monitoreo)
                                 + (1|orden)
                                 + (1|trampa_ID_unico),
                                 family=poisson,
                                 data=pest_data)
summary(reduced_model_days_temp)

anova(full_model_days, reduced_model_days_temp)

# days above temp threshold did NOT affect number of insects significantly (χ2(1)=0.0973, p=0.7551),

reduced_model_days_hum <- glmer(abundancia_trampa ~ scale(days_above_24_temp) 
                                + scale(dias_monitoreo)
                                + (1|orden)
                                + (1|trampa_ID_unico),
                                family=poisson,
                                data=pest_data)
summary(reduced_model_days_hum)

anova(full_model_days, reduced_model_days_hum)
# days above hum threshold did not affect pests significantly (χ2(1)=0.0661, p=0.7971),
### Not including them in the model definitely makes no difference.

## Conclusion: We find support to suggest that at least below the recommended thresholds, there is no 
## association of insect abundance with neither temperature or humidity.

##########
# The same analysis but with only the pests

  # Full model
full_model_days <- glmer(abundancia_trampa ~ scale(days_above_50_humi) + scale(days_above_24_temp)
                         + scale(dias_monitoreo)
                         + (1|orden)
                         + (1|trampa_ID_unico),
                         family=poisson,
                         data=pest_data_onlypests)

summary(full_model_days)

plot(data=pest_data_onlypests, days_above_50_humi ~ days_above_24_temp, cex=abundancia_trampa/10)

  # Reduced model -  temperature
reduced_model_days_temp <- glmer(abundancia_trampa ~ scale(days_above_50_humi) 
                                 + scale(dias_monitoreo)
                                 + (1|orden)
                                 + (1|trampa_ID_unico),
                                 family=poisson,
                                 data=pest_data_onlypests)
summary(reduced_model_days_temp)


anova(full_model_days, reduced_model_days_temp)

# Result: number of days above the temperature limit does not significantly affect pests

# Reduced model -  humidity

reduced_model_days_hum <- glmer(abundancia_trampa ~ scale(days_above_24_temp) 
                                + scale(dias_monitoreo)
                                + (1|orden)
                                + (1|trampa_ID_unico),
                                family=poisson,
                                data=pest_data_onlypests)
summary(reduced_model_days_hum)

anova(full_model_days, reduced_model_days_hum)
# Result: number of days above the humidity limit does not significantly affect pests, either


#####################

### OBJECTIVE 3: How much does external climate correlate with the internal herbarium environment? Is this
# correlation affected by the number of dehumidifiers or AC units running?

clim_dat <- read_csv("data/raw/climate_puerto-ayora.csv")
# remove early years:
clim_dat <- clim_dat[-c(1:15000),]

# load clean herbarium data:
herb_data <- read_csv("data/processed/enviro_data_clean.csv") %>% 
  select(fecha, year, monitoring_interval, herbarium_temp_avg = temp_avg, herbarium_humi_avg = humi_avg)

# combine with outside data:
all_env_data <- left_join(herb_data, clim_dat, by=c("fecha" = "observation_date"))

#### Plot visuals:
names(all_env_data)

## temperature :
plot(herbarium_temp_avg ~ mean_air_temp, data=all_env_data, asp=1)
abline(b=1, a=0)
## humidity:
plot(herbarium_humi_avg ~ humidity, data=all_env_data, asp=1)
# ok, need to remove the outlier values of zero for outdoor humidity:
all_env_data <- filter(all_env_data, humidity > 0)
plot(herbarium_humi_avg ~ humidity, data=all_env_data, asp=1, xlim=c(70,100))
abline(b=1, a=0)

## Correlation tests
#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

## Better plots:

 ## Temperature
ggscatter(all_env_data, x = "mean_air_temp", y = "herbarium_temp_avg", 
          add = "reg.line", add.params= list(color="blue"), conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Air Temperature (ºC)", ylab = "Mean Herbarium Temperature (ºC)")

## Relative humidity
ggscatter(all_env_data, x = "humidity", y = "herbarium_humi_avg", 
          add = "reg.line", add.params= list(color="blue"), conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean Air Relative Humidity (%)", ylab = "Mean Herbarium Relative Humidity (%)")


# temperature
#ggplot(all_env_data, aes(x=mean_air_temp, y=herbarium_temp_avg)) +
  ##print(labs(y="Average Herbarium Temperature (ºC"), x="Mean Air Temperature (ºC)") +
 # geom_point() +
  #simple_theme +
  #geom_smooth(method="lm") +
  #geom_abline(slope=1) +
  ##coord_fixed() +
  #facet_wrap(facets = vars(year))

# humidity
#ggplot(all_env_data, aes(x=humidity, y=herbarium_humi_avg)) +
#  geom_point() +
#  simple_theme +
#  geom_smooth(method="lm") +
#  geom_abline(slope=1) +
 # #coord_fixed() +
  #facet_wrap(facets = vars(year))

dehumids <- c(1,2,4,3)
slope <- c(0,0.2,0.5,0.1)

plot(slope ~ dehumids)

# cor-test for temp

res <- cor.test(all_env_data$mean_air_temp, all_env_data$herbarium_temp_avg, 
                method = "pearson")
res
 # cor-test for RH

res <- cor.test(all_env_data$humidity, all_env_data$herbarium_humi_avg, 
                method = "pearson")
res

### Just need to redo this analysis but using specific intervals based on 
### How many dehumidifiers or ACs were working and when. ACTUALLY,
### better to split the data into small chunks of one month at a time 
### (still around 30 data points each) and for each month determine how
### many ACs were functional and how many de-humidifiers were functional
### (or some sort of index calculated as # dehumidifiers per day). So for
### example, if you have 3 dehumidifiers per day for 30 days, that's 3
### dehumidifiers per day. If you have 3 dehumidifers every other day,
### then that is 1.5 dehumidifiers per day, etc.
### Then make a simple point graph of the relationship between machines
### and the cor coefficient of each period.

### The other important next step is to try the analyses in Objectives 1 and 2
### but only looking at the abundance of insects that are pests.
### Finally, can do one last analysis exactly as done in 1 and 2, but with 
### every one of the major groups of insect taxa to test each one independently.
### This would directly test what Daniel has already done, but results directly
### allow you to say which insect taxa are associated with enviornmental variables.
### Just need to make sure to use a post-hoc correction for the p-value to 
### make sure not to get any false positives when doing so many tests.

### Effectiveness of environmental control in a tropical herbarium


### get a quick summary of the total number of individual bugs per species
pest_data_summ <- group_by(pest_data, especie) %>% 
  summarize(total_bugs = sum(abundancia_trampa, na.rm=T))

pest_data_summ <- group_by(pest_data_onlypests, especie) %>% 
  summarize(total_bugs = sum(abundancia_trampa, na.rm=T))

write.csv(pest_data_summ, file="data/processed/summary_pest_table.csv")

?write.csv
