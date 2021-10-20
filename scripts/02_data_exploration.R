################ Datos de insectos de CDRS #######################

### Data exploration of insect dataset and environmental variables

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

names(pest_data)
head(pest_data)
str(pest_data)


#####################
### OBJECTIVE 1: is there an association of humidity and temperature with insect abundance?


############
### First, test the effect of maximum temperature and humidity.

pest_data = pest_data # original analysis
pest_data = filter(pest_data, pest == "yes") # just the pests


full_model_max <- glmer(abundancia_trampa ~ scale(max_humi) + scale(max_temp)
                        + (1|orden)
                        + (1|trampa_ID_unico),
                        family=poisson,
                        data=pest_data)
summary(full_model_max)
### Result: maximum humidity and maximum temperature were both not significant
# predictors of insect abundance in the herbarium, and they are not even that closely correlated:
plot(data=pest_data, max_humi ~ max_temp)

reduced_model <- glmer(abundancia_trampa ~
                         (1|orden)
                       + (1|trampa_ID_unico),
                       family=poisson,
                       data=pest_data)
summary(reduced_model)
anova(full_model_max, reduced_model)
### Additional Results, completely excluding max temperature and humidity from 
# the model has no effect (i.e., they really are not important).


#####################
### OBJECTIVE 2: test the efficacy of the recommended thresholds.
### Next test the effect of sustained periods of temperature or humidity above the recommended maximum of 24ºC
### temperature and above the midpoint recommended value of 50% humidity since tropical herbaria with more 
### insect abundance outdoors in general might need a lower humidity to stay safe than the recommended max of 60%.

# First need to calculate the periods of time above 50% or 24ºC for each monitoring period.
# Do this in the data cleaning script. DONE. So now do basically the exact same analysis as above.

# QUESTION: would it be better instead of analyzing overall insect abundance to analyze the abundance
# of strictly those insect taxa that might be pests or are related to pests?

full_model_days <- glmer(abundancia_trampa ~ scale(days_above_50_humi) + scale(days_above_24_temp)
                         + (1|orden)
                         + (1|trampa_ID_unico),
                         family=poisson,
                         data=pest_data)

summary(full_model_days)
### Result: the total number of days above humidity of 50% and 24C were both not significant
# predictors of insect abundance in the herbarium, and they are not even closely correlated:
plot(data=pest_data, days_above_50_humi ~ days_above_24_temp, cex=abundancia_trampa/10)

pest_data$abundancia_trampa

reduced_model <- glmer(abundancia_trampa ~
                         (1|orden)
                       + (1|trampa_ID_unico),
                       family=poisson,
                       data=pest_data)
summary(reduced_model)
anova(full_model_days, reduced_model)
### Not including them in the model definitely makes no difference.

## Conclusion: Yes, we find support to suggest that at least below the recommended thresholds, there is no 
## association of insect abundance with temperature and humidity.

#####################
### OBJECTIVE 3: How does external climate correlate with the internal herbarium environment? Does it matter
### how many AC units or dehumidifiers are running?
clim_dat <- read_csv("data/raw/climate_puerto-ayora.csv")
# remove early years:
clim_dat <- clim_dat[-c(1:15000),]

# load clean herbarium data:
herb_data <- read_csv("data/processed/enviro_data_clean.csv") %>% 
  select(fecha, year, monitoring_interval, herbarium_temp_avg = temp_avg, herbarium_humi_avg = humi_avg)

# combine with outside data:
all_env_data <- left_join(herb_data, clim_dat, by=c("fecha" = "observation_date"))

# Plot visuals:
names(all_env_data)
# Temperature:
plot(herbarium_temp_avg ~ mean_air_temp, data=all_env_data, asp=1)
abline(b=1, a=0)
# humidity:
plot(herbarium_humi_avg ~ humidity, data=all_env_data, asp=1)
# ok, need to remove the outlier values of zero for outdoor humidity:
all_env_data <- filter(all_env_data, humidity > 0)
plot(herbarium_humi_avg ~ humidity, data=all_env_data, asp=1, xlim=c(70,100))
abline(b=1, a=0)


## Better plots:
ggplot(all_env_data, aes(x=mean_air_temp, y=herbarium_temp_avg)) +
  geom_point() +
  simple_theme +
  geom_smooth(method="lm") +
  geom_abline(slope=1) +
  #coord_fixed() +
  facet_wrap(facets = vars(year))

ggplot(all_env_data, aes(x=humidity, y=herbarium_humi_avg)) +
  geom_point() +
  simple_theme +
  geom_smooth(method="lm") +
  geom_abline(slope=1) +
  #coord_fixed() +
  facet_wrap(facets = vars(year))

dehumids <- c(1,2,4,3)
slope <- c(0,0.2,0.5,0.1)

plot(slope ~ dehumids)

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

