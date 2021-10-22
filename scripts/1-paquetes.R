
################ Datos Invertebrados CDS #######################

##### Lista de scripts de  paquetes en R usados para el análisis


library(tidyverse) # limpieza de datos y gráficos
library(lme4) # modelaje
#citation("lme4") #para citar el paquete
library(car) # para el análisis con modelos
library(readr) # para abrir archivos csv
library("lubridate")
library(plotrix) #Standard error


 # para los plots
library("ggpubr")
#citation("ggpubr")

### Tema personalizado de ggplot :
simple_theme <- list(
  theme_classic() +
    theme(plot.margin = unit(c(.7,.7,.7,.7), "cm"),
          strip.background = element_rect(color = NA),
          axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 16, margin=ggplot2::margin(t=0.5, unit="cm")),
          axis.title.y = element_text(size = 16, margin=ggplot2::margin(r=0.5, unit="cm")),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13),
          plot.title = element_text(size = 16),
          axis.line=element_line(),
          panel.spacing = unit(1, "lines"),
          strip.text.x = element_text(size = 18, colour = "black", angle = 0)),
  
  #scale_color_manual(values= c("darkgrey","black")),
  scale_shape_manual(values=c(17,16))
)
