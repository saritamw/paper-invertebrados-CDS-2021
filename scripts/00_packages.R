################ Insect Data CDRS #######################

##### List of packages used for analysis
### Package scripts


library(tidyverse) #For data cleaning and graphs
library(lme4) #For modeling
#citation("lme4")
library(car) #For model analysis
library(readr) #For reading csv files
library("lubridate")
library(plotrix) #Standard error


#For CA 
library(ade4)
library(vegan)
library(gclus)
library(ape)
library(missMDA)
library(FactoMineR)

 # for plots
library("ggpubr")
#citation("ggpubr")

### Custom ggplot theme:
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
