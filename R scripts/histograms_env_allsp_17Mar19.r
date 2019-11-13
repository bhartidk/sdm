setwd("D:/PhD data/Analysis/Chapter2_2Nov17")
library(ggplot2)
library(devtools)
library(dplyr)
library("easyGgplot2")
library(RVAideMemoire)
library(reshape2)
library(ggbiplot)
library(plyr)
library(scales)
library(grid)
library(rJava)
library(raster)
library(sp)
library(dismo)
library(Hmisc)
library(lattice)

#install.packages("RVAideMemoire", dependencies=TRUE)
#install_github("easyGgplot2", "kassambara")

#reading in data
var.data<-read.csv("dataframes/all_species_env_29Nov18.csv")
#var.data<-read.csv("results_ip/varPlot_ip_9Mar18.csv")

#plotting all the variables to see what they look like
d <- melt(var.data[,-c(1:4)])
ggplot(d,aes(x = value)) +  facet_wrap(~variable,scales = "free_x") + 
geom_histogram()

#creating two new dataframes each for mangrove and rocky shore species
man<-c("bengalensis", "carinifera", "strigata")
rocky<-c("undulata", "leucosticta", "omanensis", "vidua", "strigata", "malaccana")

var.data.man<-var.data[var.data$species %in% man,]
var.data.man$species<-factor(var.data.man$species, levels=c('bengalensis', 'carinifera', 'strigata'), ordered=TRUE)

var.data.rocky<-var.data[var.data$species %in% rocky,]
var.data.rocky$species<-factor(var.data.rocky$species, levels=c('strigata', 'undulata', 'leucosticta', 'vidua', 'malaccana', 'omanensis'), ordered=TRUE)

#plotting the ranges of environmental variables for each of these species
##Rocky shore##

jpeg(file="results_figures/air_temp_rocky.jpeg",  height=10, width = 15, units = 'in', res=300)
ggplot(var.data.rocky,aes(x=air_temp_dryQ_C ))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Air temp of the driest quarter (deg C)", y ="Count")
dev.off()

jpeg(file="results_figures/max_air_temp_rocky.jpeg",  height=10, width = 15, units = 'in', res=300)
ggplot(var.data.rocky,aes(x=max_air_temp_C ))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Maximum air temperature (deg C)", y ="Count")
dev.off()

jpeg(file="results_figures/mean_sst.jpeg",  height=10, width = 15, units = 'in', res=300)
ggplot(var.data.rocky,aes(x=mean_annual_sst_C ))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Mean SST(deg C)", y ="Count")
dev.off()

jpeg(file="results_figures/pp_mean.jpeg",  height=10, width = 15, units = 'in', res=300)
ggplot(var.data.rocky,aes(x=pp_mean))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Mean primary productivity(g.m-3.day-1)", y ="Count")
dev.off()

jpeg(file="results_figures/sss_max.jpeg",  height=10, width = 15, units = 'in', res=300)
ggplot(var.data.rocky,aes(x=sss_saltiest_month_psu))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Sea surface salinity of saltiest month(psu)", y ="Count")
dev.off()

jpeg(file="results_figures/sst_max.jpeg",  height=10, width = 15, units = 'in', res=300)
ggplot(var.data.rocky,aes(x=sst_warmest_month_C))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Sea surface temperature of warmest month(deg C)", y ="Count")
dev.off()

jpeg(file="results_figures/tidal_range.jpeg",  height=10, width = 15, units = 'in', res=300)
ggplot(var.data.rocky,aes(x=tidal_range_minmax_fill_m))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Tidal range(m)", y ="Count")
dev.off()

jpeg(file="results_figures/topo_bath.jpeg",  height=10, width = 15, units = 'in', res=300)
ggplot(var.data.rocky,aes(x=topo_bath))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Bathymetry(m)", y ="Count")
dev.off()

jpeg(file="results_figures/topo_bath_slope.jpeg",  height=10, width = 15, units = 'in', res=300)
ggplot(var.data.rocky,aes(x=topo_bath_slope))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Bathymetric slope(degrees)", y ="Count")
dev.off()

##Mangrove species##

jpeg(file="results_figures/air_temp_man.jpeg",  height=5, width = 15, units = 'in', res=300)
ggplot(var.data.man,aes(x=air_temp_dryQ_C ))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Air temp of the driest quarter (deg C)", y ="Count")
dev.off()

jpeg(file="results_figures/max_air_temp_man.jpeg",  height=5, width = 15, units = 'in', res=300)
ggplot(var.data.man,aes(x=max_air_temp_C ))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Maximum air temperature (deg C)", y ="Count")
dev.off()

jpeg(file="results_figures/mean_sst_man.jpeg",  height=5, width = 15, units = 'in', res=300)
ggplot(var.data.man,aes(x=mean_annual_sst_C ))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Mean SST(deg C)", y ="Count")
dev.off()

jpeg(file="results_figures/pp_mean_man.jpeg",  height=5, width = 15, units = 'in', res=300)
ggplot(var.data.man,aes(x=pp_mean))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Mean primary productivity(g.m-3.day-1)", y ="Count")
dev.off()

jpeg(file="results_figures/sss_max_man.jpeg",  height=5, width = 15, units = 'in', res=300)
ggplot(var.data.man,aes(x=sss_saltiest_month_psu))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Sea surface salinity of saltiest month(psu)", y ="Count")
dev.off()

jpeg(file="results_figures/sst_max_man.jpeg",  height=5, width = 15, units = 'in', res=300)
ggplot(var.data.man,aes(x=sst_warmest_month_C))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Sea surface temperature of warmest month(deg C)", y ="Count")
dev.off()

jpeg(file="results_figures/tidal_range_man.jpeg",  height=5, width = 15, units = 'in', res=300)
ggplot(var.data.man,aes(x=tidal_range_minmax_fill_m))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Tidal range(m)", y ="Count")
dev.off()

jpeg(file="results_figures/topo_bath_man.jpeg",  height=5, width = 15, units = 'in', res=300)
ggplot(var.data.man,aes(x=topo_bath))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Bathymetry(m)", y ="Count")
dev.off()

jpeg(file="results_figures/topo_bath_slope_man.jpeg",  height=5, width = 15, units = 'in', res=300)
ggplot(var.data.man,aes(x=topo_bath_slope))+geom_histogram()+facet_wrap(~species)+theme_bw() + theme(strip.text.x=element_text(size=16), axis.text.x=element_text(size=16), axis.text.y=element_text(size=16), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16)) + labs(x= "Bathymetric slope(degrees)", y ="Count")
dev.off()

