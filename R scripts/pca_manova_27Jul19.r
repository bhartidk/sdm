setwd("D:/PhD data/Analysis/Chapter2_2Nov17")
library(ggplot2)
library(devtools)
library(plyr)
library(dplyr)
library("easyGgplot2")
library(RVAideMemoire)
library(reshape2)
library(ggbiplot)
library(scales)
library(grid)
library(rJava)
library(raster)
library(sp)
library(dismo)

#install.packages("RVAideMemoire", dependencies=TRUE)
#install_github("easyGgplot2", "kassambara")

#reading in data for the Indian coastline
var.data<-read.csv("dataframes/varPlot_in_9Mar18.csv")

#looking at data summaries
head(var.data)
str(var.data)

#taking only the variables that turn out to be significant in determining bengalensis and carinifera distributions. The variables are bathymetry, bathymetric slope, maximum SSS
var.data2<-var.data[,c(1,7,10,11)]

#making sure the right variables have been downloaded
colnames(var.data2)

#making sure the environmental variable columns are numeric
str(var.data2)

#plotting all the variables to see what they look like
d <- melt(var.data2[,-1])
ggplot(d,aes(x = value)) +  facet_wrap(~variable,scales = "free_x") + 
geom_histogram()

#looking at a scatter plot of 
plot(var.data2[,2], var.data2[,3])
#seems non-linear
plot(var.data2[,2], var.data2[,4])
plot(var.data2[,3], var.data2[,4])
#seems non-linear

#checking if the variables are normally distributed - apparently it is very rarely that distributions are found to be normally distributed - and some resources I came across said that most parametric tests are immune to some deviations from normality - so choosing to ignore the mshapiro.test results obtained below
RVAideMemoire::mshapiro.test(var.data2[,-1])
#non-normal data

## PCA
colnames(var.data2)
sp.ben<-c("bengalensis", "carinifera")
var.dataPCA<-var.data2[var.data2$species %in% sp.ben, ]
fit<-prcomp(var.dataPCA[,-1], center = TRUE, scale. = TRUE)

summary(fit)
plot(fit, type='l')

#Looking at the loadings on each PC axis
print(fit)

pc.df<-data.frame(fit$x)
pc.df$species<-var.dataPCA$species

#Plotting PC1 against PC2 and colouring points based on tidal range classes

#Code below from - http://ggplot2.tidyverse.org/reference/stat_ellipse.html
jpeg(file=paste0("results_figures/pca_und_str2.jpeg"),  height=10, width = 10, units = 'in', res=300)
ggdata<-data.frame(fit$x)
ggplot(ggdata, aes(PC1, PC2, color = var.dataPCA$species)) + geom_point() + stat_ellipse()
dev.off()

#Code below from - https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
jpeg(file=paste0("results_figures/pca_ben_car_27Jul19.jpeg"),  height=10, width = 10, units = 'in', res=300)
g <- ggbiplot(fit, obs.scale = 1, var.scale = 1, groups = var.dataPCA$species, ellipse = TRUE, circle = FALSE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
dev.off()

#interpretation of arrows on the biplot - "The arrows represent the correlation of the variables with PC1 and PC2. The white circle indicates the theoretical maximum extent of the arrows. The ellipses are 68% data ellipses" - from https://stats.stackexchange.com/questions/7860/visualizing-a-million-pca-edition

#some more info on the loadings in a biplot https://stats.stackexchange.com/questions/119746/what-is-the-proper-association-measure-of-a-variable-with-a-pca-component-on-a

#useful link - http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

#running statistical tests with PC1 and PC2 and the effect of species identity on them
wilcox.test(PC1~ species, data=pc.df) 
wilcox.test(PC2 ~ species, data=pc.df) 

#running the analysis with the variables themselves
colnames(var.dataPCA)
wilcox.test(sss_saltiest_month_psu~species, data=var.dataPCA) 
wilcox.test(topo_bath~species, data=var.dataPCA) 
wilcox.test(topo_bath_slope ~ species, data=var.dataPCA) 

median(var.dataPCA[var.dataPCA$species=="bengalensis", 2])
median(var.dataPCA[var.dataPCA$species=="carinifera", 2])

