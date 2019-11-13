#rm(list = ls())
setwd("C:/Bharti/PhD data/Analysis/Chapter2_2Nov17")
library(sdmpredictors)
library(raster)
library(sp)
library(rgdal)
library(maps)
library(rgeos)
library(dismo)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(devtools)
library(digest)
library(rJava)
library(geosphere)
library(stringr)
library(ncdf4)
library(sf)
library(reshape2)
library(ENMeval)
library(parallel)
library(devtools)
library(rmaxent)
library(MASS)
#library(foreach)
#library(iterators)
#library(doParallel)
#install_github('johnbaums/rmaxent')

#ip.ext<-c(24, 150,-36,30.21667)
#ms_layer<-raster("sdm_layers/marspec/ip_ext/mean_annual_sss_psu.tif")
#projection(ms_layer)<-wgs
#ext<-ip.ext

##input occurrence data
all.occ<-read.csv("dataframes/allsites_env_4Nov17.csv")
head(all.occ)
colnames(all.occ)

#removing the row number column and all the extracted data columns from the dataframe 
all.occ<-all.occ[,-c(1,7:10, 12:38)]
head(all.occ)

#changing the order of longitude and latitude in the dataframe - easier to handle rasters that way
all.occ<-all.occ[,c(2,5,4,3,6)]
head(all.occ)

#creating a dataframe which has unique values of all.occ
rn<-as.numeric(rownames(unique(all.occ[,2:3])))
unq.occ<-all.occ[rn,]

#adding other sampling locations where no species were found but were a part of the sampling effort to this dataframe
no.occ<-read.csv("dataframes/coords_nosp_12Jan18.csv")
unq.occ<-rbind(unq.occ, no.occ)
#write.csv(unq.occ, file="dataframes/unq_occ.csv", row.names=TRUE)

#converting this into a SpatialPoints dataframe
#coordinates(unq.occ)<-~long+lat

#Creating a variable which saves the WGS projection codes
wgs<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#projection(unq.occ)<-wgs

#rbinding all.occ and no.occ
all.occ<-rbind(all.occ, no.occ)

#extracting occurrence data for each species and saving it as a separate dataframe
sp<-as.character(levels(all.occ$species))

#input the extended occurrence data from Reid and Reid referred publications
ip.occ<-read.csv("dataframes/species_locations_25Jan18.csv", fileEncoding="latin1")
head(ip.occ)

#looking at if the columns have been given correct assignments
str(ip.occ)
ip.occ$lat<-as.numeric(as.character(ip.occ$lat))

#removing an extra column that has for some reason been added in the end
ip.occ<-ip.occ[,-ncol(ip.occ)]
head(ip.occ)

#removing locations from India
colnames(ip.occ)
ip.occ<-ip.occ[ip.occ$location5!="India", c(2,12,13,6,15)]
 
#retaining only those rows where there are no NAs - not that we have reduced the number of columns used, the only columns that can give NA values are long and lat
ip.occ<-ip.occ[complete.cases(ip.occ),]

#changing the column names to match all.occ, so that I can rbind them together
colnames(ip.occ)
colnames(ip.occ)<-colnames(all.occ)
head(ip.occ)

#merging ip.occ with all.occ
ip.occ<-rbind(all.occ, ip.occ)

#creating a list where each element is a species specific dataframe
ip.ind.occ<-list()
for(i in 1:length(sp)){
ip.ind.occ[[i]]<-subset(ip.occ, ip.occ$species %in% sp[i])
names(ip.ind.occ)[[i]]<-paste0("ip.", substr(sp[i], 1, 3), ".occ") 
write.csv(ip.ind.occ[[i]], file=paste0("dataframes/", "ip_", substr(sp[i], 1, 3), "_occ.csv"), row.names=TRUE)
}

#dissolving the list to create individual dataframes that have been appropriately labeled
list2env(ip.ind.occ, envir=.GlobalEnv)
ls()

#finding the minimum and maximum lat and long values
pt.ext<-c(min(ip.occ$long), max(ip.occ$long), min(ip.occ$lat), max(ip.occ$lat))

#trying to see how the extent is like by cropping the world coastline extent to this and plotting species points over it
coastline<-readOGR(dsn='sdm_layers/CS/ne_10m_coastline', layer='ne_10m_coastline')
crop.coastline<-crop(coastline, pt.ext)

##Loading 200 m bathymetry shapefile
shelf<-readOGR(dsn='sdm_layers/CS/ne_200m_bathymetry', layer='ne_10m_bathymetry_K_200')

plot(crop.coastline)
points(ip.und.occ[,2:3], col="brown")
points(ip.pal.occ[,2:3], col="green")

#From the above plot it appears that undulata and pallescens go into the Pacific islands as well. Since I do not have tidal range information for those locations I cannot use those regions. Therefore can use the longitudinal extent of tidal_range layer to determine the horizontal extent and the spread of points (ip.occ) to determine the vertical extent

##Downloading tidal range information in netCDF and converting it into a raster
#I downloaded netCDF data corresponding to Indian Ocean ATLAS 2011 at 1/12 degree resolution from the OTIS website. It has variables associated with latitude, longitude, h (amplitude) and phase information for 11 different tidal harmonic constitutents. This was input into TMD toolbox script code which was modified with help from Ivan Haigh's trimmed down code with changes to save the output as a map and not as a time series per location in the lat long grid. This was run every month of the year 2016 between 1-15 (15 days) so that a spring and neap tide is considered every month. In the code below I am finding the average of netCDF files obtained for each month to get the annual average.

#Opening and dealing with netCDF data - http://environmentalcomputing.net/opening-netcdf-files/
#Opening a netCDF file
tidalAmp<-nc_open("sdm_layers/tide/IO_2011atlas_netcdf/DATA/hf.IO_2011atlas.nc")

#Looking at the constituents of the netCDF file
print(tidalAmp)
#It has 7 variables, in 4 dimensions

#Obtaining some variables we are interested in
#Tidal elevation amplitude at Z-nodes
tidalAmp.ha <- ncvar_get(tidalAmp,"ha")

#Latitude of Z nodes
tidalAmp.lat<-ncvar_get(tidalAmp, "lat_z")

#Longitude of Z nodes
tidalAmp.lon<-ncvar_get(tidalAmp, "lon_z")

#Tidal constituents 
tidalAmp.con<-ncvar_get(tidalAmp, "con")

#Looking at the dimensions of each of the variables obtained
dim(tidalAmp.ha)

#The above has the same dimensions as tidalAmp.lat and lon, but has another dimension of size 13 - which refers to each of the tidal components listed in tidalAmp.con

dim(tidalAmp.lat)
dim(tidalAmp.lon)
dim(tidalAmp.con)

#Looking at the values for tidal elevation amplitude, but for tidal constituent m2 which is con=1
tidalAmp.ha[,,1] 

#What fill value is used for missing data - no fill value specified here
ncatt_get(tidalAmp, "ha", "_FillValue")

#Alternate method of obtaining fill value information
ncatt_get(tidalAmp, "ha", "_missing_value")

#This netCDF file does not have attribute data associated with the coordinate reference system used.

#Closing the netCDF file - they say this is IMPORTANT to do
nc_close(tidalAmp)

#Opening the tidal range netcdf file generated from TMD toolbox - there should be one netcdf file for each month of the year. I need to take the mean of all months put together

#List file names in a path, following a certain pattern
files<-list.files(path="sdm_layers/tide/tide_range_MATLAB/FUNCTIONS/mean", pattern="*.nc")

files.min<-list.files(path="sdm_layers/tide/tide_range_MATLAB/FUNCTIONS/min", pattern="*.nc")

files.max<-list.files(path="sdm_layers/tide/tide_range_MATLAB/FUNCTIONS/max", pattern="*.nc")

#Creating a stack called tidalRange where the netCDF to raster converted files will be stored as a stack
tidalRange<-stack()

#Opening each netCDF file, converting to raster, assigning the extent and the CRS for each and adding it to the stack

#The extent is assigned from looking at the min and max of the tidalAmp.lon and tidalAmp.lat matrices. The tidal range netCDF is derived from the tidalAmp.ha and tidalAmp.hp files - therefore the lat longs asociated with these should be the same for tidal range. The resolution of the netCDF file is 1/12 degrees

for(i in 1:length(files)){
tidalRange_nc<-nc_open(paste0("sdm_layers/tide/tide_range_MATLAB/FUNCTIONS/mean/", files[i]))
tidalRange.r<-ncvar_get(tidalRange_nc, 'tidal_range_mean')
tidalRange.r<-raster(tidalRange.r)
tidalRange.r[tidalRange.r==0]<-NA
extent(tidalRange.r)<-c(min(tidalAmp.lon), max(tidalAmp.lon), min(tidalAmp.lat), max(tidalAmp.lat))
crs(tidalRange.r)<-wgs
tidalRange<-stack(tidalRange, tidalRange.r)
nc_close(tidalRange_nc)
}

tidalRange

#Taking the mean of all rasters
tidalRange.r<-mean(tidalRange)

tidalRange.r<-flip(tidalRange.r, direction='y')
plot(tidalRange.r)

#Re-sampling it to match the resolution of ms_layers
#tidalRange.r<-resample(tidalRange.r, ms_layer)

#Setting min and max
tidalRange.r<-setMinMax(tidalRange.r)

#Saving the raster to disk
writeRaster(tidalRange.r, filename="sdm_layers/tidalRange/tidal_range_ip", format="GTiff", overwrite=TRUE)

par(mfrow=c(3,1))
plot(crop.coastline)
plot(tidalRange.r, add=TRUE) 
points(ip.und.occ[,2:3], pch=19, col="blue")
plot(crop.coastline)
plot(tidalRange.r, add=TRUE) 
points(ip.pal.occ[,2:3], pch=19, col="blue")
plot(crop.coastline)
plot(tidalRange.r, add=TRUE) 
points(ip.vid.occ[,2:3], pch=19, col="blue")

#tidalRange.r counting the number of NAs from extracted values for presence points=4
length(which(is.na(extract(tidalRange.r, unique(ip.occ[,2:3])))))
nrow(ip.occ)

#Filling NAs with mean of neighbouting 8 values
reps<-4
tidalRange.fill<-tidalRange.r
for(i in 1:reps){
tidalRange.fill<-focal(tidalRange.fill, w=matrix(1,3,3),  fun=mean, pad=TRUE, na.rm=TRUE, NAonly=TRUE)
}

#Checking if the NAs have been removed
length(which(is.na(extract(tidalRange.fill, unique(all.occ[,2:3])))))

#Saving the raster to disk
writeRaster(tidalRange.fill, filename="sdm_layers/tidalRange/tidal_range_fill_ip", format="GTiff", overwrite=TRUE)

##
#running the same set of functions for min and max

#
tidalMin<-stack()
for(i in 1:length(files.min)){
tidalMin_nc<-nc_open(paste0("sdm_layers/tide/tide_range_MATLAB/FUNCTIONS/min/", files.min[i]))
tidalMin.r<-ncvar_get(tidalMin_nc, 'tidal_min')
tidalMin.r<-raster(tidalMin.r)
tidalMin.r[tidalMin.r==0]<-NA
extent(tidalMin.r)<-c(min(tidalAmp.lon), max(tidalAmp.lon), min(tidalAmp.lat), max(tidalAmp.lat))
crs(tidalMin.r)<-wgs
tidalMin<-stack(tidalMin, tidalMin.r)
nc_close(tidalMin_nc)
}

tidalMax<-stack()
for(i in 1:length(files.max)){
tidalMax_nc<-nc_open(paste0("sdm_layers/tide/tide_range_MATLAB/FUNCTIONS/max/", files.max[i]))
tidalMax.r<-ncvar_get(tidalMax_nc, 'tidal_max')
tidalMax.r<-raster(tidalMax.r)
tidalMax.r[tidalMax.r==0]<-NA
extent(tidalMax.r)<-c(min(tidalAmp.lon), max(tidalAmp.lon), min(tidalAmp.lat), max(tidalAmp.lat))
crs(tidalMax.r)<-wgs
tidalMax<-stack(tidalMax, tidalMax.r)
nc_close(tidalMax_nc)
}

#Taking the min and max of all rasters
tidalMin.r<-setMinMax(tidalMin.r)
tidalMax.r<-setMinMax(tidalMax.r)

tidalMin.r<-min(tidalMin)
tidalMax.r<-max(tidalMax)

tidalMin.r<-flip(tidalMin.r, direction='y')
tidalMax.r<-flip(tidalMax.r, direction='y')

#Taking the difference between the two to obtain range
tidalRange2.r<-tidalMax.r-tidalMin.r

#writing it to disk
writeRaster(tidalRange2.r, filename="sdm_layers/tidalRange/tidal_range_minmax_ip", format="GTiff", overwrite=TRUE)

#Resampling to ms_layers extent and resolution
#tidalRange2.r<-resample(tidalRange2.r, ms_layer)

length(which(is.na(extract(tidalRange2.r, unique(ip.occ[,2:3])))))

#Filling NAs with mean of neighbouting 8 values
reps<-4
tidalRange2.fill<-tidalRange2.r
for(i in 1:reps){
tidalRange2.fill<-focal(tidalRange2.fill, w=matrix(1,3,3),  fun=mean, pad=TRUE, na.rm=TRUE, NAonly=TRUE)
}

#Checking if the NAs have been removed
length(which(is.na(extract(tidalRange2.fill, unique(all.occ[,2:3])))))

#set min max
tidalRange2.fill<-setMinMax(tidalRange2.fill)
extent(tidalRange2.fill)

#Saving the raster to disk
writeRaster(tidalRange2.fill, filename="sdm_layers/tidalRange/tidal_range_minmax_fill_ip", format="GTiff", overwrite=TRUE)

##Before starting to handle rasters and shape files, define extents of area of interest. The -36 comes from rounding off the ymin from looking at the sampling locations
ip.ext<-extent(xmin(tidalRange2.fill), xmax(tidalRange2.fill), -36, ymax(tidalRange2.fill))

#checking if the extent looks okay, by plotting points over it
crop.coastline<-crop(coastline, ip.ext)
tidalRange2.fill<-crop(tidalRange2.fill, ip.ext)
plot(tidalRange2.fill)
plot(crop.coastline, add=TRUE)
points(ip.occ[,2:3], col="blue", pch=19)

#retaining only those points which fall into ip.extent
ext.pol<-as(ip.ext, "SpatialPolygons")
projection(ext.pol)<-wgs

ip.occ.crop<-ip.occ
coordinates(ip.occ.crop)<-~long+lat
projection(ip.occ.crop)<-wgs

within<-over(ip.occ.crop, ext.pol)
table(within)

ip.occ.crop<-ip.occ.crop[!(is.na(within)),]

#checking to see if the points have been removed
plot(coastline)
plot(tidalRange2.fill, add=TRUE)
points(ip.occ.crop[,2:3], pch=19, col="blue")

#dividing these cropped points species-wise, again
ip.ind.occ<-list()
for(i in 1:length(sp)){
ip.ind.occ[[i]]<-subset(ip.occ.crop, ip.occ.crop$species %in% sp[i])
names(ip.ind.occ)[[i]]<-paste0("ip.", substr(sp[i], 1, 3), ".occ") 
write.csv(ip.ind.occ[[i]], file=paste0("dataframes/", "ip_", substr(sp[i], 1, 3), "_occ.csv"), row.names=FALSE)
}

#dissolving the list to create individual dataframes that have been appropriately labeled
list2env(ip.ind.occ, envir=.GlobalEnv)
ls()

#seeting ip.ext as the extent
ext<-ip.ext

#removing unrequired variables
rm(shelf, tidalAmp, tidalAmp.lat, tidalAmp.lon, tidalMax, tidalMax_nc, tidalMax.r, tidalMin, tidalMin_nc, tidalMin.r, tidalRange, tidalRange_nc, tidalRange.r, tidalRange2.r, tidalAmp.con, tidalAmp.ha)

##Downloaing raster layers 
#MARSPEC 1km layers

ms<-list.files(path="sdm_layers/marspec", pattern="*.tif")

#The file is input, cropped to the required extent and put in the stack. 

ms_layers<-stack()
for(i in 1:length(ms)){
ms.raster<-raster(paste0("sdm_layers/marspec/", ms[i]))
ms.raster<-crop(ms.raster, ext)
ms_layers<-stack(ms_layers, ms.raster)
}

#Giving the layers meaningful names
names(ms_layers)
names(ms_layers)[1:nlayers(ms_layers)]<-c("bathy_m", "dist_to_shore_km", "bathy_slope_deg", "mean_annual_sss_psu", "sss_freshest_month_psu", "sss_saltiest_month_psu", "mean_annual_sst_C", "sst_coldest_month_C", "sst_warmest_month_C" )

ms_layers
#The scaling has to be adjusted - the scaling factor info was taken from MARSPEC meta-data, also available here - http://marspec.weebly.com/modern-data.html
#bathymetric slope
names(ms_layers[[3]])
ms_layers[[3]]<-ms_layers[[3]]/10

#salinity
names(ms_layers[[4:6]])
ms_layers[[4]]<-ms_layers[[4]]/100
ms_layers[[5]]<-ms_layers[[5]]/100
ms_layers[[6]]<-ms_layers[[6]]/100

#temperature
names(ms_layers[[7:9]])
ms_layers[[7]]<-ms_layers[[7]]/100
ms_layers[[8]]<-ms_layers[[8]]/100
ms_layers[[9]]<-ms_layers[[9]]/100

#Setting min and max
ms_layers<-setMinMax(ms_layers)

#Saving the unedited rasters to disk
#setwd("sdm_layers/marspec/ip_ext_unedited")
writeRaster(ms_layers, filename=names(ms_layers), bylayer=TRUE,format="GTiff")
#setwd("D:/PhD data/Analysis/Chapter2_2Nov17")

#Plot to check the scale and the general appearance
plot(ms_layers)

#MARSPEC counting number of NAs in extracted value from presence points = 126
length(which(is.na(extract(ms_layers[[2]], unique(all.occ[,2:3])))))

#Correcting for NAs near the coastline. For each NA, taking the mean of the 8 cells surrounding it and averaging them to get the central cell value. It takes 3 reps to fills all NAs from extracted values in all.occ - ran it with a test data-set. Help from https://stat.ethz.ch/pipermail/r-sig-geo/2013-July/018709.html
reps<-3
ms_layers_fill<-ms_layers

for(i in 1:nlayers(ms_layers_fill)){
for(j in 1:reps){
ms_layers_fill[[i]]<-focal(ms_layers_fill[[i]], w=matrix(1,3,3), fun=mean, na.rm=TRUE, pad=TRUE, NAonly=TRUE)
}
}

#The layer names are removed for some reason when I run the above code.
names(ms_layers_fill)<-names(ms_layers)

#Checking if all NA's from presence points have been filled
length(which(is.na(extract(ms_layers_fill[[1]], unique(all.occ[,2:3])))))
#rm(ms_layers)

#Saving the cropped rasters to disk
#setwd("sdm_layers/marspec/wg_ext")
writeRaster(ms_layers_fill, filename=names(ms_layers_fill), bylayer=TRUE,format="GTiff", overwrite=TRUE)
#setwd("D:/PhD data/Analysis/Chapter2_2Nov17")

##WorldClim 1km layers
wc<-list.files(path="sdm_layers/worldclim", pattern="*.tif")

#The file is input, cropped to the required extent and put in the stack. 

wc_layers<-stack()
for(i in 1:length(wc)){
wc.raster<-raster(paste0("sdm_layers/worldclim/", wc[i]))
wc.raster<-crop(wc.raster, ext)
wc_layers<-stack(wc_layers, wc.raster)
}

#Giving the layers meaningful names
names(wc_layers)
names(wc_layers)<-c("mean_air_temp_C" , "max_air_temp_C", "min_air_temp_C", "air_temp_dryQ_C")

#Setting min and max
wc_layers<-setMinMax(wc_layers)

plot(wc_layers)

#Exporting unedited cropped raster to disk. Parallel processing code from https://stackoverflow.com/questions/43243611/writeraster-to-netcdf-parallelisation-r
#UseCores<-detectCores()-1
#cl<- makeCluster(UseCores, type="FORK")
#registerDoParallel(cl)
#tmp<-foreach(i = 1:nlayers(wc_layers)) %dopar% 
#{  
#r<-raster::raster(wc_layers, i)
#raster::writeRaster(r, filename=names(wc_layers)[i],overwrite=TRUE, #format="GTiff")
#rm(r)
#}
#stopCluster(cl)   

setwd("sdm_layers/worldclim/wg_ext_unedited")
writeRaster(wc_layers, filename=names(wc_layers), bylayer=TRUE,format="GTiff", overwrite=TRUE)
setwd("D:/PhD data/Analysis/Chapter2_2Nov17")

#WorldClim counting number of NAs in extracted value from presence points=90
length(which(is.na(extract(wc_layers[[1]], unique(all.occ[,2:3])))))

#Correcting for NAs near the coastline. For each NA, taking the mean of the 8 cells surrounding it and averaging them to get the central cell value. It takes 2 reps to fills all NAs from extracted values in all.occ - ran it with a test data-set. Help from https://stat.ethz.ch/pipermail/r-sig-geo/2013-July/018709.html
reps<-2
wc_layers_fill<-wc_layers

for(i in 1:nlayers(wc_layers_fill)){
for(j in 1:reps){
wc_layers_fill[[i]]<-focal(wc_layers_fill[[i]], w=matrix(1,3,3), fun=mean, na.rm=TRUE, pad=TRUE, NAonly=TRUE)
}
}

#The layer names are removed for some reason when I run the above code.
names(wc_layers_fill)<-names(wc_layers)

#Checking if all NA's from presence points have been filled
length(which(is.na(extract(wc_layers_fill[[1]], unique(all.occ[,2:3])))))

#rm(wc_layers)

#Writing the cropped layers to disk
#UseCores<-detectCores() -1
#cl<- makeCluster(UseCores, type="FORK")
#registerDoParallel(cl)
#tmp<-foreach(i = 1:nlayers(wc_layers_fill)) %dopar% 
#{  
#  r<-raster::raster(wc_layers_fill, i)
#  raster::writeRaster(r, #filename=names(wc_layers_fill)[i],overwrite=TRUE, format="GTiff")
# rm(r)
#}
#stopCluster(cl)   

setwd("sdm_layers/worldclim/wg_ext")
writeRaster(wc_layers_fill, filename=names(wc_layers_fill), bylayer=TRUE,format="GTiff", overwrite=TRUE)
setwd("D:/PhD data/Analysis/Chapter2_2Nov17")

#Inputing Bio-ORACLE layers
#bo_list<-list.files(path="sdm_layers/biooracle/ip_ext_unedited", pattern="*tif")
bo_list<-list.files(path="sdm_layers/biooracle", pattern="*tif")
bo_layers<-stack()
for(i in 1:length(bo_list)){
  #bo.raster<-raster(paste0("sdm_layers/biooracle/ip_ext_unedited/", bo_list[i]))
  bo.raster<-raster(paste0("sdm_layers/biooracle/", bo_list[i]))
  bo.raster<-crop(bo.raster, ext)
  bo_layers<-stack(bo_layers, bo.raster)
}

#Checking if everything with bo_layers looks okay
#plot(bo_layers)

#Giving the layers meaningful names
names(bo_layers)<-c("chlo_max", "chlo_mean", "chlo_min", "cloud_cover_max", "cloud_cover_mean", "cloud_cover_min", "phyto_max", "phyto_mean", "phyto_min", "pp_max", "pp_mean", "pp_min")

#Resampling bo_layers by an order of magnitude to make it match marspec and worldclim layers
bo_layers<-resample(bo_layers, ms_layer)

#Setting min and max
bo_layers<-setMinMax(bo_layers)

plot(bo_layers)

#write unedited raster to disk
#UseCores<-detectCores()-1
#cl<- makeCluster(UseCores, type="FORK")
#registerDoParallel(cl)
#tmp<-foreach(i = 1:nlayers(bo_layers)) %dopar% 
#{  
#  r<-raster::raster(bo_layers, i)
#  raster::writeRaster(r, filename=names(bo_layers)[i],overwrite=TRUE, #format="GTiff")
#  rm(r)
#}
#stopCluster(cl) 

setwd("sdm_layers/biooracle/ip_ext_unedited")
writeRaster(bo_layers, filename=names(bo_layers), bylayer=TRUE,format="GTiff", overwrite=TRUE)
setwd("C:/Bharti/PhD data/Analysis/Chapter2_2Nov17")

#Finding out how many NAs exist in extracted values = 4
length(which(is.na(extract(bo_layers[[1]], unique(all.occ[,2:3]))))) 

#Replacing NAs with mean of 8 neighbouring cells
reps<-2
bo_layers_fill<-bo_layers
for(i in 1:nlayers(bo_layers_fill)){
  for(j in 1:reps){
    bo_layers_fill[[i]]<-focal(bo_layers_fill[[i]], w=matrix(1,3,3), fun=mean, na.rm=TRUE, pad=TRUE, NAonly=TRUE)
  }
}

names(bo_layers_fill)<-names(bo_layers)

#Checking if all the NAs have been removed
length(which(is.na(extract(bo_layers_fill[[1]], unique(all.occ[,2:3]))))) 

#Writing the raster to disk
#UseCores<-detectCores()-1
#cl<- makeCluster(UseCores, type="FORK")
#registerDoParallel(cl)
#tmp<-foreach(i = 1:nlayers(bo_layers_fill)) %dopar% 
#{  
#  r<-raster::raster(bo_layers_fill, i)
#  raster::writeRaster(r, #filename=names(bo_layers_fill)[i],overwrite=TRUE, format="GTiff")
#  rm(r)
#}
#topCluster(cl) 

setwd("sdm_layers/biooracle/ip_ext")
writeRaster(bo_layers_fill, filename=names(bo_layers_fill), bylayer=TRUE, format="GTiff", overwrite=TRUE)
setwd("C:/Bharti/PhD data/Analysis/Chapter2_2Nov17")

#rm(bo_layers_fill)

##Loading topobath data from gebco
topobath<-raster("sdm_layers/gebco/GEBCO_2014_2D.nc")
projection(topobath)<-wgs

topobath<-crop(topobath, ip.ext)

#resampling topobath so that the resolution and extent match that of ms_layers
tb<-resample(topobath, ms_layer)

#Calculating the slope using the GEBCO topobath layer using the function terrain
tb_slope<-terrain(tb, opt='slope', unit='degrees')

#Set min and max
tb_slope<-setMinMax(tb_slope)

#Plot the topobath slope values next to each other
plot(tb_slope)

#Plot sampling points on the tb_slope layer
plot(tb_slope)
plot(crop.coastline, add=TRUE)

#Writing tb to disk
writeRaster(tb, filename="sdm_layers/topobath/topo_bath_ip.tif", format="GTiff", overwrite=TRUE)

#Writing tb_slope to disk
writeRaster(tb_slope, filename="sdm_layers/topobath/topo_bath_slope_ip.tif", format="GTiff", overwrite=TRUE)

ls()
ms_layer

rm(bo.raster, bo_layers, bo_list, files, files.max, files.min, tb, tb_slope, tidalAmp, tidalAmp.con, tidalAmp.ha, tidalAmp.lat, tidalAmp.lon, tidalMax, tidalMax.r, tidalMax_nc, tidalMin, tidalMin.r, tidalMin_nc, tidalRange, tidalRange.fill, tidalRange.r, tidalRange_nc, tidalRange2.fill, tidalRange2.r, topobath)

#If I keep the slope parameter, and I have to think carefully about what this actually means and how it might influence species presence in a region

##Creating the shortest distance to 200m bathymetry polygon raster

#This is how MARSPEC did it - information from their meta-data - "To generate the distance to shore raster, the GSHHS land mask was first converted to a Plate Carrée projection (also known as an equirectangular or equidistant cylindrical map projection) so that distances could be calculated in kilometers rather than arc-degrees. Due to edge effects in the calculations, distance was measured in two rounds using the Spatial Analyst extension in ArcGIS 9.3."
crop.shelf<-crop(shelf, ip.ext)
plot(crop.shelf)

#Calculate shortest distance between a point and a polygon 
gDistance(recordsSpatial, crop.shelf)

#The above code did not work because there was no spatial projection assigned to the data input. The program has to know how degrees relate to physical distance in the given area. Usually one chooses an appropriate UTM zone, but the data here spans many, so using the projection used for MARSPEC - Plate Carrée projection (equirectangular or equidistant cylindrical map projection)

#I had actually run this using UTM zone 43 for calculations - this can lead to distortions in the E-W axis - something that has to be kept in mind. Not using that though.

#Creating a variable that stores the projection we want to use
espg.32643<-"+proj=utm +zone=43 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
espg.32663<-"+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#Transforming the crop.shelf polygon into the above projection
crop.shelf.proj<-spTransform(crop.shelf, CRS(espg.32663))
plot(crop.shelf.proj)

#Creating a new raster where distance values will be stored. It has the same extent as crop.shelf.proj and the same number of cells as ms_layers rasters
#ms_layers[[3]]
dist.shore<-raster(extent(crop.shelf.proj), nrow=nrow(ms_layer), ncol=ncol(ms_layer))
values(dist.shore)<-rep(1, ncell(dist.shore))
crs(dist.shore)<-espg.32663

#Rerunning gDistance on the projected data-set again 

#The probably functions I can use are distance, gDistance, dist2Line
#Trying out gDistance and dist2Line. The gDistance code is inspired from https://gis.stackexchange.com/questions/226554/create-raster-in-r-distance-to-line-feature 

#Using the polygon as such, but converting the raster to a SpatialPoints format. Turns out the gDistance function does not work if the raster/SpatialPoints variable has NA values
dist.shore[]<-gDistance(crop.shelf.proj, as(dist.shore, "SpatialPoints"), byid=TRUE)/1000
plot(dist.shore)

#I also converted SpatialPolygon to SpatialLines and retried it - the results have identical values uptil a point, but after that the values start differing -  When I convert it to SpatialLines, the islands are considered separately while in polygon they are considered as one variable (?) and hence the difference in values between them.

#Re-projecting the distance raster dist.shore into lat long
distShore<-projectRaster(dist.shore, crs=wgs)

#On reprojection the extent and resolution are not the same as before
distShore<-resample(distShore, ms_layers_fill[[1]])
distShore<-raster::mask(distShore, crop.shelf, inverse=TRUE)

#Set min and max
distShore<-setMinMax(distShore)

distShore
ms_layers_fill[[1]]

plot(distShore)
plot(crop.coastline, add=TRUE)
plot(crop.shelf, add=TRUE)

#Writing the distShore raster to disk
writeRaster(distShore, filename="sdm_layers/dist_200m_isobath", format="GTiff", overwrite=TRUE)

###############################################################

ms_list<-list.files(path="sdm_layers/marspec/ip_ext", pattern="*.tif")
ms_layers_fill<-stack()

for(i in 1:length(ms_list)){
ms.raster<-raster(paste0("sdm_layers/marspec/ip_ext/", ms_list[i]))
ms_layers_fill<-stack(ms_layers_fill, ms.raster)
}

#Input wc_layers_fill from disk
wc_list<-list.files(path="sdm_layers/worldclim/ip_ext", pattern="*.tif")
wc_layers_fill<-stack()

for(i in 1:length(wc_list)){
wc.raster<-raster(paste0("sdm_layers/worldclim/ip_ext/", wc_list[i]))
wc_layers_fill<-stack(wc_layers_fill, wc.raster)
}

#Input bo_layers_fill from disk
bo_list<-list.files(path="sdm_layers/biooracle/ip_ext", pattern="*.tif")
bo_layers_fill<-stack()

for(i in 1:length(bo_list)){
bo.raster<-raster(paste0("sdm_layers/biooracle/ip_ext/", bo_list[i]))
bo_layers_fill<-stack(bo_layers_fill, bo.raster)
}

#Input topobath 
tb<-raster("sdm_layers/topobath/topo_bath_ip.tif")

#Input topobath slope
tb_slope<-raster("sdm_layers/topobath/topo_bath_slope_ip.tif")

#Input tidalRange.fill
tidalRange.fill<-raster("sdm_layers/tidalRange/ip_resampled/tidal_range_minmax_fill_ip.tif")

###############################################################

##Selecting predictor variables
#Putting all the raster layers together
candidates<-stack(ms_layers_fill, wc_layers_fill, bo_layers_fill, tb, tb_slope, tidalRange.fill)

names(candidates)

#Removing dist_to_shore_km from candidates - since the fill NA with mean of 8 neighbours was used - this wouldn't make much sense
candidates<-dropLayer(candidates, c(1:3, 14:16, 20:22))
names(candidates)

#Using a mask such that only points close to the coast are selected - which should be an intersection between wc, ms, bo, tb, tb_slope, tidal_range. Using code from https://stackoverflow.com/questions/5598516/intersection-of-bands-in-r-raster-package to do this
cand.mask <-sum(candidates)
extract(candidates, unique(all.occ[,2:3])))

#writing cand.mask to disk
writeRaster(cand.mask, filename="sdm_layers/cand_mask_ip", format="GTiff", overwrite=TRUE)

#Using cand.mask as a mask to convert cells to NA that don't have values for atleast one of 
candidates<-raster::mask(candidates, cand.mask)
names(candidates)

setwd("sdm_layers/candidates_ip")
writeRaster(candidates, filename=names(candidates), bylayer=TRUE, format="GTiff", overwrite=TRUE)
setwd("C:/Bharti/PhD data/Analysis/Chapter2_2Nov17") 



#plot(candidates)

##Take the records of the focal species
n<-"und.occ"
sp.occ<-ind.occ[[n]]

#Converting long and lat values to a spatial points file
recordsSpatial<-SpatialPointsDataFrame(coords=cbind(sp.occ$long, sp.occ$lat), data=sp.occ, proj4string=CRS(wgs))

plot(crop.coastline)
points(recordsSpatial, pch=19, col="blue")

##Looking at the correlation between different candidate predictors

#Calculating the correlation for 10000 random points extracted from the extent

cor.pts<-randomPoints(candidates, 100000, lonlatCorrection=TRUE)

#By default cor.test uses squared Spearman correlation coefficients, Hoeffding's D statistic can also be used which is sensitive to various kinds of relationships including highly non-monotonic relationships
envSpecies<-extract(candidates, cor.pts)
cor.tree<-varclus(envSpecies)
plot(cor.tree)

#Dropping layers that are redundant after looking at cor.test
names(candidates)
predictors<-dropLayer(candidates, c(1,3,5,9:14,16,19))
names(predictors)
predictors<-setMinMax(predictors)

writeRaster(predictors, bylayer=TRUE, filename=names(predictors), format="GTiff", overwrite=TRUE)

#Input predictors from disk
pred_list<-list.files(path="sdm_layers/predictors", pattern="*.tif")
predictors<-stack()

for(i in 1:length(pred_list)){
pred.raster<-raster(paste0("sdm_layers/predictors/", pred_list[i]))
predictors<-stack(predictors, pred.raster)
}

#rm(bo.raster, bo_layers_fill, bo_list, candidates, distShore, ms.raster, ms_layers_fill, ms_list, tb, tb_slope, tidalRange.fill, wc.raster, wc_layers_fill, wc_list)

#Extracting values for records from layers in the predictors stack
envSpecies<-extract(predictors, recordsSpatial)
records<-cbind(sp.occ[,2:3], envSpecies)

#Removing multiple records from the same cell - Adam Smith code
#inputing the function
source('./R scripts/Eliminate Points in Same Cell of a Raster.r')

recordsNoDups<-elimCellDups(records, predictors[[1]], longLatFields=c('long', 'lat'), priority=NULL)
nrow(records)
nrow(recordsNoDups)
write.csv(recordsNoDups, file="dataframes/und_occ.csv", row.names=FALSE)

#Plot extracted values. Looking at any outliers in data by plotting one variable against another. Code from here - https://stackoverflow.com/questions/13035834/plot-every-column-in-a-data-frame-as-a-histogram-on-one-page-using-ggplot - I don't understand the steps very well though - need to take some time out to look at this
d <- melt(recordsNoDups[,c(3:ncol(recordsNoDups))])
ggplot(d,aes(x = value)) +  facet_wrap(~variable,scales = "free_x") + 
geom_histogram()

##Creating random background points - Adam Smith code
set.seed(454)
randomBgSites<-randomPoints(predictors, 10000)
randomBgEnv<-as.data.frame(extract(predictors, randomBgSites))

#combine with coordinates and rename coordinate fields
randomBg<-cbind(randomBgSites, randomBgEnv)
colnames(randomBg)[1:2] <- c('long', 'lat')
head(randomBg)
write.csv(randomBg, file="dataframes/randomBg.csv", row.names=FALSE)
#randomBg<-read.csv("dataframes/randomBg.csv")

##Creating a kernel desity smoothing surface for sampling locations (combining locations were littorinids were and were not found) and using this as the probability surface to sample background locations
#Doing this for all sites and for mangrove and rocky shore separately as well

rs.hab<-c("rocky", "mixed")
mg.hab<-c("mangrove", "mixed")

rs.occ<-unq.occ[unq.occ$habitat %in% rs.hab,]
write.csv(rs.occ, file="dataframes/rs_occ", row.names=FALSE)

mg.occ<-unq.occ[unq.occ$habitat %in% mg.hab, ]
write.csv(mg.occ, file="dataframes/mg_occ", row.names=FALSE)

#creating the probability surface for all habitats 
#running the kde2d function without specifying the number of cells to calculate extent. Code from - https://scottrinnan.wordpress.com/2015/08/31/how-to-construct-a-bias-file-with-r-for-use-in-maxent-modeling/

#the rasterize function converts any part of a polygon falling over the center of a raster cell into a raster cell and the field argument transfers the specified value to this new raster cell

kd.ras.fn<-function(x.pt, y.pt, ras){
occur.ras<-rasterize(cbind(x.pt, y.pt), ras,  field=1)
kd.ras<-raster(kde2d(x.pt, y.pt, n=c(nrow(occur.ras), ncol(occur.ras)), h=0.5))
kd.ras[kd.ras==0]<-NA
kd.ras<-resample(kd.ras, ras)
kd.ras<-raster::mask(kd.ras, ras)
kd.ras
}

all.kd<-kd.ras.fn(unq.occ[,2], unq.occ[,3], cand.mask)
writeRaster(all.kd, filename="all_kd", overwrite=TRUE, format="GTiff")

rs.kd<-kd.ras.fn(rs.occ[,2], rs.occ[,3], cand.mask)
writeRaster(rs.kd, filename="rs_kd", overwrite=TRUE, format="GTiff")

mg.kd<-kd.ras(mg.occ[,2], mg.occ[,3], cand.mask)
writeRaster(mg.kd, filename="mg_kd", overwrite=TRUE, format="GTiff")

#Creating background points based on the kernel density probability surface. Doing this for both all sites and also habitat specific probability surface
set.seed(30985)
kdBgSites<-randomPoints(all.kd, 10000, prob=TRUE)
kdBgEnv<-as.data.frame(extract(predictors, kdBgSites))
kdBg<-cbind(kdBgSites, kdBgEnv)
colnames(kdBg)[1:2]<-c("long", "lat")
write.csv(kdBg, file="dataframes/kdBg.csv", row.names=FALSE)
#kdBg<-read.csv("dataframes/kdBg.csv")

set.seed(485743)
kdRockBgSites<-randomPoints(rs.kd, 10000, prob=TRUE)
kdRockBgEnv<-as.data.frame(extract(predictors, kdRockBgSites))
kdRockBg<-cbind(kdRockBgSites, kdRockBgEnv)
colnames(kdRockBg)[1:2]<-c("long", "lat")
write.csv(kdRockBg, file="dataframes/kdRockBg.csv", row.names=FALSE)
#kdRockBg<-read.csv("dataframes/kdRockBg.csv")

set.seed(9238)
kdMangBgSites<-randomPoints(mg.kd, 10000, prob=TRUE)
kdMangBgEnv<-as.data.frame(extract(predictors, kdMangBgSites))
kdMangBg<-cbind(kdMangBgSites, kdMangBgEnv)
colnames(kdMangBg)[1:2]<-c("long", "lat")
write.csv(kdMangBg, file="dataframes/kdMangBg.csv", row.names=FALSE)
#kdMangBg<-read.csv("dataframes/kdMangBg.csv")

##Creating another background sites dataframe - where a x km radius is drawn about each of the target species points samples - it is coalesced into a polygon and random background points are drawn from this polygon
#This code is taken from http://www.rspatial.org/sdm/rst/3_sdm_absence-background.html
#Also doing a 20 km radius polygon

#Save sites as a spatial points dataframe - defining which columns in are the coordinates
coordinates(unq.occ)<-~long+lat
projection(unq.occ)<-wgs

coordinates(rs.occ)<-~long+lat
projection(rs.occ)<-wgs

coordinates(mg.occ)<-~long+lat
projection(mg.occ)<-wgs

#Creating circles of radius 20 km around each sampled point
rad<-20000 #20 km radius

pol20<-polygons(circles(all.occ, d=rad, lonlat=TRUE))
pol20Rock<-polygons(circles(rs.occ, d=rad, lonlat=TRUE))
pol20Mang<-polygons(circles(mg.occ, d=rad, lonlat=TRUE))

plot(pol20, col=alpha("blue", 0.2))
plot(pol20Rock, col=alpha("red", 0.2))
plot(pol20Mang, col=alpha("green", 0.2))

#Creating an intersect of the raster stack with the polygons and extracting random points from there
##Creating random background points - Adam Smith code

set.seed(6644)
bufBgSites<-randomPoints(raster::mask(cand.mask, pol20), 10000)
bufBgEnv<-as.data.frame(extract(predictors, bufBgSites))
bufBg<-cbind(bufBgSites, bufBgEnv)
colnames(bufBg)[1:2]<-c("long", "lat")
write.csv(bufBg, file="dataframes/bufBg.csv", row.names=FALSE)
#bufBg<-read.csv("dataframes/bufBg.csv")

set.seed(8457)
bufRockBgSites<-randomPoints(raster::mask(cand.mask, pol20Rock), 10000)
bufRockBgEnv<-as.data.frame(extract(predictors, bufRockBgSites))
bufRockBg<-cbind(bufRockBgSites, bufRockBgEnv)
colnames(bufRockBg)[1:2]<-c("long", "lat")
write.csv(bufRockBg, file="dataframes/bufRockBg.csv", row.names=FALSE)
#bufRockBg<-read.csv("dataframes/bufRockBg.csv")

set.seed(90384)
bufMangBgSites<-randomPoints(raster::mask(cand.mask, pol20Mang), 10000)
bufMangBgEnv<-as.data.frame(extract(predictors, bufMangBgSites))
bufMangBg<-cbind(bufMangBgSites, bufMangBgEnv)
colnames(bufMangBg)[1:2]<-c("long", "lat")
write.csv(bufMangBg, file="dataframes/bufMangBg.csv", row.names=FALSE)
#bufMangBg<-read.csv("dataframes/bufMangBg.csv")

##I have decided on testing out two sets of feature functions - linear + quadratic + product, linear + quadratic + threshold and maybe for species where there are enough points linear + quadratic + product + threshold - STARTING WITH LINEAR + QUADRATIC + PRODUCT

##Running maxent models with each of the background locations, keeping the beta regularization factor as default and plotting the results

#Code for jackknifing over variables
pred.jk<-function(input){
env.input<-input[,3:(ncol(input)-1)]
pres.input<-input[,ncol(input)]
modGain<-as.data.frame(matrix(NA, nrow=nrow(input), ncol=2))
names(modGain)<-c("ex-var", "in-var")

for(i in 1:ncol(env.input)){
exModel<-maxent(x=env.input[,-i], p=pres.input, args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false'))

inModel<-maxent(x=env.input[,i],  p=pres.input, args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false'))

modGain[i,]<-c(exModel@results[2], inModel@results[2])
}
modGain
}

##Maxent model using random background points
trainData<-rbind(recordsNoDups, randomBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(randomBg)))

randomBgIn<-cbind(trainData, presentBg)
ncol(randomBgIn)

write.csv(randomBgIn, file="dataframes/und_randomBg_input.csv", row.names=FALSE)

randomBgModel<-maxent(x=randomBgIn[,3:(ncol(randomBgIn)-1)], p=randomBgIn[,ncol(randomBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', 'jackknife=true', 'responsecurves=true'))

plot(randomBgModel)

save(randomBgModel, file="results/undulata/randomBgModel")

randomBgResponse<-response(randomBgModel, range='p')

randomBgMap<-predict(randomBgModel, predictors)
writeRaster(randomBgMap, filename="results/undulata/randomBgMap", format="GTiff", overwrite=TRUE)

#Creating MESS map to see where combination of environmental variables is not the same as the training data
randomBgMESS<-mess(predictors, randomBgIn[,3:(ncol(randomBgIn)-1)])
plot(randomBgMESS)
writeRaster(randomBgMESS, filename="results/undulata/randomBgMESS", format="GTiff")

#creating a limiting variable map for the model
randomBgLim<-limiting(predictors, randomBgModel)
plot(randomBgLim)
writeRaster(randomBgLim, filename="results/undulata/randomBgLim", format="GTiff", overwrite=TRUE)

##Maxent model using kernel density based points 
trainData<-rbind(recordsNoDups, kdBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(kdBg)))

kdBgIn<-cbind(trainData, presentBg)

write.csv(kdBgIn, file="dataframes/und_kdBg_input.csv", row.names=FALSE)

kdBgModel<-maxent(x=kdBgIn[,3:(ncol(kdBgIn)-1)], p=kdBgIn[,ncol(kdBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', 'jackknife=true', 'responsecurves=true'))

plot(kdBgModel)

save(kdBgModel, file="results/undulata/kdBgModel")

kdBgResponse<-response(kdBgModel, range='p')

kdBgMap<-predict(kdBgModel, predictors)
writeRaster(kdBgMap, filename="results/undulata/kdBgMap", format="GTiff", overwrite=TRUE)

plot(kdBgMap)

#Creating MESS map to see where combination of environmental variables is not the same as the training data
kdBgMESS<-mess(predictors, kdBgIn[,3:(ncol(kdBgIn)-1)])
plot(kdBgMESS)
writeRaster(kdBgMESS, filename="results/undulata/kdBgMESS", format="GTiff")

#creating a limiting variable map for the model
kdBgLim<-limiting(predictors, kdBgModel)
plot(kdBgLim)
writeRaster(kdBgLim, filename="results/undulata/kdBgLim", format="GTiff")

##Maxent model using kernel density based points - but for habitat specific background sites only
kdHabBg<-kdRockBg 
trainData<-rbind(recordsNoDups, kdHabBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(kdHabBg)))

kdHabBgIn<-cbind(trainData, presentBg)
nrow(kdHabBgIn)

write.csv(kdHabBgIn, file="dataframes/und_kdRockBg_input.csv", row.names=FALSE)

kdHabBgModel<-maxent(x=trainData[,3:(ncol(kdHabBgIn)-1)], p=kdHabBgIn[,ncol(kdHabBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', 'jackknife=true', 'responsecurves=true'))

plot(kdHabBgModel)

save(kdHabBgModel, file="results/undulata/kdHabBgModel")

kdHabBgResponse<-response(kdHabBgModel, range='p')

kdHabBgMap<-predict(kdHabBgModel, predictors)
writeRaster(kdHabBgMap, filename="results/undulata/kdHabBgMap", format="GTiff", overwrite=TRUE)

#Creating MESS map to see where combination of environmental variables is not the same as the training data
kdHabBgMESS<-mess(predictors, kdHabBgIn[,3:(ncol(kdHabBgIn)-1)])
plot(kdBahBgMESS)
writeRaster(kdHabBgMESS, filename="results/undulata/kdHabBgMESS", format="GTiff")

#creating a limiting variable map for the model
kdHabBgLim<-limiting(predictors, kdHabBgModel)
plot(kdHabBgLim)
writeRaster(kdHabBgLim, filename="results/undulata/kdHabBgLim")

##Maxent model using background sites from 20 km buffer
trainData<-rbind(recordsNoDups, bufBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(bufBg)))

bufBgIn<-cbind(trainData, presentBg)

write.csv(bufBgIn, file="dataframes/und_bufBg_input.csv", row.names=FALSE)

bufBgModel<-maxent(x=bufBgIn[,3:(ncol(bufBgIn)-1)], p=bufBgIn[,ncol(bufBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', 'jackknife=true', 'responsecurves=true'))

plot(bufBgModel)

save(bufBgModel, file="results/undulata/bufBgModel")

bufBgResponse<-response(bufBgModel, range='p')

bufBgMap<-predict(bufBgModel, predictors)

writeraster(bufBgMap, filename="results/undulata/bufBgMap", format="GTiff", overwrite=TRUE))

#Creating MESS map to see where combination of environmental variables is not the same as the training data
bufBgMESS<-mess(predictors, bufBgIn[,3:(ncol(bufBgIn)-1)])
plot(bufBgMESS)
writeRaster(bufBgMESS, filename="results/undulata/bufBgMESS", format="GTiff", overwrite=TRUE)

#creating a limiting variable map for the model
bufBgLim<-limiting(predictors, bufBgModel)
writeRaster(bufBgLim, filename="results/undulata/bufBgLim", format="GTiff", overwrite=TRUE)

##Maxent model using buffer based points - but for habitat specific background sites only
bufHabBg<-bufRockBg 
trainData<-rbind(recordsNoDups, bufHabBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(bufHabBg)))

bufHabBgIn<-cbind(trainData, presentBg)

write.csv(bufHabBgIn, file="dataframes/und_bufRockBg_input.csv", row.names=FALSE)

bufHabBgModel<-maxent(x=bufHabBgIn[,3:(ncol(bufHabBgIn)-1)], p=bufHabBgIn[,ncol(bufHabBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', 'jackknife=true', 'responsecurves=true'))

plot(bufHabBgModel)

save(bufHabBgModel, file="results/undulata/bufHabBgModel")

bufHabBgResponse<-response(bufHabBgModel, range='p')

bufHabBgMap<-predict(bufHabBgModel, predictors)
writeRaster(bufHabBgMap, filename="results/undulata/bufHabBgMap", format="GTiff", overwrite=TRUE)

#Creating MESS map to see where combination of environmental variables is not the same as the training data
bufHabBgMESS<-mess(predictors, bufHabBgIn[,3:(ncol(bufHabBgIn)-1)])
plot(bufHabBgMESS)
writeRaster(bufHabBgMESS, filename="results/undulata/bufHabBgMESS", format="GTiff", overwrite=TRUE)

#creating a limiting variable map for the model
bufHabBgLim<-limiting(predictors, bufHabBgModel)
plot(bufHabBgLim)
writeRaster(bufHabBgLim, filename="results/undulata/bufHabBgLim", format=GTiff, overwrite=TRUE)

##Running calc.aicc from the package ENMeval to find the value of beta regularization factor - choosing to use mixedBg20 and the features as above - linear, quadratic and product
beta.AIC<-function(bgIn, beta.val, predictors, coord){
nparams<-rep(NA, times=length(beta.val))
pred.stack<-stack()
model<-list()
for(i in 1:length(beta.val)){
model[[i]]<-maxent(x=bgIn[,3:(ncol(bgIn)-1)], p=bgIn[, ncol(bgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', paste0("betamultiplier=", beta.val[i])))
pred.stack<-stack(pred.stack, predict(model[[i]], predictors))
names(pred.stack)[i]<-paste0("beta ", beta.val[i])
nparams[i]<-get.params(model[[i]])
}
aicc<-calc.aicc(nparams, coord, pred.stack)
aicc$beta.val<-beta.val
aicc<-aicc[order(aicc$AICc),]
aicc
}

#Carrying out test with different values of beta for all the models
beta.val<-seq(1,5, by=1)
randomBg_beta<-beta.AIC(randomBgIn, beta.val, predictors, recordsNoDups[,1:2])
kdBg_beta<-beta.AIC(kdBgIn, beta.val, predictors, recordsNoDups[,1:2])
kdHabBg_beta<-beta.AIC(kdHabBgIn, beta.val, predictors, recordsNoDups[,1:2])
bufBg_beta<-beta.AIC(bufBgIn, beta.val, predictors, recordsNoDups[,1:2])
bufHabBg_beta<-beta.AIC(bufHabBgIn, beta.val, predictors, recordsNoDups[,1:2])

##Evaluating the models - looking at the ROC 
randomBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=randomBg[,3:ncol(randomBg)], randomBgModel)
save(randomBgEval, file="results/undulata/randomBgEval")

kdBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=kdBg[,3:ncol(kdBg)], kdBgModel)
plot(kdBgEval, 'ROC')
save(kdBgEval, file="results/undulata/kdBgEval")

kdHabBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=kdHabBg[,3:ncol(kdHabBg)], kdHabBgModel)
plot(kdHabBgEval, 'ROC')
save(kdHabBgEval, file="results/undulata/kdHabBgEval")

bufBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=bufBg[,3:ncol(bufBg)], bufBgModel)
plot(bufBgEval, 'ROC')
save(bufBgEval, file="results/undulata/bufBgEval")

bufHabBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=bufHabBg[,3:ncol(bufHabBg)], bufHabBgModel)
plot(bufHabBgEval, 'ROC')
save(bufHabBgEval, file="results/undulata/bufHabBgEval")

##Dividing presence data into 5 groups - using 4/5ths for training and 1/5ths for test. Code from Hijmans and Elith 2017

#For each of the folds , dividing data into test and training data, combining test data with background data and giving presence codes to the rows. Running the model using training data, keeping the features linear, quadratic and product.

#Here k refers to the number of k folds, fold refers to the k fold group, pres is the presence data dataframe and bg is the background data dataframe

maxent_kfold<-function(k, pres, bg, beta_val){
evl<- list()
model<-list()
group <- kfold(pres, k)
for (i in 1:k) {
test <- pres[group  == i,]
train_pres <- pres[group  != i, ]
train<-rbind(train_pres, bg)
code<-c(rep(1, nrow(train_pres)), rep(0, nrow(bg)))
model[[i]] <- maxent(x=train[,3:ncol(train)],  p=code, args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', paste0("betamultiplier=", beta_val)))

  evl[[i]] <- evaluate(p=test[,3:ncol(test)], a=bg[,3:ncol(bg)], model[[i]])
 }
 evl
 }
  
#Running the k-folds evaluation on the default beta value model
beta_val<-1
maxent_kfold(5, recordsNoDups, randomBg, beta_val)
maxent_kfold(5, recordsNoDups, kdBg, beta_val)
maxent_kfold(5, recordsNoDups, kdHabBg, beta_val)
maxent_kfold(5, recordsNoDups, bufBg, beta_val)
maxent_kfold(5, recordsNoDups, bufHabBg, beta_val)

#AUC value tells you the probability that if you pick a random presence and absence, the presence has been given a higher relative occurrence rate than the absence. But what sense does it make for pseudoabsences.

#the cor values are very low - cor is the correlation coefficient for predicted value of environmental suitability for a cell and the actual status of the cell (presence or absence)

##Looking if the residuals have a spatial patterning
modelMap<-stack(randomBgMap, kdBgMap, kdHabBgMap, bufBgMap, bufHabBgMap)
names(modelMap)<-c("randomBg", "kdBg", "kdHabBg", "bufBg", "bufHabBg")

for(i in 1:nlayers(modelMap)){
predict.pres<-raster::extract(modelMap[[i]], recordsNoDups[,1:2])
residual<-1-predict.pres
predict.pres<-cbind(recordsNoDups[,1:2], residual)

mycol<-colorRampPalette(c("blue", "red"))
jpeg(file = paste0("results/undulata/", names(modelMap)[i], "Res.jpeg"))
plot(modelMap[[i]],  main="Residuals - Model")
points(predict.pres, pch=19, col=mycol(10), cex=0.5)
dev.off()
}


##############################################################

names(predictors)








