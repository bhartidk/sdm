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

#input extent
ip.ext<-c(24, 150,-36,30.21667)

#input model raster layer for resampling
ms_layer<-raster("sdm_layers/marspec/ip_ext/mean_annual_sss_psu.tif")

#input projection
wgs<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
projection(ms_layer)<-wgs

#save extent as another name
ext<-ip.ext

#input occurrence data
all.occ<-read.csv("dataframes/allsites_env_4Nov17.csv")

#removing the row number column and all the extracted data columns from the dataframe 
all.occ<-all.occ[,-c(1,7:10, 12:38)]
head(all.occ)

#changing the order of longitude and latitude in the dataframe - easier to handle rasters that way
all.occ<-all.occ[,c(2,5,4,3,6)]
head(all.occ)

#creating a dataframe which has unique values of all.occ
rownames(all.occ)
rn<-as.numeric(rownames(unique(all.occ[,2:3])))
unq.occ<-all.occ[rn,]

#adding other sampling locations where no species were found but were a part of the sampling effort to this dataframe
no.occ<-read.csv("dataframes/coords_nosp_12Jan18.csv")
unq.occ<-rbind(unq.occ, no.occ)
#write.csv(unq.occ, file="dataframes/unq_occ.csv", row.names=TRUE)

#rbinding all.occ and no.occ
all.occ<-rbind(all.occ, no.occ)

#writing the above dataframe to disk
#write.csv(all.occ, file="dataframes/all_occ_12Feb18.csv", row.names=FALSE)

#input the extended occurrence data from Reid and Reid referred publications
ip.occ<-read.csv("dataframes/species_locations_25Jan18.csv")
head(ip.occ)

#removing an extra column that has for some reason been added in the end
ip.occ<-ip.occ[,-ncol(ip.occ)]
head(ip.occ)

#removing locations from India
colnames(ip.occ)
ip.occ<-ip.occ[, c(2,12,13,6,15)]
nrow(ip.occ)
 
#retaining only those rows where there are no NAs - not that we have reduced the number of columns used, the only columns that can give NA values are long and lat
ip.occ<-ip.occ[complete.cases(ip.occ),]
nrow(ip.occ)

#changing the column names to match all.occ, so that I can rbind them together
colnames(ip.occ)
colnames(ip.occ)<-colnames(all.occ)
head(ip.occ)

#merging ip.occ with all.occ
ip.occ<-rbind(all.occ, ip.occ)
rownames(ip.occ)<-seq(1, nrow(ip.occ), by=1)
nrow(ip.occ)

plot(ms_layer)
points(ip.occ[,2:3])

#retaining only those points which fall into ip.extent
ext.pol<-as(raster::extent(ip.ext), "SpatialPolygons")
projection(ext.pol)<-wgs

coordinates(ip.occ)<-~long+lat
projection(ip.occ)<-wgs

within<-over(ip.occ, ext.pol)
table(within)

ip.occ<-ip.occ[!(is.na(within)),]
nrow(ip.occ)

plot(ms_layer)
points(ip.occ[,2:3])

#write.csv(ip.occ, "dataframes/ip_occ_15Feb18.csv")

##Downloading raster layers 
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

#MARSPEC counting number of NAs in extracted value from presence points
length(which(is.na(extract(ms_layers[[1]], unique(ip.occ[,2:3])))))

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
length(which(is.na(extract(ms_layers_fill[[1]], unique(ip.occ[,2:3])))))
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

setwd("sdm_layers/worldclim/wg_ext_unedited")
writeRaster(wc_layers, filename=names(wc_layers), bylayer=TRUE,format="GTiff", overwrite=TRUE)
setwd("D:/PhD data/Analysis/Chapter2_2Nov17")

#inputing wc_layers_fill
wc<-list.files(path="sdm_layers/worldclim/ip_ext", pattern="*.tif")

wc_layers_fill<-stack()
for(i in 1:length(wc)){
wc.raster<-raster(paste0("sdm_layers/worldclim/ip_ext/", wc[i]))
wc.raster<-crop(wc.raster, ext)
wc_layers_fill<-stack(wc_layers_fill, wc.raster)
}

#WorldClim counting number of NAs in extracted value from presence points=90
length(which(is.na(extract(wc_layers[[1]], ip.occ[,2:3]))))

#Correcting for NAs near the coastline. For each NA, taking the mean of the 8 cells surrounding it and averaging them to get the central cell value. It takes 2 reps to fills all NAs from extracted values in all.occ - ran it with a test data-set. Help from https://stat.ethz.ch/pipermail/r-sig-geo/2013-July/018709.html
reps<-2
#reps<-1
wc_layers_fill<-wc_layers

for(i in 1:nlayers(wc_layers_fill)){
for(j in 1:reps){
wc_layers_fill[[i]]<-focal(wc_layers_fill[[i]], w=matrix(1,3,3), fun=mean, na.rm=TRUE, pad=TRUE, NAonly=TRUE)
}
}

#The layer names are removed for some reason when I run the above code.
names(wc_layers_fill)<-names(wc_layers)

#Checking if all NA's from presence points have been filled
length(which(is.na(extract(wc_layers_fill[[1]], unique(ip.occ[,2:3])))))

## the NAs are not getting filled - probably because it is very far from land - must be a problem with the islands
napts<-which(is.na(extract(wc_layers_fill[[1]], ip.occ[,2:3])))
ip.occ<-ip.occ[-napts,]

#writing the edited ip.occ points to disk
write.csv(ip.occ, "dataframes/ip_occ_12Feb18.csv", row.names=FALSE)

#rm(wc_layers)

setwd("sdm_layers/worldclim/wg_ext")
writeRaster(wc_layers_fill, filename=names(wc_layers_fill), bylayer=TRUE,format="GTiff", overwrite=TRUE)
setwd("D:/PhD data/Analysis/Chapter2_2Nov17")

#Inputing Bio-ORACLE layers
bo_list<-list.files(path="sdm_layers/biooracle", pattern="*tif")
bo_layers<-stack()
for(i in 1:length(bo_list)){
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

setwd("sdm_layers/biooracle/ip_ext_unedited")
writeRaster(bo_layers, filename=names(bo_layers), bylayer=TRUE,format="GTiff", overwrite=TRUE)
setwd("C:/Bharti/PhD data/Analysis/Chapter2_2Nov17")

#bo_list<-list.files(path="sdm_layers/biooracle/ip_ext_unedited", pattern="*.tif")
#bo_layers<-stack()
#for(i in 1:length(bo_list)){
#bo.raster<-raster(paste0("sdm_layers/biooracle/ip_ext_unedited/", bo_list[i]))
#bo_layers<-stack(bo_layers, bo.raster)
#}
#bo_layers

#Finding out how many NAs exist in extracted values = 4
length(which(is.na(extract(bo_layers[[1]], unique(ip.occ[,2:3]))))) 

#Replacing NAs with mean of 8 neighbouring cells
reps<-2+1+1
#reps<-1
bo_layers_fill<-bo_layers
for(i in 1:nlayers(bo_layers_fill)){
  for(j in 1:reps){
    bo_layers_fill[[i]]<-focal(bo_layers_fill[[i]], w=matrix(1,3,3), fun=mean, na.rm=TRUE, pad=TRUE, NAonly=TRUE)
  }
}

names(bo_layers_fill)<-names(bo_layers)

#Checking if all the NAs have been removed
length(which(is.na(extract(bo_layers_fill[[1]], unique(ip.occ[,2:3]))))) 

setwd("sdm_layers/biooracle/ip_ext")
writeRaster(bo_layers_fill, filename=names(bo_layers_fill), bylayer=TRUE, format="GTiff", overwrite=TRUE)
setwd("C:/Bharti/PhD data/Analysis/Chapter2_2Nov17")

#rm(bo_layers)

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

#If I keep the slope parameter, and I have to think carefully about what this actually means and how it might influence species presence in a region

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

#Setting min and max
tidalRange.r<-setMinMax(tidalRange.r)

#Saving the raster to disk
writeRaster(tidalRange.r, filename="sdm_layers/tidalRange/ip_not_resampled/tidal_range_ip", format="GTiff", overwrite=TRUE)

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
reps<-3
tidalRange.fill<-tidalRange.r
for(i in 1:reps){
tidalRange.fill<-focal(tidalRange.fill, w=matrix(1,3,3),  fun=mean, pad=TRUE, na.rm=TRUE, NAonly=TRUE)
}

#Checking if the NAs have been removed
length(which(is.na(extract(tidalRange.fill, unique(all.occ[,2:3])))))

#Re-sampling it to match the resolution of ms_layers
tidalRange.fill<-resample(tidalRange.fill, ms_layer)

#Saving the raster to disk
writeRaster(tidalRange.fill, filename="sdm_layers/tidalRange/ip_resampled/tidal_range_fill_ip", format="GTiff", overwrite=TRUE)

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
writeRaster(tidalRange2.r, filename="sdm_layers/tidalRange/ip_not_resampled/tidal_range_minmax_ip", format="GTiff", overwrite=TRUE)

length(which(is.na(extract(tidalRange2.r, unique(ip.occ[,2:3]))))) 

#Filling NAs with mean of neighbouring 8 values
reps<-3
tidalRange2.fill<-tidalRange2.r
for(i in 1:reps){
tidalRange2.fill<-focal(tidalRange2.fill, w=matrix(1,3,3),  fun=mean, pad=TRUE, na.rm=TRUE, NAonly=TRUE)
}

#Checking if the NAs have been removed
length(which(is.na(extract(tidalRange2.fill, unique(ip.occ[,2:3])))))

#Resampling to ms_layers extent and resolution
tidalRange2.fill<-resample(tidalRange2.fill, ms_layer)

#Checking if the NAs removal still holds
length(which(is.na(extract(tidalRange2.fill, unique(ip.occ[,2:3])))))

#set min max
tidalRange2.fill<-setMinMax(tidalRange2.fill)

#writing raster to disk
writeRaster(tidalRange2.fill, filename="sdm_layers/tidalRange/ip_resampled/tidal_range_minmax_fill_ip", format="GTiff", overwrite=TRUE)

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

#extracting occurrence data for each species and saving it as a separate dataframe
sp<-as.character(levels(ip.occ$species))

#dividing these cropped points species-wise, again
ip.ind.occ<-list()
for(i in 1:length(sp)){
ip.ind.occ[[i]]<-subset(ip.occ, ip.occ$species %in% sp[i])
names(ip.ind.occ)[[i]]<-paste0("ip.", substr(sp[i], 1, 3), ".occ") 
write.csv(ip.ind.occ[[i]], file=paste0("dataframes/", "ip_", substr(sp[i], 1, 3), "_occ.csv"), row.names=FALSE)
}

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
extract(candidates, unique(ip.occ[,2:3]))

#writing cand.mask to disk
writeRaster(cand.mask, filename="sdm_layers/candidates_mask_ip", format="GTiff", overwrite=TRUE)

#Using cand.mask as a mask to convert cells to NA that don't have values for atleast one of 
candidates<-raster::mask(candidates, cand.mask)
names(candidates)

#checking if all the sampling points are covered
extract(candidates, unique(ip.occ[,2:3]))
nrow(unique(ip.occ[,2:3]))

#writing candidate rasters to disk
setwd("sdm_layers/candidates_ip")
writeRaster(candidates, filename=names(candidates), bylayer=TRUE, format="GTiff", overwrite=TRUE)
setwd("C:/Bharti/PhD data/Analysis/Chapter2_2Nov17") 

#plot(candidates)

##Looking at the correlation between different candidate predictors

#Calculating the correlation for 10000 random points extracted from the extent
#tryf gives the initial sample size from which random points are drawn after removing NAs - since large parts of the raster are NA, the default tryf value - 3, is insufficient to draw randomPoints
cor.pts<-randomPoints(candidates, 10000,  tryf=50, lonlatCorrection=TRUE)
cor.pts

#saving the coordinates to disk
write.csv(cor.pts, "dataframes/random_pts_ip_13Feb18.csv", row.names=FALSE)

#By default cor.test uses squared Spearman correlation coefficients, Hoeffding's D statistic can also be used which is sensitive to various kinds of relationships including highly non-monotonic relationships
envSpecies<-extract(candidates, cor.pts)
cor.tree<-varclus(envSpecies)
plot(cor.tree)

#Dropping layers that are redundant after looking at cor.test
names(candidates)
predictors<-dropLayer(candidates, c(1,3,5,9:14,16))
names(predictors)
predictors<-setMinMax(predictors)

#writing predictors to disk
setwd("sdm_layers/predictors_ip")
writeRaster(predictors, bylayer=TRUE, filename=names(predictors), format="GTiff", overwrite=TRUE)
setwd("C:/Bharti/PhD data/Analysis/Chapter2_2Nov17") 

#Input predictors from disk
pred_list<-list.files(path="sdm_layers/predictors_ip", pattern="*.tif")
predictors<-stack()
for(i in 1:length(pred_list)){
pred.raster<-raster(paste0("sdm_layers/predictors_ip/", pred_list[i]))
predictors<-stack(predictors, pred.raster)
}

ls()

#rm(all.occ ,bo.raster, bo_layers, bo_layers_fill, bo_list, cand.mask, candidates, cor.tree, distShore, ext, ext.pol, files, files.max, files.min, i, ip.ext, ip.ind.occ, ip.occ.crop, ms.raster, ms_layer, ms_layers_fill, ms_list, napts.tide, tb, tb_slope, test, tidalAmp, tidalAmp.con, tidalAmp.ha, tidalAmp.lat, tidalAmp.lon, tidalMax, tidalMax.r, tidalMax_nc, tidalMin, tidalMin.r, tidalMin_nc, tidalRange.fill, tidalRange.r, tidalRange2.fill, tidalRange2.r, wc.raster, wc_layers_fill, wc_list)

##Creating random background points - Adam Smith code
set.seed(454)
randomBgSites<-randomPoints(predictors, 10000, tryf=1000)
nrow(randomBgSites)
randomBgEnv<-as.data.frame(extract(predictors, randomBgSites))

#combine with coordinates and rename coordinate fields
randomBg<-cbind(randomBgSites, randomBgEnv)
colnames(randomBg)[1:2] <- c('long', 'lat')
nrow(randomBg)
write.csv(randomBg, file="dataframes/randomBg_ip.csv", row.names=FALSE)
#randomBg<-read.csv("dataframes/randomBg.csv")

##Creating a kernel desity smoothing surface for sampling locations (combining locations were littorinids were and were not found) and using this as the probability surface to sample background locations
#Doing this for all sites and for mangrove and rocky shore separately as well

rs.hab<-c("rocky", "mixed")
mg.hab<-c("mangrove", "mixed")

ip.occ<-as.data.frame(ip.occ)
rownames(ip.occ)<-seq(1, nrow(ip.occ), by=1)

rn<-as.numeric(rownames(unique(ip.occ[,2:3])))
ip.unq.occ<-ip.occ[rn,]
nrow(ip.unq.occ)
#write.csv(ip.unq.occ, file="dataframes/ip_unq_occ.csv", row.names=FALSE)

rs.occ<-ip.unq.occ[ip.unq.occ$habitat %in% rs.hab,]
#write.csv(rs.occ, file="dataframes/rs_occ_ip.csv", row.names=FALSE)

plot(ms_layer)
points(rs.occ[,2:3])

mg.occ<-ip.unq.occ[ip.unq.occ$habitat %in% mg.hab, ]
#write.csv(mg.occ, file="dataframes/mg_occ_ip.csv", row.names=FALSE)

plot(ms_layer)
points(mg.occ[,2:3])

#creating the probability surface for all habitats 
#running the kde2d function without specifying the number of cells to calculate extent. Code from - https://scottrinnan.wordpress.com/2015/08/31/how-to-construct-a-bias-file-with-r-for-use-in-maxent-modeling/]

#the rasterize function converts any part of a polygon falling over the center of a raster cell into a raster cell and the field argument transfers the specified value to this new raster cell. I think it transfers the value 1 to each cell where a coordinate occurs. It seems like if two coordinates fall into the same cell, the cell is still given a value of 1. According to Scott Rinnan's guide above - he considers only one coordinate per cell - he uses cells which have a value = 1 from the rasterized coordinates raster as the input into the kde2d function. This gives a result that is different from using all the sampling coordinates directly - the former takes the center of the cell, and ignores multiple coordinates per cell whereas the latter does not. The former also gives negative values with the current data-set which are not acceptable as an input in the maxent function 

kd.ras.fn<-function(x.pt, y.pt, ras){
occur.ras<-rasterize(cbind(x.pt, y.pt), ras, field=1)
presences<-which(values(occur.ras)==1)
pres.locs<-coordinates(occur.ras)[presences,]
kd.ras<-raster(kde2d(pres.locs[,1], pres.locs[,2], n=c(nrow(occur.ras), ncol(occur.ras)), h=0.5))
kd.ras[kd.ras==0]<-NA
kd.ras<-resample(kd.ras, ras)
kd.ras<-raster::mask(kd.ras, ras)
kd.ras
}

#cand.mask<-predictors[[1]]

all.kd<-kd.ras.fn(ip.unq.occ[,2], ip.unq.occ[,3], cand.mask)
all.kd
plot(all.kd)
#writeRaster(all.kd, filename="sdm_layers/bias/all_kd_ip", overwrite=TRUE, format="GTiff")

rs.kd<-kd.ras.fn(rs.occ[,2], rs.occ[,3], cand.mask)
rs.kd
plot(rs.kd)
#writeRaster(rs.kd, filename="sdm_layers/bias/rs_kd_ip", overwrite=TRUE, format="GTiff")

mg.kd<-kd.ras.fn(mg.occ[,2], mg.occ[,3], cand.mask)
mg.kd
plot(mg.kd)
#writeRaster(mg.kd, filename="sdm_layers/bias/mg_kd_ip", overwrite=TRUE, format="GTiff")

#A note that the kd.ras.fn function generates negative values too - turning anything less than 0 to 0.

#Creating background points based on the kernel density probability surface. Doing this for both all sites and also habitat specific probability surface
set.seed(30985)
kdBgSites<-randomPoints(all.kd, 10000, prob=TRUE)
kdBgEnv<-as.data.frame(extract(predictors, kdBgSites))
kdBg<-cbind(kdBgSites, kdBgEnv)
colnames(kdBg)[1:2]<-c("long", "lat")
nrow(kdBg)
write.csv(kdBg, file="dataframes/kdBg_ip.csv", row.names=FALSE)

plot(all.kd)
points(kdBg[,1:2])

set.seed(485743)
kdRockBgSites<-randomPoints(rs.kd, 10000, prob=TRUE, cellnumbers=TRUE)
kdRockBgSites<-xyFromCell(rs.kd, kdRockBgSites)
kdRockBgEnv<-as.data.frame(extract(predictors, kdRockBgSites))
kdRockBg<-cbind(kdRockBgSites, kdRockBgEnv)
colnames(kdRockBg)[1:2]<-c("long", "lat")
nrow(kdRockBg)
write.csv(kdRockBg, file="dataframes/kdRockBg_ip.csv", row.names=FALSE) 

set.seed(9238)
kdMangBgSites<-randomPoints(mg.kd, 10000, prob=TRUE, cellnumbers=TRUE)
kdMangBgSites<-xyFromCell(mg.kd, kdMangBgSites)
kdMangBgEnv<-as.data.frame(extract(predictors, kdMangBgSites))
kdMangBg<-cbind(kdMangBgSites, kdMangBgEnv)
colnames(kdMangBg)[1:2]<-c("long", "lat")
nrow(kdMangBg)
write.csv(kdMangBg, file="dataframes/kdMangBg_ip.csv", row.names=FALSE)

plot(mg.kd)
points(kdMangBg[,1:2])

##Creating another background sites dataframe - where a x km radius is drawn about each of the target species points samples - it is coalesced into a polygon and random background points are drawn from this polygon
#This code is taken from http://www.rspatial.org/sdm/rst/3_sdm_absence-background.html
#Also doing a 20 km radius polygon

#Save sites as a spatial points dataframe - defining which columns in are the coordinates
coordinates(ip.unq.occ)<-~long+lat
projection(ip.unq.occ)<-wgs

coordinates(rs.occ)<-~long+lat
projection(rs.occ)<-wgs

coordinates(mg.occ)<-~long+lat
projection(mg.occ)<-wgs

#Creating circles of radius 20 km around each sampled point
rad<-20000 #20 km radius

pol20<-polygons(circles(ip.unq.occ, d=rad, lonlat=TRUE)) #I think I said all.occ instead of unq.occ for the wg.ext set of models
pol20Rock<-polygons(circles(rs.occ, d=rad, lonlat=TRUE))
pol20Mang<-polygons(circles(mg.occ, d=rad, lonlat=TRUE))

plot(ip.unq.occ)
plot(pol20, col=alpha("blue", 0.2))
plot(pol20Rock, col=alpha("red", 0.2))
plot(pol20Mang, col=alpha("green", 0.2))

#Creating an intersect of the raster stack with the polygons and extracting random points from there
##Creating random background points - Adam Smith code

set.seed(6644)
bufBgSites<-randomPoints(raster::mask(cand.mask, pol20), 10000, tryf=1000)
bufBgEnv<-as.data.frame(extract(predictors, bufBgSites))
bufBg<-cbind(bufBgSites, bufBgEnv)
colnames(bufBg)[1:2]<-c("long", "lat")
write.csv(bufBg, file="dataframes/bufBg_ip.csv", row.names=FALSE)
nrow(bufBgSites)
plot(predictors[[1]])
points(bufBgSites)

set.seed(8457)
bufRockBgSites<-randomPoints(raster::mask(cand.mask, pol20Rock), 10000, tryf=1000)
bufRockBgEnv<-as.data.frame(extract(predictors, bufRockBgSites))
bufRockBg<-cbind(bufRockBgSites, bufRockBgEnv)
colnames(bufRockBg)[1:2]<-c("long", "lat")
write.csv(bufRockBg, file="dataframes/bufRockBg_ip.csv", row.names=FALSE)
nrow(bufRockBgSites)
plot(predictors[[1]])
points(bufRockBgSites)

set.seed(90384)
bufMangBgSites<-randomPoints(raster::mask(cand.mask, pol20Mang), 10000, tryf=2000)
bufMangBgEnv<-as.data.frame(extract(predictors, bufMangBgSites))
bufMangBg<-cbind(bufMangBgSites, bufMangBgEnv)
colnames(bufMangBg)[1:2]<-c("long", "lat")
write.csv(bufMangBg, file="dataframes/bufMangBg_ip_2.csv", row.names=FALSE)
nrow(bufMangBgSites)
plot(predictors[[1]])
points(bufMangBgSites)

#check all.kd points, and buf pts. kd.rs and kd.mg are limited to the Indian coast only for some reason - check the rasters in qgis
####################################
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
####################################









