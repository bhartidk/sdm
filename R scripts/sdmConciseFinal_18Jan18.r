setwd("D:/PhD data/Analysis/Chapter2_2Nov17")

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

##########################INPUT################################
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

#extracting occurrence data for each species and saving it as a separate dataframe
sp<-as.character(levels(all.occ$species))

#creating a list where each element is a species specific dataframe
ind.occ<-list()
for(i in 1:length(sp)){
ind.occ[[i]]<-subset(all.occ, all.occ$species %in% sp[i])
names(ind.occ)[[i]]<-paste0(substr(sp[i], 1, 3), ".occ") 
}

#dissolving the list to create indimalaccanal dataframes that have been appropriately labeled
list2env(ind.occ, envir=.GlobalEnv)

#List all objects in the workspace to see if the dataframes have been stored
ls()

#creating a dataframe which has unique values of all.occ
rn<-as.numeric(rownames(unique(all.occ[,2:3])))
unq.occ<-all.occ[rn,]

#adding other sampling locations where no species were found but were a part of the sampling effort to this dataframe
no.occ<-read.csv("dataframes/coords_nosp_12Jan18.csv")

unq.occ<-rbind(unq.occ, no.occ)
nrow(unq.occ)

##Take the records of the focal species
n<-"und.occ"
sp.occ<-ind.occ[[n]]
sp_name<-"undulata"

#Creating a variable which saves the WGS projection codes
wgs<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Converting long and lat values to a spatial points file
recordsSpatial<-SpatialPointsDataFrame(coords=cbind(sp.occ$long, sp.occ$lat), data=sp.occ, proj4string=CRS(wgs))

plot(recordsSpatial, pch=19, col="blue")

#Input predictors from disk
pred_list<-list.files(path="sdm_layers/predictors", pattern="*.tif")
predictors<-stack()

for(i in 1:length(pred_list)){
pred.raster<-raster(paste0("sdm_layers/predictors/", pred_list[i]))
predictors<-stack(predictors, pred.raster)
}

#Extracting values for records from layers in the predictors stack
envSpecies<-extract(predictors, recordsSpatial)
records<-cbind(sp.occ[,2:3], envSpecies)

#Removing multiple records from the same cell - Adam Smith code
#inputing the function
source('./R scripts/Eliminate Points in Same Cell of a Raster.r')

recordsNoDups<-elimCellDups(records, predictors[[1]], longLatFields=c('long', 'lat'), priority=NULL)
nrow(records)
nrow(recordsNoDups)
write.csv(recordsNoDups, file=paste0("results/", sp_name, "/occ.csv"), row.names=FALSE)

#Plot extracted values. Looking at any outliers in data by plotting one variable against another. Code from here - https://stackoverflow.com/questions/13035834/plot-every-column-in-a-data-frame-as-a-histogram-on-one-page-using-ggplot - I don't malerstand the steps very well though - need to take some time out to look at this
jpeg(file=paste0("results/", sp_name, "/hist.jpeg"),  height=10, width = 10, units = 'in', res=300)
d <- melt(recordsNoDups[,c(3:ncol(recordsNoDups))])
ggplot(d,aes(x = value)) +  facet_wrap(~variable,scales = "free_x") + 
geom_histogram()
dev.off()

#inputing backgromal sites
randomBg<-read.csv("dataframes/randomBg.csv")
kdBg<-read.csv("dataframes/kdBg.csv")
kdRockBg<-read.csv("dataframes/kdRockBg.csv")
kdMangBg<-read.csv("dataframes/kdMangBg.csv")
bufBg<-read.csv("dataframes/bufBg.csv")
bufRockBg<-read.csv("dataframes/bufRockBg.csv")
bufMangBg<-read.csv("dataframes/bufMangBg.csv")

######################MODEL INPUT FILES########################
##Maxent model using random background points
trainData<-rbind(recordsNoDups, randomBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(randomBg)))

randomBgIn<-cbind(trainData, presentBg)
write.csv(randomBgIn, file=paste0("results/", sp_name, "/randomBg_input.csv"), row.names=FALSE)

##Maxent model using kernel density based points 
trainData<-rbind(recordsNoDups, kdBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(kdBg)))

kdBgIn<-cbind(trainData, presentBg)

write.csv(kdBgIn, file=paste0("results/", sp_name, "/kdBg_input.csv"), row.names=FALSE)

##Maxent model using kernel density based points - but for habitat specific background sites only
kdHabBg<-kdRockBg 
trainData<-rbind(recordsNoDups, kdHabBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(kdHabBg)))

kdHabBgIn<-cbind(trainData, presentBg)

write.csv(kdHabBgIn, file=paste0("results/", sp_name, "/kdHabBg_input.csv"), row.names=FALSE)

##Maxent model using background sites from 20 km buffer
trainData<-rbind(recordsNoDups, bufBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(bufBg)))

bufBgIn<-cbind(trainData, presentBg)

write.csv(bufBgIn, file=paste0("results/", sp_name, "/bufBg_input.csv"), row.names=FALSE)

##Maxent model using buffer based points - but for habitat specific backgromal sites only
bufHabBg<-bufRockBg 
trainData<-rbind(recordsNoDups, bufHabBg)
presentBg<-c(rep(1, times=nrow(recordsNoDups)), rep(0, times=nrow(bufHabBg)))

bufHabBgIn<-cbind(trainData, presentBg)

write.csv(bufHabBgIn, file=paste0("results/", sp_name, "/bufHabBg_input.csv"), row.names=FALSE)

###################BETA REGULARIZATION#########################
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

beta.val<-seq(1,5, by=1)

randomBg_AIC<-beta.AIC(randomBgIn, beta.val, predictors, recordsNoDups[,1:2])
randomBg_beta<-randomBg_AIC$beta.val[which.min(randomBg_AIC$AICc)]

kdBg_AIC<-beta.AIC(kdBgIn, beta.val, predictors, recordsNoDups[,1:2])
kdBg_beta<-kdBg_AIC$beta.val[which.min(kdBg_AIC$AICc)]

kdHabBg_AIC<-beta.AIC(kdHabBgIn, beta.val, predictors, recordsNoDups[,1:2])
kdHabBg_beta<-kdHabBg_AIC$beta.val[which.min(kdHabBg_AIC$AICc)]

bufBg_AIC<-beta.AIC(bufBgIn, beta.val, predictors, recordsNoDups[,1:2])
bufBg_beta<-bufBg_AIC$beta.val[which.min(bufBg_AIC$AICc)]

bufHabBg_AIC<-beta.AIC(bufHabBgIn, beta.val, predictors, recordsNoDups[,1:2])
bufHabBg_beta<-bufHabBg_AIC$beta.val[which.min(bufHabBg_AIC$AICc)]

beta_list<-c(randomBg_beta, kdBg_beta, kdHabBg_beta, bufBg_beta, bufHabBg_beta)
#beta_list<-c(randomBg_beta, kdBg_beta, bufBg_beta)

sink(paste0("results/", sp_name, "/AIC.txt"))
cat(sprintf("randomBg\n"))
print(randomBg_AIC)
cat(sprintf("kdBg\n"))
print(kdBg_AIC)
cat(sprintf("kdHabBg\n"))
print(kdHabBg_AIC)
cat(sprintf("bufBg\n"))
print(bufBg_AIC)
cat(sprintf("bufHabBg\n"))
print(bufHabBg_AIC)
cat(sprintf("betas\n"))
print(beta_list)
sink()

##########################MODEL################################
#randomBg_beta<-1
#kdBg_beta<-4
#kdHabBg_beta<-5
#bufBg_beta<-1
#bufHabBg_beta<-1

beta_list<-c(randomBg_beta, kdBg_beta, kdHabBg_beta, bufBg_beta, bufHabBg_beta)
#beta_list<-c(randomBg_beta, kdBg_beta, bufBg_beta)

##Maxent model using random background points
randomBgModel<-maxent(x=randomBgIn[,3:(ncol(randomBgIn)-1)], p=randomBgIn[,ncol(randomBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', paste0("betamultiplier=", randomBg_beta)))

jpeg(file=paste0("results/", sp_name, "/randomBgvarImp.jpeg"), height=10, width = 10, units = 'in', res=300)
plot(randomBgModel)
dev.off()

write.csv(randomBgModel@results, paste0("results/", sp_name, "/randomBgModel.csv"))

save(randomBgModel, file=paste0("results/", sp_name, "/randomBgModel"))

jpeg(file=paste0("results/", sp_name, "/randomBgResponse.jpeg") ,  height=10, width = 10, units = 'in', res=300)
response(randomBgModel, range='p')
dev.off()

randomBgMap<-predict(randomBgModel, predictors)
writeRaster(randomBgMap, filename=paste0("results/", sp_name, "/randomBgMap"), format="GTiff", overwrite=TRUE)

##Maxent model using kernel density based points 
kdBgModel<-maxent(x=kdBgIn[,3:(ncol(kdBgIn)-1)], p=kdBgIn[,ncol(kdBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', paste0("betamultiplier=", kdBg_beta)))

write.csv(kdBgModel@results, paste0("results/", sp_name, "/kdBgModel.csv"))

jpeg(file=paste0("results/", sp_name, "/kdBgvarImp.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(kdBgModel)
dev.off()

save(kdBgModel, file=paste0("results/", sp_name, "/kdBgModel"))

jpeg(file=paste0("results/", sp_name, "/kdBgResponse.jpeg"),  height=10, width = 10, units = 'in', res=300)
response(kdBgModel, range='p')
dev.off()

kdBgMap<-predict(kdBgModel, predictors)
writeRaster(kdBgMap, filename=paste0("results/", sp_name, "/kdBgMap"), format="GTiff", overwrite=TRUE)

##Maxent model using kernel density based points - but for habitat specific backgromal sites only
kdHabBgModel<-maxent(x=trainData[,3:(ncol(kdHabBgIn)-1)], p=kdHabBgIn[,ncol(kdHabBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false',  paste0("betamultiplier=", kdHabBg_beta)))

write.csv(kdHabBgModel@results, paste0("results/", sp_name, "/kdHabBgModel.csv"))

jpeg(file=paste0("results/", sp_name, "/kdHabBgvarImp.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(kdHabBgModel)
dev.off()

save(kdHabBgModel, file=paste0("results/", sp_name, "/kdHabBgModel"))

jpeg(file=paste0("results/", sp_name, "/kdHabBgResponse.jpeg"),  height=10, width = 10, units = 'in', res=300)
response(kdHabBgModel, range='p')
dev.off()

kdHabBgMap<-predict(kdHabBgModel, predictors)
plot(kdHabBgMap)
writeRaster(kdHabBgMap, filename=paste0("results/", sp_name, "/kdHabBgMap"), format="GTiff", overwrite=TRUE)

##Maxent model using backgromal sites from 20 km buffer
bufBgModel<-maxent(x=bufBgIn[,3:(ncol(bufBgIn)-1)], p=bufBgIn[,ncol(bufBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', paste0("betamultiplier=", bufBg_beta)))

jpeg(file=paste0("results/", sp_name, "/bufBgvarImp.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(bufBgModel)
dev.off()

write.csv(bufBgModel@results, paste0("results/", sp_name, "/bufBgModel.csv"))
save(bufBgModel, file=paste0("results/", sp_name, "/bufBgModel"))

jpeg(file=paste0("results/", sp_name, "/bufBgResponse.jpeg"), height=10, width = 10, units = 'in', res=300)
response(bufBgModel, range='p')
dev.off()

bufBgMap<-predict(bufBgModel, predictors)

writeRaster(bufBgMap, filename=paste0("results/", sp_name, "/bufBgMap"), format="GTiff", overwrite=TRUE)

##Maxent model using buffer based points - but for habitat specific backgromal sites only
bufHabBgModel<-maxent(x=bufHabBgIn[,3:(ncol(bufHabBgIn)-1)], p=bufHabBgIn[,ncol(bufHabBgIn)], args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', paste0("betamultiplier=", bufHabBg_beta)))

write.csv(bufHabBgModel@results, paste0("results/", sp_name, "/bufHabBgModel_copy.csv"))

jpeg(file=paste0("results/", sp_name, "/bufHabBgvarImp.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(bufHabBgModel)
dev.off()

save(bufHabBgModel, file=paste0("results/", sp_name, "/bufHabBgModel"))

jpeg(file=paste0("results/", sp_name, "/bufHabBgResponse.jpeg"), height=10, width = 10, units = 'in', res=300)
bufHabBgResponse<-response(bufHabBgModel, range='p')
dev.off()

bufHabBgMap<-predict(bufHabBgModel, predictors)
writeRaster(bufHabBgMap, filename=paste0("results/", sp_name, "/bufHabBgMap"), format="GTiff", overwrite=TRUE)

########################JACKKNIFING############################
#Code for jackknifing over variables
mod.input<-list(randomBgIn, kdBgIn, kdHabBgIn, bufBgIn, bufHabBgIn)
names(mod.input)<-c("randomBgIn", "kdBgIn", "kdHabBgIn", "bufBgIn", "bufHabBgIn")

#mod.input<-list(randomBgIn, kdBgIn, bufBgIn)
#names(mod.input)<-c("randomBgIn", "kdBgIn", "bufBgIn")
#beta_list<-c(1,1,1)

for (j in 1:length(mod.input)){
input<-mod.input[[j]]
env.input<-input[,3:(ncol(input)-1)]
pres.input<-input[,ncol(input)]
modGain<-rep(NA, times=ncol(env.input))

for(i in 1:ncol(env.input)){
exModel<-maxent(x=env.input[,-i], p=pres.input, args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', paste0("betamultiplier=", beta_list[j])))
modGain[i]<-exModel@results[2]
}

fullModel<-maxent(x=env.input, p=pres.input, args=c('linear=true', 'quadratic=true', 'product=true',
'threshold=false', 'hinge=false', paste0("betamultiplier=", beta_list[j])))
modGain[ncol(env.input)+1]<-fullModel@results[2]
pred<-c(colnames(env.input), "total")

jpeg(file = paste0("results/", sp_name,"/", names(mod.input)[j], "JK.jpeg"),  height=10, width = 10, units = 'in', res=300)
area.colour <- c(rep("1", times=ncol(env.input)), "2")
p<-ggplot() + geom_bar(aes(x=pred, y=modGain, fill=area.colour), stat='identity', width=0.75)  + xlab("Predictor variable") + ylab("Regularised gain") + scale_fill_manual(values=c("gray50", "red"), guide=FALSE) + coord_flip()
print(p)
dev.off()
}

#############################ROC###############################
randomBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=randomBg[,3:ncol(randomBg)], randomBgModel)
jpeg(file=paste0("results/", sp_name, "/randomBgROC.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(randomBgEval, 'ROC')
dev.off()
jpeg(file=paste0("results/", sp_name, "/randomBgDens.jpeg"),  height=10, width = 20, units = 'in', res=300)
par(mfrow=c(1,2))
boxplot(randomBgEval)
density(randomBgEval)
dev.off()
save(randomBgEval, file=paste0("results/", sp_name, "/randomBgEval"))

kdBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=kdBg[,3:ncol(kdBg)], kdBgModel)
jpeg(file=paste0("results/", sp_name, "/kdBgROC.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(kdBgEval, 'ROC')
dev.off()
jpeg(file=paste0("results/", sp_name, "/kdBgDens.jpeg"),  height=10, width = 20, units = 'in', res=300)
par(mfrow=c(1,2))
boxplot(kdBgEval, notch=FALSE)
density(kdBgEval)
dev.off()
save(kdBgEval, file=paste0("results/", sp_name, "/kdBgEval"))

kdHabBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=kdHabBg[,3:ncol(kdHabBg)], kdHabBgModel)
jpeg(file=paste0("results/", sp_name, "/kdHabBgROC.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(kdHabBgEval, 'ROC')
dev.off()
jpeg(file=paste0("results/", sp_name, "/kdHabBgDens.jpeg"),  height=10, width = 20, units = 'in', res=300)
par(mfrow=c(1,2))
boxplot(kdHabBgEval, notch=FALSE)
density(kdHabBgEval)
dev.off()
save(kdHabBgEval, file=paste0("results/", sp_name, "/kdHabBgEval"))

bufBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=bufBg[,3:ncol(bufBg)], bufBgModel)
jpeg(file=paste0("results/", sp_name, "/bufBgROC.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(bufBgEval, 'ROC')
dev.off()
jpeg(file=paste0("results/", sp_name, "/bufBgDens.jpeg"),  height=10, width = 20, units = 'in', res=300)
par(mfrow=c(1,2))
boxplot(bufBgEval, notch=FALSE)
density(bufBgEval)
dev.off()
save(bufBgEval, file=paste0("results/", sp_name, "/bufBgEval"))

bufHabBgEval<-evaluate(p=recordsNoDups[,3:ncol(recordsNoDups)], a=bufHabBg[,3:ncol(bufHabBg)], bufHabBgModel)
jpeg(file=paste0("results/", sp_name, "/bufHabBgROC.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(bufHabBgEval, 'ROC')
dev.off()
jpeg(file=paste0("results/", sp_name, "/bufHabBgDens.jpeg"),  height=10, width = 20, units = 'in', res=300)
par(mfrow=c(1,2))
boxplot(bufHabBgEval, notch=FALSE)
density(bufHabBgEval)
dev.off()
save(bufHabBgEval, file=paste0("results/", sp_name, "/bufHabBgEval"))

#######################kfold crossvalidation###################
##Dividing presence data into 5 groups - using 4/5ths for training and 1/5ths for test. Code from Hijmans and Elith 2017

#For each of the folds , dimaling data into test and training data, combining test data with backgromal data and giving presence codes to the rows. Running the model using training data, keeping the features linear, quadratic and product.

#Here k refers to the number of k folds, fold refers to the k fold group, pres is the presence data dataframe and bg is the backgromal data dataframe

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
 
#Running the k-folds evaluation 
randomBgKfold<-maxent_kfold(5, recordsNoDups, randomBg, randomBg_beta)
kdBgKfold<-maxent_kfold(5, recordsNoDups, kdBg, kdBg_beta)
kdHabBgKfold<-maxent_kfold(5, recordsNoDups, kdHabBg, kdHabBg_beta)
bufBgKfold<-maxent_kfold(5, recordsNoDups, bufBg, bufBg_beta)
bufHabBgKfold<-maxent_kfold(5, recordsNoDups, bufHabBg, bufHabBg_beta)

sink(paste0("results/", sp_name, "/kfold.txt"))
cat(sprintf("randomBg\n"))
print(randomBgKfold)
cat(sprintf("kdBg\n"))
print(kdBgKfold)
cat(sprintf("kdHabBg\n"))
print(kdHabBgKfold)
cat(sprintf("bufBg\n"))
print(bufBgKfold)
cat(sprintf("bufHabBg\n"))
print(bufHabBgKfold)
sink()

#AUC value tells you the probability that if you pick a random presence and absence, the presence has been given a higher relative occurrence rate than the absence. But what sense does it make for pseudoabsences.

#the cor values are very low - cor is the correlation coefficient for predicted value of environmental suitability for a cell and the actual status of the cell (presence or absence)

#######################RESIDUALS PATTERN#######################
#Looking if the residuals have a spatial patterning
modelMap<-stack(randomBgMap, kdBgMap, kdHabBgMap, bufBgMap, bufHabBgMap)
names(modelMap)<-c("randomBg", "kdBg", "kdHabBg", "bufBg", "bufHabBg")

#modelMap<-stack(randomBgMap, kdBgMap, bufBgMap)
#names(modelMap)<-c("randomBg", "kdBg", "bufBg")

for(i in 1:nlayers(modelMap)){
predict.pres<-raster::extract(modelMap[[i]], recordsNoDups[,1:2])
residual<-1-predict.pres
predict.pres<-cbind(recordsNoDups[,1:2], residual)

mycol<-colorRampPalette(c("blue", "red"))
jpeg(file = paste0("results/", sp_name, "/", names(modelMap)[i], "Res.jpeg"),  height=10, width = 10, units = 'in', res=300)
plot(modelMap[[i]],  main="Residuals - Model")
points(predict.pres, pch=19, col=mycol(10), cex=0.5)
dev.off()
}

########################MESS MAP###############################
#Creating MESS map to see where combination of environmental variables is not the same as the sampled location data
#Input predictors from disk

#all.unq.occ<-rbind(unq.occ[,2:3], no.occ[, 2:3])
#sampled.env<-raster::extract(predictors, all.unq.occ)
#messIn<-cbind(all.unq.occ, sampled.env)
#head(messIn)

#messMap<-dismo::mess(predictors, messIn[,3:ncol(messIn)], full=FALSE)
#plot(messMap)
#writeRaster(messMap, filename="results/messMap", format="GTiff")

#####################LIMITING FACTOR###########################
#creating a limiting variable map for the model
randomBgLim<-limiting(predictors, randomBgModel)
writeRaster(randomBgLim, filename=paste0("results/", sp_name, "/randomBgLim"), format="GTiff", overwrite=TRUE)

#creating a limiting variable map for the model
kdBgLim<-limiting(predictors, kdBgModel)
writeRaster(kdBgLim, filename=paste0("results/", sp_name, "/kdBgLim"), format="GTiff", overwrite=TRUE)

#creating a limiting variable map for the model
kdHabBgLim<-limiting(predictors, kdHabBgModel)
writeRaster(kdHabBgLim, filename=paste0("results/", sp_name, "/kdHabBgLim"), format="GTiff", overwrite=TRUE)

#creating a limiting variable map for the model
bufBgLim<-limiting(predictors, bufBgModel)
writeRaster(bufBgLim, filename=paste0("results/", sp_name, "/bufBgLim"), format="GTiff", overwrite=TRUE)

#creating a limiting variable map for the model
bufHabBgLim<-limiting(predictors, bufHabBgModel)
writeRaster(bufHabBgLim, filename=paste0("results/", sp_name, "/bufHabBgLim"), format="GTiff", overwrite=TRUE)
###############################################################

