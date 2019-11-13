library(tidyr)
library(reshape2)
library(dplyr)

### TABLE 1 ###
#input sampling sheet - updated with information 22/05/19
dat<-read.csv(file="D:/PhD data/sample list_vanilla_22May19.csv", header=TRUE)

#looking at the column names and the kind of data contained
head(dat)
colnames(dat)

#subsetting all other species except Conus
#using only columns that are essential for the purpose of focusing on littorinids 
dat<-dat[dat$genus!="Conus",  c(1:2, 10, 11, 13:15, 19:20)]

#checking the columns selected
colnames(dat)
head(dat)

#remove unnecessary locations
rmv<-c(" ", "", "Kochi", "Kochi/Munambam", "not listed anywhere", "Neendakara", "Palayar", "Puthiyappa harbour", "Shakthikulangara", "Vizhinjam harbour", "Kochi harbour", "Manora")
#Though Manora is a mangrove site, it was not accessible and hence removing it from the no occurrence sites

#convert dat$area into character
dat$area<-as.character(dat$area)
dat<-dat[!(dat$area %in% rmv), ]

#changing some of the location names for -
#Koyivila - north western part of Ashtamudi lake, near Kashid, Diu check post, Vanakbara, Diu, Jallandhar beach, Diu, Gangeshwar, Diu
dat$area[dat$area=="Koyivila - north western part of Ashtamudi lake"]<-"Koyivila"
dat$area[dat$area=="near Kashid"]<-"Near Kashid"
dat$area[dat$area=="Vanakbara, Diu"]<-"Vanakbara"
dat$area[dat$area=="Jallandhar beach, Diu"]<-"Jallandhar"
dat$area[dat$area=="Gangeshwar, Diu"]<-"Gangeshwar"

unique(dat$area)
colnames(dat)

#creating a new habitat column
dat$habitat<-rep(NA, times=nrow(dat))

#assigning habitat information to locations
mx<-c("Narpad",  "Ghivali", "Velas", "Near Kashid", "Kelshi", "Harsiddhi/Miyani", "Jakhau")

mg<-c("Pichavaram", "Palackode, Kannur", "Dharmadom_mangrove", "Valapattanam", "Madakkara", "Dharmadom rail", "Koyivila", "Kochi CNG", "Kochi/Puthuvypin", "Ponnani", "Kolavipalam", "Mahe", "Kadalundi bridge", "Kadalundi", "Azad Nagar/Mudipu", "Mulki", "Airody", "Kundapura", "North of Kodi beach", "Gujjadi_mangrove", "Honnavar", "Kumta Masur Road", "Kumta",  "Zuari", "Mandovi", "Tiracol", "Mhartale", "Tank", "Vengurla", "Kolamb", "Achara Bandar", "Morve", "Mith Mumbri", "Phanase", "Shirse", "Purnagad_mangrove", "Ganeshgule_mangrove", "Ranpur", "Maroli_mangrove", "Gholvad",  "Dadarpada", "Near Alibag", "Revdanda", "Divegar", "Near Harihareshwar", "Harihareshwar jetty", "Diu checkpost", "Dwarka checkpost", "Narara", "Tuticorin", "Devipattinam", "Manora", "Erippurakarai", "Pondicherry harbour", "Machilipatnam", "Hamsaladeevi", "Kakinada")

rs<-c("Ettikulam beach", "Dharmadom_rocky", "Thottadam beach", "Azhikode/Chala beach", "Anjugramam", "Kanyakumari", "Muttom", "Kovalam", "Vizhinjam", "Varkala", "Thalakolattur", "Suratkal", "Gujjadi_rocky", "Bhatkal", "Apsarakonda beach", "Gokarna", "Ankola", "Belekere", "Canaguinim", "Vagator", "Arambol", "Varachiker", "Kelus", "Dhuriwada", "Sarjekot", "Kunkeshwar", "Girye", "Vijaydurg", "Ambolgad", "Purnagad_rocky", "Ganeshgule_rocky", "Onjal", "Maroli_rocky", "Varsoi/Alibag", "Murud", "Harihareshwar", "Vanakbara", "Jallandhar", "Gangeshwar", "Sutrapada", "Somnath/Hirakud", "Chorwad", "Madhavpur", "Porbandar", "Okha Madhi beach", "Dwarka", "Okha", "Modhwa", "Mandvi", "Jakhau harbour", "Pingaleshwar", "Tiruchendur", "Mandapam", "Dhanushkodi", "Manamelkudi", "Velankanni", "Pondicherry", "Veerampattinam", "Kovalam_TN", "Yarada", "Rushikonda")

#assigning habitat information to locations
dat$habitat[dat$area %in% mx]<-"mixed"
dat$habitat[dat$area %in% mg]<-"mangrove"
dat$habitat[dat$area %in% rs]<-"rocky"

#checking that there aren't any NAs
unique(dat$habitat)

#convert dat$area back to factor
dat$area<-factor(dat$area)
dat$habitat<-factor(dat$habitat)

#keeping only area, lat, long and habitat columns
dat<-unique(dat[,c(6,8:10)])

#converting lat and lon which are listed as factors into numeric
dat$lat<-as.numeric(as.character(dat$lat))
dat$long<-as.numeric(as.character(dat$long))

#finding out which locations have NA values in lat-long columns
dat[which(is.na(dat$lat)),]

#removing the rows which have NA values
dat<-dat[-(which(is.na(dat$lat))),]
nrow(dat)

#clubbing the different habitat types together
dat<-dat[order(dat$habitat),]

#changing column names
colnames(dat)[1]<-"Area"
colnames(dat)[2]<-"Latitude"
colnames(dat)[3]<-"Longitude"
colnames(dat)[4]<-"Habitat"

#dat$Latitude<-round(dat$Latitude, digits = 4)
#dat$Longitude<-round(dat$Longitude, digits = 4)

#finding out the number of unique areas sampled under each habitat class
length(unique(dat$Area[dat$Habitat=="mangrove"]))
length(unique(dat$Area[dat$Habitat=="rocky"]))
length(unique(dat$Area[dat$Habitat=="mixed"]))

#exporting the dataframe
write.csv(dat, file="D:/PhD data/Analysis/Chapter2_2Nov17/results_figures/SuppInfo_1.1.csv", row.names=FALSE)

### Table 2 ###
#input sampling sheet - updated with information 22/05/19
dat.in<-read.csv(file="D:/PhD data/Analysis/Chapter2_2Nov17/results_figures/in_presence_locations.csv", header=TRUE)

#input no species dataframe
dat.no<-read.csv(file="D:/PhD data/Analysis/Chapter2_2Nov17/dataframes/coords_nosp_12Jan18.csv", header=TRUE)

#looking at a snapshot of the data, and its structure
head(dat.in)
head(dat.no)
str(dat.in)
str(dat.no)

#adding a column called "Genus"
dat.no$Genus<-"none"
colnames(dat.no)

#reordering columns, and keeping only required columns
dat.no<-dat.no[,c(6,1,2,3)]
head(dat.no)

#changing column names to match dat.in
colnames(dat.no)[2]<-"Species"
colnames(dat.no)[3]<-"Longitude"
colnames(dat.no)[4]<-"Latitude"

#adding rows from dat.no to dat.in
dat.in<-rbind(dat.in, dat.no)

#merging dat with dat.in, keeping all rows in dat.in and columns from dat.in and dat
dat.in2<-left_join(dat.in, dat, by=c("Latitude", "Longitude"))
head(dat.in2)

#changing the order of latitude and longitude
dat.in2<-dat.in2[,c(1,2,4,3,5,6)]

#checking if there are duplicate rows
duplicated(dat.in2)

#output the resulting dataframe
write.csv(dat.in2, file="D:/PhD data/Analysis/Chapter2_2Nov17/results_figures/SuppInfo_1.2.csv", row.names=FALSE)

### Table 3 ###
#input sampling sheet for the Indo-Pacific
dat.ip<-read.csv(file="D:/PhD data/Analysis/Chapter2_2Nov17/results_figures/ip_presence_locations.csv", header=TRUE)

#input the original datasheet that has extra information regarding location and source
dat.all<-read.csv(file="D:/PhD data/Analysis/Chapter2_2Nov17/dataframes/species_locations_25Jan18.csv", header=TRUE)
nrow(dat.all)

#create a new reference column
colnames(dat.all)
unique(dat.all[,c(3,4,5)])

#adding the above reference column to dat.all
dat.all$ref<-dat.all$short.title
colnames(dat.all)

#changing the reference information to a formal citation
levels(dat.all$ref)
levels(dat.all$ref)[1]<-"Reid et al. 2006"
levels(dat.all$ref)[2]<-"Rosewater 1970"
levels(dat.all$ref)[3]<-"Reid 2007"
levels(dat.all$ref)[4]<-"Reid et al. 2010"
levels(dat.all$ref)[5]<-"Reid 2001"
levels(dat.all$ref)[6]<-"Reid & Kaiser 2001"
levels(dat.all$ref)[7]<-"Dong et al. 2015"
levels(dat.all$ref)[8]<-"Reid 1984"

dat.all[dat.all$ref=="Reid & Kaiser 2001",]
dat.all[dat.all$Reference=="Reid & Kaiser 2001",]

#converting location information from factor to character
dat.all[,c(6:10)]<-apply(dat.all[,c(6:10)], 2, as.character)
dat.all$area<-rep(NA, nrow(dat.all))

#joining all the location information into one column
for (i in 1:nrow(dat.all)){
temp<-as.character(dat.all[i, c(6:10)])
temp<-temp[-which(temp=="")]
dat.all$area[i]<-paste(temp, collapse=", ")
}

#checking if all the column names are in place and selecting a subset of columns
colnames(dat.all)
dat.all<-dat.all[,c(1,2,13,12,19,15,18)]
#dat.all<-dat.all[,c(1,2,13,12,19,15)]
colnames(dat.all)<-c("Genus", "Species", "Latitude", "Longitude", "Area", "Habitat", "Reference")
#colnames(dat.all)<-c("Genus", "Species", "Latitude", "Longitude", "Area", "Habitat")

#there is some error in the factor levels of Genus in dat.all
levels(dat.all$Genus)
dat.all$Genus<-as.character(dat.all$Genus)
dat.all$Genus[dat.all$Genus=="Littoraria "]<-"Littoraria"
dat.all$Genus<-factor(dat.all$Genus)

#checking if there are duplicated rows in dat.all and removing duplicated rows
duplicated(dat.all)
dat.all[duplicated(dat.all)|duplicated(dat.all, fromLast=TRUE),]
nrow(dat.all)
dat.all<-dat.all[!duplicated(dat.all),]
nrow(dat.all)

#checking if there are duplicated rows in dat.ip
dat.ip[duplicated(dat.ip),]
nrow(dat.ip)

#merging dat.ip with dat.all
colnames(dat.all)
colnames(dat.ip)
dat.ip2<-left_join(dat.ip, dat.all, by=c("Genus", "Species", "Latitude", "Longitude", "Habitat"))

#checking the number of rows in the merged dataframe - looks like there are several more here than before - in search for duplicate values
nrow(dat.ip2)
dat.ip2[duplicated(dat.ip2[,c(1:6)])|duplicated(dat.ip2[,c(1:6)] , fromLast = TRUE)|duplicated(dat.ip2[,c(1:5,7)])|duplicated(dat.ip2[,c(1:5,7)] , fromLast = TRUE),]

#Turns out there are duplicates with slight variations of the area names for the same location and species - removing these duplicates

#Looking at the output above and removing duplicate rows
dat.ip2<-dat.ip2[-c(360,362,400, 402,445,446,448,463,517,580,620,655,691,706,709,715,729),]
nrow(dat.ip2)
nrow(dat.ip)
colnames(dat.ip)
colnames(dat.ip2)

#There still are some extra rows as compared to dat.ip, so investigating if there are duplicates considering the references
dat.ip2[duplicated(dat.ip2[,1:5])|duplicated(dat.ip2[,1:5], fromLast=TRUE),]

#in the above dataframe there are duplicated rows - where there are two instances of the same location information from two different sources. Writing this out to disk after merging it with dat so that it can be modified manually. The modified file ends with suffix "mod" 

#merging dat.ip2 with dat after adding an extra reference column to it
dat2<-dat
dat2$Reference<-rep("This study", times=nrow(dat2))
head(dat2)

head(dat2)
head(dat.ip2)

dat.ip3<-left_join(dat.ip2, dat2, by=c("Latitude", "Longitude"))

#comparing the number of rows of dat.ip2 with dat.ip3
nrow(dat.ip2)
nrow(dat.ip3)

write.csv(dat.ip3, file="D:/PhD data/Analysis/Chapter2_2Nov17/results_figures/SuppInfo_1.3.csv", row.names=FALSE)

#Inputing the modified dat.ip3 file and to look for duplicates
dat.ip4<-read.csv(file="D:/PhD data/Analysis/Chapter2_2Nov17/results_figures/SuppInfo_1.3_mod.csv", header=TRUE)

#There were some duplicated strigata locations - because these belonged to mixed sites in my sampling - but they were used both in rocky shore and mangrove sites separately and hence the duplicates. Changed the habitat from mixed to mangrove and rocky in each.
dat.ip4[duplicated(dat.ip4)|duplicated(dat.ip4, fromLast=TRUE),]
duplicated(dat.ip4)

#There were no duplicates once the above correction was made. To conserve the order of points as dat.ip - left_joining dat.ip4 with dat.ip. I'm excluding the habitat column from dat.ip - because there is a "mixed" level in dat.ip4 that is missing from dat.ip
duplicated(dat.ip4)

colnames(dat.ip4)
colnames(dat.ip)

#The order of rows in dat.ip4 is the same as dat.ip - so don't need to do anything about it
identical(dat.ip4[,1:4], dat.ip[,1:4])

#changing the order of latitude and longitude in SuppInfo_1.3_mod.csv directly in Excel

#cross-checking the reference information for the table in the manuscript
unique(dat.ip4$Reference[dat.ip4$Species=="mangrove"]))
table(dat.ip4$Species, dat.ip4$Reference)



the family littorinidae in the indo pacific rosewater











library(tidyr)
library(reshape2)
library(dplyr)

#set working directory
setwd("D:/PhD data/Analysis/Chapter1_16Oct17")

#input sampling location information
sam.loc<-read.csv("results/table1_sampling locations_16S.csv")

#saving habitat related information
mg<-c("ben","car","pal","mel")
rs<-c("und", "mal", "oma", "vid", "leu")
mx<-c("Harsiddhi/Miyani","Jakhau","Kelshi", "Purnagad", "Ghivali", "Narpad", "Velas")

mg.str<-c("Erippurakarai", "Kakinada", "Machilipatnam", "Mandovi", "Narara", "Revdanda")

rs.str<-c("Ambolgad", "Ankola", "Arambol", "Belekere", "Harihareshwar","Jakhau harbour", "Jallandhar beach, Diu", "Manamelkudi", "Mandvi", "Modhwa", "Murud", "Okha", "Porbandar", "Rushikonda", "Varsoi/Alibag", "Velankanni")

#creating a habitat column
sam.loc$habitat<-rep(NA, times=nrow(sam.loc))
sam.loc$habitat[sam.loc$species %in% mg]<-"mangrove"
sam.loc$habitat[sam.loc$species %in% rs]<-"rocky"
sam.loc$habitat[sam.loc$area%in% mx]<-"mixed"
sam.loc$habitat[sam.loc$species=="str" & sam.loc$area %in% mg.str]<-"mangrove"
sam.loc$habitat[sam.loc$species=="str" & sam.loc$area %in% rs.str]<-"rocky"

#creating a table with location names and species names
sam.loc2<-as.data.frame(table(sam.loc$area, sam.loc$species))
colnames(sam.loc2)<-c("area", "species", "count")
sam.loc2<-dcast(sam.loc2, formula=area~species)

#putting together location information
loc<-sam.loc[,c(3:5)]
loc<-aggregate(loc[,c(2:3)], by=list(loc$area), FUN=mean)
colnames(loc)[1]<-"area"

#left join sam.loc2 with loc
sam.df<-left_join(sam.loc2, loc, by="area")
sam.df<-unique(left_join(sam.loc[,c(3, 6,7,9)], sam.df,  by="area"))
sam.df$SNo<-seq(from=1, to=nrow(sam.df), by=1)
head(sam.df)
colnames(sam.df)

#arranging column names in sam.df
sam.rs.df<-sam.df[sam.df$habitat=="rocky", c(17,1,15,16,2,3,13,12,8,7,14,10)]
e<-sam.rs.df[sam.rs.df$coast=="east",]
e<-e[order(e$lat, decreasing=FALSE),]
w<-sam.rs.df[sam.rs.df$coast=="west",]
w<-w[order(w$lat, decreasing=TRUE),]

sam.rs.df2<-rbind(w,e)
sam.rs.df2$SNo<-seq(from=1, to=nrow(sam.rs.df2), by=1)

#saving to disk
write.csv(sam.rs.df2, file="results/table1_rocky_locations_16S.csv", row.names=FALSE)

sam.mg.df<-sam.df[sam.df$habitat=="mangrove" | sam.df$habitat=="mixed" , c(17,1,15,16,2,3,5,12,6,11,9)]
e<-sam.mg.df[sam.mg.df$coast=="east",]
e<-e[order(e$lat, decreasing=FALSE),]
w<-sam.mg.df[sam.mg.df$coast=="west",]
w<-w[order(w$lat, decreasing=TRUE),]

sam.mg.df2<-rbind(w,e)
sam.mg.df2$SNo<-seq(from=1, to=nrow(sam.mg.df2), by=1)

#saving to disk
write.csv(sam.mg.df2, file="results/table1_mangrove_locations_16S.csv", row.names=FALSE)
