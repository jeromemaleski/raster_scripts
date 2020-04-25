#################################
## Image analysis RGB to VARI GLI
#################################

#################################
## Zonal statistics #############
#################################

# rm(list=ls(all=TRUE))
# 
# setwd("C:\\Users\\jzhang06\\OneDrive - University of Georgia\\Jing Zhang UAV files\\SCRI\\DJIP4_20190605\\pix4d_process\\3_dsm_ortho\\2_mosaic")
# getwd()
# 
# setwd("D:\\Jing Zhang UAV files after June 12 2019\\SCRI_After_20190612\\DJIP4_20190614\\pix4d_process\\3_dsm_ortho\\2_mosaic")
# getwd()

library(raster)
library(rgdal)
library(maptools)
library(foreign)

# Loop through folder Images folder under working directory, return the names of the files in the same flight date
# files<-list.files(path = "./Mavic_Matrice_RGB",pattern='tif', full.names = TRUE, recursive=TRUE)
# names<-list.files(path = "./Mavic_Matrice_RGB",pattern='tif', full.names = FALSE, recursive=TRUE)
# names<-sub("\\..*","",names)
# dates<-as.Date(sub("2019.*","2019",substring(names,6)),format="%d%b")
files<-list.files(path = "./rep2RGB",pattern='JPG', full.names = TRUE, recursive=TRUE)
names<-list.files(path = "./rep2RGB",pattern='JPG', full.names = FALSE, recursive=TRUE)
names<-sub("\\..*","",names)
dates<-as.Date(sub("2019.*","2019",substring(names,6)),format="%d%b")



### Defining color indices
# Old functions - slow, takes 10 min for each raster calculation
# GLI <- function(img,i,j,k)
# {
#   r<-img[[i]]/(img[[i]]+img[[j]]+img[[k]]);
#   g<-img[[j]]/(img[[i]]+img[[j]]+img[[k]]);
#   b<-img[[k]]/(img[[i]]+img[[j]]+img[[k]]);
#   GLI <- (2*g-b-r)/(2*g+b+r);
#   return(GLI);
# }
# VARI <-  function(img,i,j,k)
# {
#   r<-img[[i]]/(img[[i]]+img[[j]]+img[[k]]);
#   g<-img[[j]]/(img[[i]]+img[[j]]+img[[k]]);
#   b<-img[[k]]/(img[[i]]+img[[j]]+img[[k]]);
#   VARI <- (g-r)/(g+r-b);
#   return(VARI);
# }
# ExGR <- function(img,i,j,k)
# {
#   r<-img[[i]]/(img[[i]]+img[[j]]+img[[k]]);
#   g<-img[[j]]/(img[[i]]+img[[j]]+img[[k]]);
#   b<-img[[k]]/(img[[i]]+img[[j]]+img[[k]]);
#   ExGR<-3*g-2.4*r-b
#   return(ExGR)
# }
# New functions - fast, takes 1 min for each raster calculation
GLI <- function(img,i,j,k){
  r<-getValues(img[[i]])
  g<-getValues(img[[j]])
  b<-getValues(img[[k]])
  sumras<-r+g+b
  r1<-r/sumras;
  g1<-g/sumras;
  b1<-b/sumras;
  GLI <- (2*g1-b1-r1)/(2*g1+b1+r1);
  GLI_ras<-img[[1]];
  values(GLI_ras)<-GLI
  return(GLI_ras);
}
VARI <-  function(img,i,j,k){
  r<-getValues(img[[i]])
  g<-getValues(img[[j]])
  b<-getValues(img[[k]])
  sumras<-r+g+b
  r1<-r/sumras;
  g1<-g/sumras;
  b1<-b/sumras;
  VARI <- (g1-r1)/(g1+r1-b1);
  VARI_ras<-img[[1]];
  values(VARI_ras)<-VARI
  return(VARI_ras);
}

######################################
# Raster Calculation #################
# Write Raster File - VARI and GLI ###
# Calculation of each color index takes about 10 min
# Need improvement ###################
######################################

# length(files)
# files[4]
#i=1

for (i in seq_along(files)){

my_raster<-brick(files[i])
GLI_1<-GLI(my_raster,1,2,3) 
VARI_1<-VARI(my_raster,1,2,3)
writeRaster(GLI_1,paste0("image2/pic/GLI_",names[i],".tiff"),overwrite=TRUE)
writeRaster(VARI_1,paste0("image2/pic/VARI_",names[i],".tiff"),overwrite=TRUE)
}

#############################
# Zonal statiscis 3 - TRY ###
# Average 2 min per raster ##
#############################
library(tidyr)
library(dplyr)
# Zonal statistics function define
# Clip and Extract the extent of each polygon in the shapefile and returns a list "e"
# Clip raster using the polygon extents and returns a list "raster.crop"
# transfer pixel values to dataframes
# Merge into one data file
zonal_vector<-function(poly,raster,index,ID){
  e<-list()
  for (i in 1:length(poly)){
    e[[i]]<-extent(poly[i,])
  }
  raster.crop<-list()
  for (i in 1:length(e)){
    raster.crop[[i]]<-crop(my_raster,e[[i]])
  }
  vector<-list()
  for (i in 1:length(raster.crop)){
    vector[[i]]<-data.frame(getValues(raster.crop[[i]]))
    vector[[i]]$PID<-i
  }
  index_raw<-do.call(rbind,vector)
  names(index_raw)<-c(index,ID)
  return(index_raw)
}
GLI_raster_20180924<-my_raster

my_raster<-raster(files[1]) # Specify where the raster is

#my_raster<-GLI_raster_20181022
index<-"GLI"                # Specify which color index
Date<-"20190605"            # Specify the date
species<-"zoysia hybrid"            # Specify the species
ID<-"Orig_ID"
# Read shape files "Bermuda_All"; "ZOY_All"; "Seashore_All"; "STA_All"; "Zoysia_Hybrids"
poly<-
  readOGR("C:\\Users\\jzhang06\\OneDrive - University of Georgia\\Jing Zhang UAV files\\SCRI\\ArcGIS","Zoysia_Hybrids")
dbfdata<-
  read.dbf("C:\\Users\\jzhang06\\OneDrive - University of Georgia\\Jing Zhang UAV files\\SCRI\\ArcGIS\\Zoysia_Hybrids.dbf")
head(dbfdata)
dbfdata$ORIG_FID<-dbfdata$ORIG_FID+1
head(dbfdata)
# Zonal statistics and return a vector
index_raw<-zonal_vector(poly,my_raster,index,ID)
# Merge attributes based on Orig_ID
index_raw<-full_join(index_raw,dbfdata,by=ID)
# Add Date information to the dataframe
index_raw$Date<-Date
# Summarize the pixel values within each field plot
index_mean_sd<-summarise(group_by(index_raw,Id),GLI_mean=mean(GLI),GLI_sd=sd(GLI))
# Change the header of the data file
names(index_mean_sd)<-c(ID,"GLI_mean","GLI_sd")
# Add new ID and Entry columns to the target file
# Read plot layout file
layout_bermuda<-read.csv("C:\\Users\\jzhang06\\OneDrive - University of Georgia\\Jing Zhang UAV files\\SCRI\\ArcGIS\\bermuda Shapefile and plotID conversion.csv",header = TRUE,na.strings = c("","NA","."))
layout_zoysia<-read.csv("C:\\Users\\jzhang06\\OneDrive - University of Georgia\\Jing Zhang UAV files\\SCRI\\ArcGIS\\zoysia Shapefile and plotID conversion.csv",header = TRUE,na.strings = c("","NA","."))
layout_seashore<-read.csv("C:\\Users\\jzhang06\\OneDrive - University of Georgia\\Jing Zhang UAV files\\SCRI\\ArcGIS\\seashore Shapefile and plotID conversion.csv",header = TRUE,na.strings = c("","NA","."))
layout_staug<-read.csv("C:\\Users\\jzhang06\\OneDrive - University of Georgia\\Jing Zhang UAV files\\SCRI\\ArcGIS\\staug Shapefile and plotID conversion.csv",header = TRUE,na.strings = c("","NA","."))
layout_hybrid<-read.csv("C:\\Users\\jzhang06\\OneDrive - University of Georgia\\Jing Zhang UAV files\\Zoysiagrass_Germplasm\\Plot_layout.csv",header = TRUE,na.strings = c("","NA","."))

layout_hybrid$Orig_ID<-layout_hybrid$Id_Arc
index_mean_sd<-full_join(index_mean_sd,layout_hybrid,by=ID) # INPUT SPECIES
index_mean_sd$Date<-Date
write.csv(index_mean_sd,paste(species,index,Date,'.csv'))


###########################
## Data Merging ###########
## ggplot #################
###########################

setwd("C:\\Users\\jzhang06\\OneDrive - University of Georgia\\Jing Zhang UAV files\\SCRI")
getwd()

# Loop through folder Images folder under working directory, return the names of the files in the same flight date
files2<-list.files(pattern='zoysia hybrid GLI', full.names = TRUE, recursive=TRUE)

files2
df<-read.csv(files2[1],header = TRUE,na.strings = c("","NA","."))

for (i in 2:12){
  df0<-read.csv(files2[i],header = TRUE,na.strings = c("","NA","."))
  df<-rbind(df,df0)
}
df$Date<-as.factor(df$Date)
levels(df$Date)
df$ORIG_FID<-NULL
df3<-read.csv(files2[12],header = TRUE,na.strings = c("","NA","."))
df3$X.1<-NULL
df<-rbind(df,df3)

write.csv(df,paste(species,index,'dates combined.csv'))

library(ggplot2)
files2<-list.files(pattern='dates combined', full.names = TRUE, recursive=TRUE)
files2
df<-read.csv(files2[10],header = TRUE,na.strings = c("","NA","."))
df2<-read.csv(files2[5],header = TRUE,na.strings = c("","NA","."))
df$Date<-as.factor(df$Date) 
df$Id_Arc<-as.factor(df$Id_Arc)
All_Date_bar<-ggplot(df,aes(x=Date,y=GLI_mean))+geom_bar(stat = "summary",fun.y="mean")+
  theme(axis.text.x=element_text(angle = 45,hjust = 1))+ggtitle("Average change over time")
All_Date_bar
# by species
All_Date_bar<-ggplot(subset(df,df$Species!='NA'),aes(x=Date,y=GLI_mean,fill=Species))+geom_bar(stat = "identity",position = position_dodge())+
  theme(axis.text.x=element_text(angle = 45,hjust = 1))+ggtitle("Average change over time")
All_Date_bar+expand_limits(y=0)
Geno_name<-levels(df$Id_Arc)
dates_save<-levels(df$Date)
dates_save
Geno_name
one_Geno<-Geno_name[1401]
two_Geno<-Geno_name[2059]
two_Geno_bar<-ggplot(subset(df,df$Id_Arc==one_Geno|df$Id_Arc==two_Geno),aes(x=Date,y=GLI_mean,fill=Genotype))+
  geom_bar(stat = "identity",position = position_dodge())+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  ggtitle("Two genotypes comparison")
two_Geno_bar
##############################################################################
################# TEST MASK ##################################################
files2<-list.files(pattern='Mask', full.names = TRUE, recursive=TRUE)
files2
response="GLI_mean"
mask<-read.csv(files2[2],header = TRUE,na.strings = c("","NA","."))
df<-read.csv(files2[7],header = TRUE,na.strings = c("","NA","."))
mask$Date<-as.factor(mask$Date)
date_mask_save<-list(levels(mask$Date))
df2<-subset(df,df$Date==date_mask_save[[1]])
df2<-df2[order(df2$PlotID),]
df<-df[order(df$Date,df$PlotID),]
mask<-mask[order(mask$PlotID),]
length(df$PlotID)

index=match(date_mask_save[1],df$Date)
j=1
for (i in 2740:2988){
  if (!is.na(mask$Mask[[j]])){
      df2$GLI_mean[[i]]=NA
  }  
  j=j+1
}
library(dplyr)
df %>%
  anti_join(df2,by="PlotID") %>%
  bind_rows(df2) %>%
  arrange(PlotID)





######cut##############




as.Date("24April",format="%d%b")
?as.Date

dates<-substring(names,6)

dates<-sapply(strsplit(names,"*2019"),"[",1)

string<-("Posted 69 months ago (7/4/2011)")




substring(substring(names,6),"2019")
substring(names,"2019")

sub(".*2019", "",names)

sub(".*2019", "",sub("\\..*", "",names))

grep(".2019",names,value=TRUE)

strsplit(names,".*2019")
strsplit(names,".*2019","[",2)
strsplit(names,".*2019","[",1)

sub(".*\\(", "", sub("\\).*", "", string))



