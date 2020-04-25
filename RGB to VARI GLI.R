#################################
##RGB to VARI GLI
#################################

library(rgdal)
library(raster)

# look though xxx folder under working directory, return the names of the jpg or tif files
files<-list.files(path = "./rep2RGB/New",pattern='.tiff', full.names = TRUE, recursive=TRUE)

# extract dates from filenames
# file name pattern must be *****ddMonthYYYY[anything].jpg or .tiff
names<-list.files(path = "./rep2RGB/New",pattern='.tiff', full.names = FALSE, recursive=TRUE)
dates<-as.Date(sub("(?<=2019|2020).*","",substring(names,6),perl=TRUE),format="%d%b")

### Functions creating indices from RGB

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

###
######################################
# Raster Calculation #################
# Write Raster File - VARI and GLI ###
# takes ~1min per raster #############
######################################


for (i in seq_along(files)){

my_raster<-brick(files[i])
GLI_1<-GLI(my_raster,1,2,3) 
VARI_1<-VARI(my_raster,1,2,3)
writeRaster(GLI_1,paste0("image2/GLI_",names[i],".tiff"),overwrite=TRUE)
writeRaster(VARI_1,paste0("image2/VARI_",names[i],".tiff"),overwrite=TRUE)
}
