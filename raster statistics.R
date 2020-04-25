#################################
## % green cover #############
#################################


library(raster)
library(ggplot2)
library(scales)

##pct green cover fundctions##
#pass function vector of files, vector of dates
#and threshold constant (files,dates,threshold)
#threshold VARI =.001 GLI =.07 NDVI = .6
pctgreen <- function(files,dates,threshold){
  
  pctg <- matrix(ncol = 2, nrow = length(files))
  
  for (i in seq_along(files)){
    
    myraster<-brick(files[i])
    
    pctgreen1=(sum(getValues(myraster>threshold))/ncell(myraster))*100
    
    pctg[i,]<-c(pctgreen1,dates[i])
    
  }
  pctg<-as.data.frame(pctg)
  names(pctg)<-c("pct","date")
  pctg$date<-as.Date(pctg$date,origin="1970-01-01")
  names(pctg)<-c("pct","date")
  return(pctg);
  
}

pctgreenNDVI <- function(files,dates,threshold){
  
  pctg <- matrix(ncol = 2, nrow = length(files))
  
  for (i in seq_along(files)){
    
    myraster<-brick(files[i])
    
    pctgreen1=(cellStats(myraster>threshold,'sum')/ncell(myraster))*100
    
    pctg[i,]<-c(pctgreen1,dates[i])
    
  }
  pctg<-as.data.frame(pctg)
  names(pctg)<-c("pct","date")
  pctg$date<-as.Date(pctg$date,origin="1970-01-01")
  names(pctg)<-c("pct","date")
  return(pctg);
  
}


##statistics of raster pixels above given threshold##
thrshav <- function(files,dates,threshold,stat){
  
  pctg <- matrix(ncol = 2, nrow = length(files))
  
  for (i in seq_along(files)){
    
    myraster<-brick(files[i])
    myraster[myraster<threshold]<- 0
    av_abv_t=cellStats(myraster, stat)
      
    pctg[i,]<-c(av_abv_t,dates[i])
    
  }
  pctg<-as.data.frame(pctg)
  names(pctg)<-c("mean","date")
  pctg$date<-as.Date(pctg$date,origin="1970-01-01")
  
  
  return(pctg);
  
}

i=2

ncell(myraster)

hist(myraster)

plot(myraster)

myraster[myraster<.6]<- NA

cellStats(myraster, 'sum')

files
ndvi[ndvi<.6] <- NA
plot(ndvi,
     main="NDVI > .6")



###VARI####
# dates<-as.Date(sub("2019.*","2019",substring(names,11)),format="%d%b")
files<-list.files(path = "./image2",pattern='VARI', full.names = TRUE, recursive=FALSE)
names<-list.files(path = "./image2",pattern='VARI', full.names = FALSE, recursive=FALSE)
dates<-as.Date(sub("(?<=2019|2020).*","",substring(names,11),perl=TRUE),format="%d%b")


pctg<-pctgreen(files,dates,.001)
thav<-thrshav(files,dates,0,"mean")

##clean##

pctgR<-na.omit(pctg)
pctg$date<-as.Date(pctg$date)
pctgR <- pctg[pctg$V2 > as.Date("2019-05-15") & 
                pctg$V2> as.Date("2019-05-20"),]

plot(x=pctg$date,y=pctg$pct)

write.csv(pctg, file = "VARIpctGreen2.csv")


####GLI#####
files<-list.files(path = "./image2",pattern='GLI', full.names = TRUE, recursive=FALSE)
names<-list.files(path = "./image2",pattern='GLI', full.names = FALSE, recursive=FALSE)
dates<-as.Date(sub("(?<=2019|2020).*","",substring(names,10),perl=TRUE),format="%d%b")


pctg<-pctgreen(files,dates,.07)
#names(pctg)<-c("pct","date")


write.csv(pctg, file = "GLIpctGreen2.csv")

pctg2<-read.csv("GLIpctGreen.csv")
pctg2<-as.data.frame(pctg2)
pctg2$date<-as.Date(pctg2$date)

#select#
pctgR <- pctg[pctg$V2 > as.Date("2019-05-15") & 
                pctg$V2> as.Date("2019-05-20"),]

####NDVI#####
files<-list.files(path = "./NDVI", full.names = TRUE, recursive=TRUE)
names<-list.files(path = "./NDVI", full.names = FALSE, recursive=TRUE)
names<-sub("\\..*","",names)
dates<-as.Date(sub("2019.*","2019",names),format="%d%b%Y")

pctg<-pctgreenNDVI(files,dates,.6)

thav<-thrshav(files,dates,0,"mean")

write.csv(pctg, file = "NDVIpctGreen.csv")
write.csv(thav, file = "NDVImean.csv")

pctg2<-read.csv("GLIpctGreen.csv")
pctg2<-as.data.frame(pctg2)
pctg2$date<-as.Date(pctg2$date)

#select#
thavR <- thav[thav$date > as.Date("2019-04-15") & 
                thav$date< as.Date("2019-07-08"),]



#plot####

###plot function####
ggplotRegression <- function(dat, xvar, yvar){
  
  fml <- paste(yvar, "~", xvar)
  
  fit <- lm(fml, dat)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                          "Intercept =",signif(fit$coef[[1]],5 ),
                          " Slope =",signif(fit$coef[[2]], 5),
                          " P =",signif(summary(fit)$coef[2,4], 5)))
}
####################

ggplotRegression(pctg, ("date"), "pct") +
  scale_x_date(labels = date_format("%m-%d"),breaks="weeks")+
  labs(title="meanNDVI th=0", x="date",y="meanNDVI")



###fit sigmoid###

df <- read.csv("pctGreen/VARIpctGreen.csv",
               header = TRUE)

df$date[1]
df$date<-as.Date(df$date,format="%Y-%m-%d")

df$date[1]
pl1<-ggplot(df,aes(x=date,y=pct))+geom_point()+geom_smooth(method=lm)


plot(df$pct ~ (df$date))
fit <- nls(pct ~ SSlogis(date1, Asym, xmid, scal), data = df)

fit <- lm(pct~date1, df)
summary(fit)
help("SSlogis")

as.factor()
is.data.frame(df)

nls(y ~ SSlogis(x, Asym, xmid, scal)


ggplot(df,x=df$date,y=df$pct)

plot(x=df$date,y=df$pct)

d <- data.frame(x = 1, y = 1:10)

##example
Chick.1 <- ChickWeight[ChickWeight$Chick == 1, ]
SSlogis(Chick.1$Time, 368, 14, 6)  # response only
local({
  Asym <- 368; xmid <- 14; scal <- 6
  SSlogis(Chick.1$Time, Asym, xmid, scal) # response _and_ gradient
})
getInitial(weight ~ SSlogis(Time, Asym, xmid, scal), data = Chick.1)
## Initial values are in fact the converged one here, "Number of iter...: 0" :
fm1 <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = Chick.1)
summary(fm1)
## but are slightly improved here:
fm2 <- update(fm1, control=nls.control(tol = 1e-9, warnOnly=TRUE), trace = TRUE)
all.equal(coef(fm1), coef(fm2)) # "Mean relative difference: 9.6e-6"
str(fm2$convInfo) # 3 iterations



## Visualize the SSlogis()  model  parametrization :
xx <- seq(-0.75, 5, by=1/32)
yy <- 5 / (1 + exp((2-xx)/0.6)) # == SSlogis(xx, *):
stopifnot( all.equal(yy, SSlogis(xx, Asym = 5, xmid = 2, scal = 0.6)) )
require(graphics)
op <- par(mar = c(0.5, 0, 3.5, 0))
plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,6), xlim = c(-1, 5),
     xlab = "", ylab = "", lwd = 2,
     main = "Parameters in the SSlogis model")
mtext(quote(list(phi[1] == "Asym", phi[2] == "xmid", phi[3] == "scal")))
usr <- par("usr")
arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
text(     -0.1, usr[4], "y", adj = c(1, 1))
abline(h = 5, lty = 3)
arrows(-0.8, c(2.1, 2.9),
       -0.8, c(0,   5  ), length = 0.1, angle = 25)
text  (-0.8, 2.5, quote(phi[1]))
segments(c(2,2.6,2.6), c(0,  2.5,3.5),   # NB.  SSlogis(x = xmid = 2) = 2.5
         c(2,2.6,2  ), c(2.5,3.5,2.5), lty = 2, lwd = 0.75)
text(2, -.1, quote(phi[2]))
arrows(c(2.2, 2.4), 2.5,
       c(2.0, 2.6), 2.5, length = 0.08, angle = 25)
text(      2.3,     2.5, quote(phi[3])); text(2.7, 3, "1")
par(op)




