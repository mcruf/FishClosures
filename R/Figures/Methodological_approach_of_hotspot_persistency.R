
#####################################################################
#       Plot the step-by-step of the methodological approach        #
#       to identify hotspots and persistency of these hotspots      #
#####################################################################

# To be able to run these lines, I need to source the script "Identify spawner/recruits_hotspots"


setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Shapefile/")
DK <- readOGR(".", "DK") #Importing contracted DK shapefile with width=-0.010
proj4string(DK) <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")







par(mar=c(5,7,5,3),mgp=c(3.5,0.5,0))
plot(gr,type="n",xlab="Longitude (°)",ylab="Latitude (°)",las=1,xlim=c(12,15.25),cex.axis=1.5,cex.lab=1.5)
image(gr, dfstack[,5],add=T,col=tim.colors(99))
#plot(gr,add=T,col="gray40")
plot(DK,add=T,col="gray70")
box()



par(mar=c(5,7,5,3),mgp=c(3.5,0.5,0))
plot(gr,type="n",xlab="Longitude (°)",ylab="Latitude (°)",las=1,xlim=c(12,15.25),cex.axis=1.5,cex.lab=1.5)
image(gr, concTransform3(dfstack[,5]) >= treshold2[5,],add=T)
#plot(gr,add=T,col="gray40")
plot(DK,add=T,col="gray70")
box()



par(mar=c(5,7,5,3),mgp=c(3.5,0.5,0))
plot(gr,type="n",xlab="Longitude (°)",ylab="Latitude (°)",las=1,xlim=c(12,15.25),cex.axis=1.5,cex.lab=1.5, main="HI=0.3")
image(gr, persistency2$Index >= 0.3,add=T) #I need to decide what value I use here as a treshold
plot(gr,add=T)
plot(DK,add=T,col=1)
box()
