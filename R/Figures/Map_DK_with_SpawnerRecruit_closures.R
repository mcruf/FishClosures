##########################################################################################
#                                                                                        #
##                         Mapping the juvenile and spawner closures                    ##
##                                    (Rufener et al.)                                  ##
#                                                                                        #
##########################################################################################


# Last update: March 2022

# Code written and mantained by Marie-Christine Rufener
# Contact < macrufener@gmail.com > for any query or to report code issues.


# The following script returns a plot in which the juvenile and spawner
# closures are exposed in the study area (Western Baltic Sea).
# These closures were based on the persistent hotspot areas, that were
# identified in the Identify_spawner_recruits_hotspots.R script.


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~
# 1) Load R libraries
#~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
library(ggpatern)
library(ggpubr)
library(raster)
library(rgdal)
library(maptools)
library(ggpattern)
library(marmap)
library(oce)
library(ocedata)
library(rgeos)
library(ggspatial)
library(ggsn)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Plot study area with hotspot polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

UTMzone  <- 32 # Define the UTM zone



# 2.1) Import shapefile of the study area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("~/Data/Shapefile/Denmark")
DK <- readOGR(".", "DK") 

crs(DK) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 



# 2.2) Import ICES area shapefiles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Data/Shapefile/ICES")
ICES <- readOGR(dsn = ".", layer = "ICESareas_WB_and_Kattegat")

crs(ICES) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 




# 2.3) Get bathymetry raster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To make the depth contour layers on the map

## 2.3.1) Get NOAA bathymetry for the study region
data("coastlineWorldFine")
b <- getNOAA.bathy(lon1 = 9.5, lon2 = 15.5, lat1 = 53.7, lat2 = 56.5, 
                  resolution = 1)

# The following map with oce does not use a projection, which basically means
# that all plotting occurs as if the world were completely flat. Plotting in this 
# way is quick and convenient, especially because it allows you to use all of R's
# base graphic functions, but is not appropriate in many cases. I typically use
# this approach when plotting relatively small areas (maybe 10s of kilometers)
# and in circumstances where exact distances aren't crticially important.




# 2.3.2) Convert bathymetry layer
bathyLon = as.numeric(rownames(b))
bathyLat = as.numeric(colnames(b))
bathyZ = as.numeric(b)
dim(bathyZ) = dim(b)



# 2.4) Import spawners closure shapefiles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Data/Shapefile/Hotspots/Spawners/")
sbox1 <- readOGR(".", "Spawner_box1")
sbox2 <- readOGR(".", "Spawner_box2")


crs(sbox1) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 
crs(sbox2) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 


# #Run these lines if only area of polygon is to be calculated
# sbox1 <- spTransform(sbox1, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
# sbox2 <- spTransform(sbox2, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM

# #Calculate the area
# rgeos::gArea(sbox1)
# rgeos::gArea(sbox2)

# sbox1 <- spTransform(sbox1, CRS(paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
# sbox2 <- spTransform(sbox2, CRS(paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM



# 2.5) Import Juvenile closure shapefiles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Data/Shapefile/Hotspots/Recruits")
rbox1 <- readOGR(".", "Recruits_box1")
rbox2 <- readOGR(".", "Recruits_box2")
rbox3 <- readOGR(".", "Recruits_box3")

crs(rbox1) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 
crs(rbox2) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 
crs(rbox3) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 


## Run these lines if only area of polygon is to be calculated
# rbox1 <- spTransform(rbox1, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
# rbox2 <- spTransform(rbox2, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
# rbox3 <- spTransform(rbox3, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM

# rgeos::gArea(rbox1)
# rgeos::gArea(rbox2)
# rgeos::gArea(rbox3)

# rbox1 <- spTransform(rbox1, CRS(paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
# rbox2 <- spTransform(rbox2, CRS(paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
# rbox3 <- spTransform(rbox3, CRS(paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM




# 2.6) Get intersection between spawners & juvenile closures
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# There was an intersection area close to Bornholm

srbox <- intersect(rbox2, sbox2)
rgeos::gArea(srbox)

poly_SRbox <- as(srbox, "SpatialPolygonsDataFrame" )

## Save polygons
# writeOGR(poly_SRbox, dsn = '.', layer = 'SpawnerRecruits_intersection_box', driver = "ESRI Shapefile")


#gArea(DK)
#DK@polygons[[1]]@area#there are actually several polygons

gArea(sbox1); raster::area(sbox1)
gArea(sbox2); raster::area(sbox2)
gArea(rbox1); raster::area(rbox1)
gArea(rbox2); raster::area(rbox2)
gArea(rbox3); raster::area(rbox3)
gArea(srbox); raster::area(srbox)



#~~~~~~~~~~~~~~~~~~~
# 3) Go for the plot
#~~~~~~~~~~~~~~~~~~~~~



# 3.1) Do it in ggplot
#~~~~~~~~~~~~~~~~~~~~~~
ext <- extent(9, 15.5, 53.7, 56.5)
DK2 <- crop(DK,ext)


ICES <- crop(ICES,ext)



#Turn GIS object into a DataFrame for ggplot to use

## DK
DK2@data$id <- rownames(DK2@data)
DKPoints <- fortify(DK2, region = "id")
DKDF <- merge(DKPoints, DK2@data, by = "id")



## ICES
ICES@data$id <- rownames(ICES@data)
ICESPoints <- fortify(ICES, region = "id")
ICESDF <- merge(ICESPoints, ICES@data, by = "id")


## Bathy
bf = fortify.bathy(b)



## sbox1
sbox1@data$id <- rownames(sbox1@data)
sbox1Points <- fortify(sbox1, region = "id")
sbox1DF <- merge(sbox1Points, sbox1@data, by = "id")


## sbox2
sbox2@data$id <- rownames(sbox2@data)
sbox2Points <- fortify(sbox2, region = "id")
sbox2DF <- merge(sbox2Points, sbox2@data, by = "id")


## rbox1
rbox1@data$id <- rownames(rbox1@data)
rbox1Points <- fortify(rbox1, region = "id")
rbox1DF <- merge(rbox1Points, rbox1@data, by = "id")


## rbox2
rbox2@data$id <- rownames(rbox2@data)
rbox2Points <- fortify(rbox2, region = "id")
rbox2DF <- merge(rbox2Points, rbox2@data, by = "id")


## rbox3
rbox3@data$id <- rownames(rbox3@data)
rbox3Points <- fortify(rbox3, region = "id")
rbox3DF <- merge(rbox3Points, rbox3@data, by = "id")



## srbox
srbox@data$id <- rownames(srbox@data)
srboxPoints <- fortify(srbox, region = "id")
srboxDF <- merge(srboxPoints, srbox@data, by = "id")







ggplot() +  
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-10),
               #size=c(1),
               linetype = "solid",
               colour="grey80") +
  
  geom_contour(data = bf,
               aes(x=x, y=y, z=z),
               breaks=c(-30),
               size=c(1),
               linetype = "solid",
               colour="grey80") +


  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-50),
               #size=c(1),
               linetype = "solid",
               colour="grey80") +
 

  # geom_contour(data = bf, 
  #              aes(x=x, y=y, z=z),
  #              breaks=c(-100),
  #              #size=c(1),
  #              linetype = "dotted",
  #              colour="grey80") +
 

  
  geom_polygon(data = sbox1DF, aes(x = long, y = lat, group = group),fill="deepskyblue4", colour="black", alpha=0.6) +
  geom_polygon(data = sbox2DF, aes(x = long, y = lat, group = group),fill="deepskyblue4", colour="black", alpha=0.6) +
  geom_polygon(data = rbox1DF, aes(x = long, y = lat, group = group),fill="orange", colour="orange", alpha=0.6) +
  geom_polygon(data = rbox2DF, aes(x = long, y = lat, group = group),fill="orange", colour="orange", alpha=0.6) +
  geom_polygon(data = rbox3DF, aes(x = long, y = lat, group = group),fill="orange", colour="orange", alpha=0.6) +
  geom_polygon_pattern(data = srboxDF, aes(x = long, y = lat, group = group),pattern="stripe",fill="transparent",pattern_density=0.3,pattern_spacing=0.01) +
  
  geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
  
  geom_polygon(data = ICESDF,aes(x = long, y = lat, group = group),fill=NA,colour="black") +
  
  annotate(geom = "text", x = 12.75, y = 55.8, label = "SG1", 
           fontface = "bold", color = "grey10", size = 4) +
  
  annotate(geom = "text", x = 14.3, y = 55.2, label = "SG2", 
           fontface = "bold", color = "grey10", size = 4) +
  
  annotate(geom = "text", x = 13.28, y = 55.20, label = "NG1", 
           fontface = "bold", color = "grey10", size = 4) +
  
  annotate(geom = "text", x = 14.15, y = 54.9, label = "NG2", 
           fontface = "bold", color = "grey10", size = 4) +
  
  annotate(geom = "text", x = 14.75, y = 54.73, label = "NG3", 
           fontface = "bold", color = "grey10", size = 4) +
  
  # annotate(geom = "text", x = 13.7, y = 55.8, angle=50, label = "S w e d e n", 
  #          fontface = "bold", color = "grey40", size = 45) +
  # 
  # annotate(geom = "text", x = 11.5, y = 53.85, label = "G e r m a n y", 
  #          fontface = "bold", color = "grey40", size = 4) +
  # 
  # annotate(geom = "text", x = 9.4, y = 55.8, angle=70, label = "D e n m a r k", 
  #          fontface = "bold", color = "grey40", size = 4) +
  
  annotate(geom = "text", x = 15.12, y = 55.25, angle=322, label = "Bornholm", 
           fontface = "italic", color = "grey40", size = 4) +
  
  annotate(geom = "text", x = 13.4, y = 54.9, angle=30, label = "Arkona Basin", 
           fontface = "italic", color = "grey40", size = 4) +
  
  annotate(geom = "text", x = 12.65, y = 55.45, angle=42, label = "Øresund", 
           fontface = "italic", color = "grey40", size = 4) +
  
  annotate(geom = "text", x = 11.4, y = 54.25, angle=37, label = "Mecklenburg Bay", 
           fontface = "italic", color = "grey40", size = 4) +
  
  annotate(geom = "text", x = 10.6, y = 54.6, label = "Kiel Bay", 
           fontface = "italic", color = "grey40", size = 4) +
  
  
  annotate("text", x = 11.8, y = 56.2, label = "21",fontface = 2, color = "grey30", size=4) +
  annotate("text", x = 15, y = 55.7, label = "25",fontface = 2, color = "grey30", size=4) +
  annotate("text", x = 11.2, y = 54.6, label = "22",fontface = 2, color = "grey30",size=4,angle = 30) +
  annotate("text", x = 12.52, y = 55.47, label = "23",fontface = 2,color = "grey30", size=4) +
  annotate("text", x = 14.4, y = 54.3, label = "24",fontface = 2, color = "grey30",size=4) +
  
  
  
  coord_map() +
  theme_bw() + 
  scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
  scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
  labs(x = "Longitude (°)", y="Latitude (°)") +
  theme(panel.border = element_rect(colour="black", size=1),
    legend.position = "none",
    
    axis.text = element_text(size=13),
    
    #axis.title = element_blank(),
    axis.text.x = element_text(size=14),
    axis.text.y = element_text(size=14),
    axis.title.y = element_text(margin=margin(t = 0, r = 18, b = 0, l = 0),size=16),
    axis.title.x = element_text(margin=margin(t=18,r=0,b=0,l=0),size=16),
    plot.title = element_text(hjust = 0.5,size=14),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 


ggsave("DK_map_Closures.png",dpi=450, width = 25, height = 35, units = "cm")





# 3.2) Do it in normal plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~
## Just an alternative for fun...


contour(bathyLon,bathyLat,bathyZ,
        levels = c(-10, -20, -30, -50, -100,-200),
        lwd = c(1.5, 1.5, 1.5,1.5,1.5,1.5),
        lty = c(1, 1, 2, 2, 3, 3),
        drawlabels = T,  col = rev(gray.colors(6)))


plot(DK,add=T,col="gray80")




# add depth legend
legend("bottomleft", seg.len = 3, cex = 0.8,
       lwd = c(1, 1, 1,1,1,1),
       lty = c(1, 1, 2, 2, 3, 3),
       legend = c("10", "20", "30", "50", "100","200"),
       col = rev(gray.colors(6)), title = "Depth [m]", bg= "white")



plot(sbox1,col="deepskyblue4",add=T) #Spawner box 1
plot(sbox2,col="deepskyblue4",add=T,alpha=0.5) #Spawner box 2

plot(rbox1,col="orange",add=T) #Recruit box 1
plot(rbox2,col="orange",add=T) #Recruit box 2
plot(rbox3,col="orange",add=T) #Recruit box 3

plot(srbox,density=10,add=T) #Spawner x Recruit intersection 
