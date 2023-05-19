
library(ggplot2)
library(ggpattern)
library(ggpubr)
library(raster)
library(rgdal)
library(maptools)
library(ggpattern)
library(marmap)

UTMzone  <- 32


# Plot study area with hotspot polygons
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# DK shapefile
#~~~~~~~~~~~~~~~~

setwd("~/Desktop/FishClosures/Data/Shapefile/Denmark/")
DK <- readOGR(".", "DK") 

crs(DK) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 


#run this line if plotting map only
#DK <- spTransform(DK, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


##Do not run the line below if plotting the map (I need degrees, not UTM units for the map)
#DK <- spTransform(DK, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM



# Import ICES rect shapefile
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Desktop/FishClosures/Data/Shapefile/ICES/")
ICES <- readOGR(dsn = ".", layer = "ICESareas_WB_and_Kattegat")

crs(ICES) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 


#ICES <- spTransform(ICES, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


# Depth contour shapefile
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Shapefile/Bathymetry/")
# depth <- readOGR(".", "Depth contours") 
# 
# 
# depth <- readShapePoly(file.path('C:','Users','mruf','OneDrive - Danmarks Tekniske Universitet','PhD',
#                               'Manuscript_03','Data','Shapefile','Bathymetry'), 
#                     proj4string=CRS("+proj=longlat +ellps=WGS84"))
# 
# depth <- spTransform(depth, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

##Do not run the line below if plotting the map (I need degrees, not UTM units for the map)
#depth <- spTransform(depth, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM


# get bathymetry data
b = getNOAA.bathy(lon1 = 9.5, lon2 = 15.5, lat1 = 53.7, lat2 = 56.5, 
                  resolution = 1)
## Querying NOAA database ...
## This may take seconds to minutes, depending on grid size
## Building bathy matrix ...

# The following map with oce does not use a projection, which basically means
# that all plotting occurs as if the world were completely flat. Plotting in this 
# way is quick and convenient, especially because it allows you to use all of R's
# base graphic functions, but is not appropriate in many cases. I typically use
# this approach when plotting relatively small areas (maybe 10s of kilometers)
# and in circumstances where exact distances aren't crticially important.


library(oce)
library(ocedata)
data("coastlineWorldFine")

# convert bathymetry
bathyLon = as.numeric(rownames(b))
bathyLat = as.numeric(colnames(b))
bathyZ = as.numeric(b)
dim(bathyZ) = dim(b)




# Recruits shapefiles
#~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Desktop/Review_FR/R2/Results/Hotspot/Juveniles/")
rbox1 <- readOGR(".", "Recruits_box1")
rbox2 <- readOGR(".", "Recruits_box2")

crs(rbox1) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
crs(rbox2) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"



#~~~~~~~~~~~~~~~~~
# Go for the plot
#~~~~~~~~~~~~~~~~~


## Do it in normal plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# contour(bathyLon,bathyLat,bathyZ,
#         levels = c(-10, -20, -30, -50, -100,-200),
#         lwd = c(1.5, 1.5, 1.5,1.5,1.5,1.5),
#         lty = c(1, 1, 2, 2, 3, 3),
#         drawlabels = T,  col = rev(gray.colors(6)))
# 
# 
# plot(DK,add=T,col="gray80")
# 
# 
# 
# 
# # add depth legend
# legend("bottomleft", seg.len = 3, cex = 0.8,
#        lwd = c(1, 1, 1,1,1,1),
#        lty = c(1, 1, 2, 2, 3, 3),
#        legend = c("10", "20", "30", "50", "100","200"),
#        col = rev(gray.colors(6)), title = "Depth [m]", bg= "white")
# 
# 
# 
# plot(sbox1,col="deepskyblue4",add=T) #Spawner box 1
# plot(sbox2,col="deepskyblue4",add=T,alpha=0.5) #Spawner box 2
# 
# plot(rbox1,col="orange",add=T) #Recruit box 1
# plot(rbox2,col="orange",add=T) #Recruit box 2
# plot(rbox3,col="orange",add=T) #Recruit box 3
# 
# plot(srbox,density=10,add=T) #Spawner x Recruit intersection 




## Do it in ggplot
#~~~~~~~~~~~~~~~~~~~~
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


## rbox1
rbox1@data$id <- rownames(rbox1@data)
rbox1Points <- fortify(rbox1, region = "id")
rbox1DF <- merge(rbox1Points, rbox1@data, by = "id")


## rbox2
rbox2@data$id <- rownames(rbox2@data)
rbox2Points <- fortify(rbox2, region = "id")
rbox2DF <- merge(rbox2Points, rbox2@data, by = "id")



library(ggspatial)
library(ggsn)




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
  
  
  geom_polygon(data = rbox1DF, aes(x = long, y = lat, group = group),fill="orange", colour="black", alpha=0.6) +
  geom_polygon(data = rbox2DF, aes(x = long, y = lat, group = group),fill="orange", colour="black", alpha=0.6) +
  
  
  geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
  
  geom_polygon(data = ICESDF,aes(x = long, y = lat, group = group),fill=NA,colour="black") +
  

  annotate(geom = "text", 14.48, y = 55.24, label = "NG1", 
           fontface = "bold", color = "grey10", size = 6) +
  
  # annotate(geom = "text", x = 14.73, y = 55.55, label = "NG1", 
  #          fontface = "bold", color = "grey10", size = 6) +
  # 
  # geom_segment(aes(x = 14.7, y = 55.5, xend = 14.3, yend = 55.2),
  #              lineend = "butt", 
  #              linejoin = "round",
  #              size = 1,
  #              arrow = arrow(length = unit(0.5, "cm"))) +
  
  annotate(geom = "text", x = 14.78, y = 54.73, label = "NG2", 
           fontface = "bold", color = "grey10", size = 6) +
  
  # annotate(geom = "text", x = 14.5, y = 54.33, label = "NG2", 
  #          fontface = "bold", color = "grey10", size = 6) +
  # 
  # geom_segment(aes(x = 14.5, y = 54.4, xend = 14.8, yend = 54.6),
  #              lineend = "butt", 
  #              linejoin = "round",
  #              size = 1,
  #              arrow = arrow(length = unit(0.5, "cm")))  +
  # 
  

  annotate(geom = "text", x = 15.12, y = 55.25, angle=322, label = "Bornholm", 
         fontface = "italic", color = "grey40", size = 6) +
  
  annotate(geom = "text", x = 13.4, y = 54.9, angle=30, label = "Arkona Basin", 
           fontface = "italic", color = "grey40", size = 6) +
  
  annotate(geom = "text", x = 12.65, y = 55.45, angle=42, label = "Øresund", 
           fontface = "italic", color = "grey40", size = 6) +
  
  annotate(geom = "text", x = 11.4, y = 54.25, angle=37, label = "Mecklenburg Bay", 
           fontface = "italic", color = "grey40", size = 6) +
  
  annotate(geom = "text", x = 10.6, y = 54.6, label = "Kiel Bay", 
           fontface = "italic", color = "grey40", size = 6) +
  
  
  annotate("text", x = 11.8, y = 56.23, label = "21",fontface = 2, color = "grey30", size=6) +
  annotate("text", x = 15.05, y = 55.73, label = "25",fontface = 2, color = "grey30", size=6) +
  annotate("text", x = 11.2, y = 54.6, label = "22",fontface = 2, color = "grey30",size=6,angle = 30) +
  annotate("text", x = 12.52, y = 55.47, label = "23",fontface = 2,color = "grey30", size=6) +
  annotate("text", x = 13.5, y = 54.8, label = "24",fontface = 2, color = "grey30",size=6) +
  
  
  
  coord_map() +
  theme_bw() + 
  scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
  scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
  labs(x = "Longitude (°)", y="Latitude (°)", title = 'Nursery Area Closure') +
  theme(panel.border = element_rect(colour="black", size=1),
        legend.position = "none",
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        axis.title.y = element_text(margin=margin(t = 0, r = 18, b = 0, l = 0),size=16),
        axis.title.x = element_text(margin=margin(t=18,r=0,b=0,l=0),size=16),
        plot.title = element_text(hjust = 0.5,size=17, face = 'bold'),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 



setwd("~/Desktop/Review_FR/R2/Figures/")
ggsave("DK_map_Nursery_Closures.png",dpi=450, width = 25, height = 35, units = "cm")



## Backup
# ggplot() +  
#       geom_polygon(data = sbox1DF, aes(x = long, y = lat, group = group),fill="deepskyblue4", colour="black", alpha=0.6) +
#       geom_polygon(data = sbox2DF, aes(x = long, y = lat, group = group),fill="deepskyblue4", colour="black", alpha=0.6) +
#       geom_polygon(data = rbox1DF, aes(x = long, y = lat, group = group),fill="orange", colour="orange", alpha=0.6) +
#       geom_polygon(data = rbox2DF, aes(x = long, y = lat, group = group),fill="orange", colour="orange", alpha=0.6) +
#       geom_polygon(data = rbox3DF, aes(x = long, y = lat, group = group),fill="orange", colour="orange", alpha=0.6) +
#       
#       geom_polygon_pattern(data = srboxDF, aes(x = long, y = lat, group = group),pattern="stripe",fill="transparent",pattern_density=0.3,pattern_spacing=0.01) +
#       
#       geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="black") +
#       
#       coord_map() +
#       theme_bw() + 
#       scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
#       scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
#       labs(x = "Longitude (?)", y="Latitude (?)") +
#       theme(#legend.text = element_blank(),
#         legend.position = "none",
#         axis.text = element_text(size=13),
#         #axis.title = element_blank(),
#         axis.text.x = element_text(size=14),
#         axis.text.y = element_text(size=14),
#         axis.title.y = element_text(margin=margin(t = 0, r = 18, b = 0, l = 0),size=16),
#         axis.title.x = element_text(margin=margin(t=18,r=0,b=0,l=0),size=16),
#         plot.title = element_text(hjust = 0.5,size=14),
#         plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) 




