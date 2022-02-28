##################################################################
#   Generate GIS from the model output - maps based on ggplot    #
##################################################################



#~~~~~~~~~~~~~~
#  Spawner
#~~~~~~~~~~~~~~




library(ggplot2)
library(maptools)
library(gridConstruct)
library(raster)
library(ggsn)
library(mapdata)
library(fields)
library(rgdal)
library(pals)
library(tidyr)
library(gganimate)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set working directoy and load the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("D:/PhD/Project III/Results/Spawning_HotSpots/YearMonth") #For YearMonth resolution



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Step 1) Stack the age-specific abundances and retrieve total abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Consider spawners only from A3-A5+

# LGNB output gives me a dataframe where each column is a time-period.
# Since we run the model from 2005-2019 on a monthly basis, this means that
# I will have 180 columns (V1-V180).


# The column numbers that corresponds to the spawning monhts (JAN-MARCH)
spawn <- c("V1","V2","V3",
           "V13","V14","V15",
           "V25","V26","V27",
           "V37","V38","V39",
           "V49","V50","V51",
           "V61","V62","V63",
           "V73","V74","V75",
           "V85","V86","V87",
           "V97","V98","V99",
           "V109","V110","V111",
           "V121","V122","V123",
           "V133","V134","V135",
           "V145","V146","V147",
           "V157","V158","V159",
           "V169","V170","V171")



# 1.1) Load the results
#~~~~~~~~~~~~~~~~~~~~~~~~

## A2
# load("results_WBScod_m1_A2_both_No_One_noprofile_.RData")
# A2 <- as.list(env1$sdr,"Estimate"); A2  <- as.data.frame(A2$eta_density)
# rm(list=setdiff(ls(), ls(pattern=c("A2"))))


## A3
load("results_WBScod_m1_A3_both_No_One_noprofile_.RData")
A3 <- as.list(env1$sdr,"Estimate"); A3  <- as.data.frame(A3$eta_density)
A3 <- A3[,spawn] #Select only the columns corresponding to spawning period
colnames(A3) <- paste("V",1:ncol(A3),sep="") #Rename for readbility

rm(list=setdiff(ls(), ls(pattern=c("A2|A3|spawn"))))


## A4
load("results_WBScod_m1_A4_both_No_One_noprofile_.RData")
A4 <- as.list(env1$sdr,"Estimate"); A4  <- as.data.frame(A4$eta_density)
A4 <- A4[,spawn] #Select only the columns corresponding to spawning period
colnames(A4) <- paste("V",1:ncol(A4),sep="") #Rename for readbility


rm(list=setdiff(ls(), ls(pattern=c("A2|A3|A4|spawn"))))


## A5
load("results_WBScod_m1_A5_both_No_One_noprofile_.RData")
A5 <- as.list(env1$sdr,"Estimate"); A5  <- as.data.frame(A5$eta_density)
A5 <- A5[,spawn] #Select only the columns corresponding to spawning period
colnames(A5) <- paste("V",1:ncol(A5),sep="") #Rename for readbility

rm(list=setdiff(ls(), ls(pattern=c("A2|A3|A4|A5|spawn|gr|datatot"))))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract and transform the abundance estimated values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# Transform abundance values to 0-1 interval for better visualization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A3_ct <- apply(A3,2,concTransform)
A4_ct <- apply(A4,2,concTransform)
A5_ct <- apply(A5,2,concTransform)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate raster files from the estimated abundances
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We need to do this step if we want to produce the maps on ggplot.
# To do so, we first neet do create a dataframe with the grids long/lat and the abundance associated to this locations.
# Afterwards we need to create an "empty raster" because we have an unregularly spaced grid

load("results_WBScod_m1_A4_both_No_One_noprofile_.RData") #choose an arbitrary result file



# Create a dataframe with the grid lon/lat and the associated abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_A3CT <- cbind(gr,A3_ct)
df_A4CT <- cbind(gr,A4_ct)
df_A5CT <- cbind(gr,A5_ct)



# spdf <- SpatialPixelsDataFrame(points=df[c("lon", "lat")], data = df,tolerance = 0.959642)
# test_df <- as.data.frame(spdf)
# colnames(test_df) <- c("value", "x", "y")

# Create an empty rasterfile based on the dataframe from the previous step
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
e <- extent(as.matrix(gr))
#r <- raster(e,ncol=55,nrow=55) #WBS cod for a 5x5 km grid (in YearQuarter case)
#r <- raster(e,ncol=33,nrow=37) #WBS cod for a 10x10 km grid (in YearMonth case)
r <- raster(e,ncol=30,nrow=30) #WBS cod for a 10x10 km grid (in YearMonth case)


abulist <- list(df_A3CT,df_A4CT,df_A5CT)
names(abulist) <- c("A3","A4","A5")



# Rasterize the previously created raster - FIXME: OTIMIZE THAT IN A LOOP!!!!!!!!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Convert df from wide to long format
abulist_long <- list()
for(i in seq_along(abulist)){
  abulist_long[[i]] <- gather(abulist[[i]],key=YearMonth, value=Abundance, -c(lon,lat))
}


# Set YearMonth to factor and put it in the right order
for(i in seq_along(abulist_long)){
  abulist_long[[i]]$YearMonth <- as.factor(abulist_long[[i]]$YearMonth)
  abulist_long[[i]]$YearMonth <- factor(abulist_long[[i]]$YearMonth, levels = c(paste("V",1:nlevels(abulist_long[[i]]$YearMonth),sep="")))
}


# Rasterize each abundance info
abulist_longsplit <- list()
for(i in seq_along(abulist_long)){
  abulist_longsplit[[i]] <- split(abulist_long[[i]], list(abulist_long[[i]]$YearMonth))
}

#str(abulist_longsplit)


#lraster <- rep(list(vector("list", 180)),3) #needs to be the same number as age groups ; 180 when all months are considered
lraster <- rep(list(vector("list", 45)),3) #needs to be the same number as age groups ; 45 when only JAN-MARCH months are considered


for(i in seq_along(abulist_longsplit)){
  for(j in seq_along(abulist_longsplit[[i]])){
    lraster[[i]][[j]] <- rasterize(abulist_longsplit[[i]][[j]][,c("lon","lat")], r, abulist_longsplit[[i]][[j]][,"Abundance"], fun=mean)
    lraster[[i]][[j]] <- disaggregate(lraster[[i]][[j]],2, method = "bilinear")
  }
}

image(lraster[[1]][[1]],col=tim.colors(99))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate GIFs of the predicted abundance with ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Plotting raster on ggplot isn't very straightforward.
#In order to plot them, one needs first to convert the raster file into a dataframe.
#But before doing that, one needs to convert the raster to a spatialpixeldataframe (SPDF) and afterwards,
#convert the SPDF to a conventional dataframe (DF).
#I also created manually a vector of titles for all the 48 maps, and which will be included within the ggplot script.



# Convert rasterlayer to a DF (raster -> SPDF -> DF)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#r.spdf2_bothCT <- as(raster_bothCT, "SpatialPixelsDataFrame")
#r.df2 <- as.data.frame(r.spdf2)

# Combined
r.spdf2 <- rep(list(vector("list", 45)),3)
r.df2 <- rep(list(vector("list", 45)),3)
for(i in 1:length(lraster)){
  for(j in seq_along(lraster[[i]])){
  r.spdf2[[i]][[j]] <- as(lraster[[i]][[j]],"SpatialPixelsDataFrame")
  r.df2[[i]][[j]] <- as.data.frame(r.spdf2[[i]][[j]])
  }
}




# Create shapefile for study area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data("worldHiresMapEnv")
# DK_coast_poly <- map("worldHires",  fill=TRUE, col="transparent",
#                      plot=FALSE, xlim=c(9,15.5), ylim=c(54.5,58))
# DK_coast_poly$names
# IDs <- sapply(strsplit(DK_coast_poly$names, ":"), function(x) x[1])
# DK_poly <- map2SpatialPolygons(DK_coast_poly, IDs=IDs,
#                                proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))


setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Shapefile/")
DK <- readOGR(".", "DK") 

UTMzone  <- 32

ext <- extent(9, 15.5, 53.7, 56.5)
DK2 <- crop(DK,ext)
crs(DK2)   <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 

DK2@data$id <- rownames(DK2@data)
DKPoints <- fortify(DK2, region = "id")
DKDF <- merge(DKPoints, DK2@data, by = "id")



#  Go for the plot
#~~~~~~~~~~~~~~~~~~~~

r.df2[[1]][[5]]

library(fishualize)
pal <- fish(99, option = "Epinephelus_lanceolatus",end=0.8)

ggplot() +
  geom_tile(data=r.df2[[1]][[45]],aes(x=x,y=y,fill = layer)) +
  #geom_polygon(data=DK, aes(x=long, y=lat, group=group), fill="black", colour="black") + 
  geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
  #scale_fill_gradientn(colours = rev(parula(99)[99:1])) +
  scale_fill_gradientn(colours = pal) +
  coord_map() +
  scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
  scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
  #labs(x = "Longitude (°)", y="Latitude (°)") +
  theme_bw() +
  theme(#legend.text = element_text(size=13),
    legend.position = "none",
    
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    #axis.text.x = element_text(size=16),
    #axis.text.y = element_text(size=16),
    #axis.title.y = element_text(margin=margin(t = 0, r = 18, b = 0, l = 0),size=16),
    #axis.title.x = element_text(margin=margin(t=18,r=0,b=0,l=0),size=16),
    axis.title = element_blank(),
    #plot.title = element_text(hjust = 0.5,size=18,face="bold"),
    plot.title = element_text(hjust = 0.5,size=28,face="bold"),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    plot.margin = unit(c(0, 0, 0, 0), "cm"))



setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Schematic_overview")
ggsave("WBScod_A2_t45.png",dpi=300)



##########################################



#~~~~~~~~~~~~~~
#  Recruits
#~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set working directoy and load the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("D:/PhD/Project III/Results/Recruits_HotSpot/YearMonth/") #For YearMonth resolution



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Step 1) Stack the age-specific abundances and retrieve total abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Consider recruits only from A0-A2

# LGNB output gives me a dataframe where each column is a time-period.
# Since we run the model from 2005-2019 on a monthly basis, this means that
# I will have 180 columns (V1-V180).



# 1.1) Load the results
#~~~~~~~~~~~~~~~~~~~~~~~~

## A0
load("results_WBScod_m1_A0_survey_No_One_noprofile_.RData")
A0 <- as.list(env1$sdr,"Estimate"); A0  <- as.data.frame(A0$eta_density)
rm(list=setdiff(ls(), ls(pattern=c("A0|A1|A2|gr|datatot"))))


## A1
load("results_WBScod_m1_A1_survey_No_One_noprofile_.RData")
A1 <- as.list(env1$sdr,"Estimate"); A1  <- as.data.frame(A1$eta_density)
rm(list=setdiff(ls(), ls(pattern=c("A0|A1|A2|gr|datatot"))))


## A1
load("results_WBScod_m1_A2_both_No_One_noprofile_.RData")
A2 <- as.list(env1$sdr,"Estimate"); A2  <- as.data.frame(A2$eta_density)
rm(list=setdiff(ls(), ls(pattern=c("A0|A1|A2|gr|datatot"))))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract and transform the abundance estimated values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# Transform abundance values to 0-1 interval for better visualization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A0_ct <- apply(A0,2,concTransform)
A1_ct <- apply(A1,2,concTransform)
A2_ct <- apply(A2,2,concTransform)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate raster files from the estimated abundances
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We need to do this step if we want to produce the maps on ggplot.
# To do so, we first neet do create a dataframe with the grids long/lat and the abundance associated to this locations.
# Afterwards we need to create an "empty raster" because we have an unregularly spaced grid

load("results_WBScod_m1_A0_survey_No_One_noprofile_.RData") #choose an arbitrary result file



# Create a dataframe with the grid lon/lat and the associated abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_A0CT <- cbind(gr,A0_ct)
df_A1CT <- cbind(gr,A1_ct)
df_A2CT <- cbind(gr,A2_ct)



# spdf <- SpatialPixelsDataFrame(points=df[c("lon", "lat")], data = df,tolerance = 0.959642)
# test_df <- as.data.frame(spdf)
# colnames(test_df) <- c("value", "x", "y")

# Create an empty rasterfile based on the dataframe from the previous step
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
e <- extent(as.matrix(gr))
#r <- raster(e,ncol=55,nrow=55) #WBS cod for a 5x5 km grid (in YearQuarter case)
#r <- raster(e,ncol=33,nrow=37) #WBS cod for a 10x10 km grid (in YearMonth case)
r <- raster(e,ncol=30,nrow=30) #WBS cod for a 10x10 km grid (in YearMonth case)


abulist <- list(df_A0CT,df_A1CT,df_A2CT)
names(abulist) <- c("A0","A1","A2")



# Rasterize the previously created raster - FIXME: OTIMIZE THAT IN A LOOP!!!!!!!!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Convert df from wide to long format
abulist_long <- list()
for(i in seq_along(abulist)){
  abulist_long[[i]] <- gather(abulist[[i]],key=YearMonth, value=Abundance, -c(lon,lat))
}


# Set YearMonth to factor and put it in the right order
for(i in seq_along(abulist_long)){
  abulist_long[[i]]$YearMonth <- as.factor(abulist_long[[i]]$YearMonth)
  abulist_long[[i]]$YearMonth <- factor(abulist_long[[i]]$YearMonth, levels = c(paste("V",1:nlevels(abulist_long[[i]]$YearMonth),sep="")))
}


# Rasterize each abundance info
abulist_longsplit <- list()
for(i in seq_along(abulist_long)){
  abulist_longsplit[[i]] <- split(abulist_long[[i]], list(abulist_long[[i]]$YearMonth))
}

#str(abulist_longsplit)


lraster <- rep(list(vector("list", 180)),3) #needs to be the same number as age groups ; 180 when all months are considered
#lraster <- rep(list(vector("list", 45)),3) #needs to be the same number as age groups ; 45 when only JAN-MARCH months are considered


for(i in seq_along(abulist_longsplit)){
  for(j in seq_along(abulist_longsplit[[i]])){
    lraster[[i]][[j]] <- rasterize(abulist_longsplit[[i]][[j]][,c("lon","lat")], r, abulist_longsplit[[i]][[j]][,"Abundance"], fun=mean)
    lraster[[i]][[j]] <- disaggregate(lraster[[i]][[j]],2, method = "bilinear")
  }
}

image(lraster[[1]][[1]],col=tim.colors(99))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate GIFs of the predicted abundance with ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Plotting raster on ggplot isn't very straightforward.
#In order to plot them, one needs first to convert the raster file into a dataframe.
#But before doing that, one needs to convert the raster to a spatialpixeldataframe (SPDF) and afterwards,
#convert the SPDF to a conventional dataframe (DF).
#I also created manually a vector of titles for all the 48 maps, and which will be included within the ggplot script.



# Convert rasterlayer to a DF (raster -> SPDF -> DF)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#r.spdf2_bothCT <- as(raster_bothCT, "SpatialPixelsDataFrame")
#r.df2 <- as.data.frame(r.spdf2)

# Combined
r.spdf2 <- rep(list(vector("list", 180)),3)
r.df2 <- rep(list(vector("list", 180)),3)
for(i in 1:length(lraster)){
  for(j in seq_along(lraster[[i]])){
    r.spdf2[[i]][[j]] <- as(lraster[[i]][[j]],"SpatialPixelsDataFrame")
    r.df2[[i]][[j]] <- as.data.frame(r.spdf2[[i]][[j]])
  }
}




# Create shapefile for study area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data("worldHiresMapEnv")
# DK_coast_poly <- map("worldHires",  fill=TRUE, col="transparent",
#                      plot=FALSE, xlim=c(9,15.5), ylim=c(54.5,58))
# DK_coast_poly$names
# IDs <- sapply(strsplit(DK_coast_poly$names, ":"), function(x) x[1])
# DK_poly <- map2SpatialPolygons(DK_coast_poly, IDs=IDs,
#                                proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))


setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Shapefile/")
DK <- readOGR(".", "DK") 

UTMzone  <- 32
DK <- spTransform(DK, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM

ext <- extent(9, 15.5, 53.7, 56.5)
DK2 <- crop(DK,ext)
crs(DK2)   <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 

DK2@data$id <- rownames(DK2@data)
DKPoints <- fortify(DK2, region = "id")
DKDF <- merge(DKPoints, DK2@data, by = "id")



#  Go for the plot
#~~~~~~~~~~~~~~~~~~~~

r.df2[[1]][[105]]

pal <- fish(99, option = "Epinephelus_lanceolatus",end=0.8)

ggplot() +
  geom_tile(data=r.df2[[3]][[180]],aes(x=x,y=y,fill = layer)) +
  #geom_polygon(data=DK, aes(x=long, y=lat, group=group), fill="black", colour="black") + 
  geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
  #scale_fill_gradientn(colours = rev(parula(99)[99:1])) +
  scale_fill_gradientn(colours = pal) +
  coord_map() +
  scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
  scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
  #labs(x = "Longitude (°)", y="Latitude (°)") +
  theme_bw() +
  theme(#legend.text = element_text(size=13),
    legend.position = "none",
    
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    #axis.text.x = element_text(size=16),
    #axis.text.y = element_text(size=16),
    #axis.title.y = element_text(margin=margin(t = 0, r = 18, b = 0, l = 0),size=16),
    #axis.title.x = element_text(margin=margin(t=18,r=0,b=0,l=0),size=16),
    axis.title = element_blank(),
    #plot.title = element_text(hjust = 0.5,size=18,face="bold"),
    plot.title = element_text(hjust = 0.5,size=28,face="bold"),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    plot.margin = unit(c(0, 0, 0, 0), "cm"))



setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Schematic_overview")
ggsave("WBScod_A2_t180.png",dpi=300)



