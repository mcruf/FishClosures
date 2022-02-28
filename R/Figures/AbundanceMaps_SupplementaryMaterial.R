

#############################################################################
#                                                                           #
#    Paper III: Identifying cod abundance hotspots in the WBS (part 1)      #
#                                                                           #
#############################################################################



# I have run the LGNB model for the WBS cod on a YearMonth (2005-2019) resolution and grid size 10 x 10km.
# The model was run from age0-age5+, hence I have monthly maps from 2005-2019 for each age group.

# To identify spawner hotpspot, I will select only those months in which the peak of the spawning season occur.
# Following Hüssy (2011) and the 2019-ICES report (http://ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/acom/2019/WKBALTCOD2/WKBALTCOD2%202019.pdf)
# the main spawning peak occurs from JANUARY-MARCH every year.

# For the recruits hotspot, I will have to take all the months into consideration.



# In order to identify and evaluate the persistency of recruits (A0-A2) and spawners (A3-A5+) abundance hotspot,I will have to do three consecutive steps:

# Step 1) Stack the YearMonth abundance layers of the individual age groups. For each YearMonth map, I then take the sum over all abundance values.
# This will provide me with a "combined abundance" layer for each YearMonth.

# Step 2) Take the generated YearMonth maps and evaluate the abundance hotspots following Bartolino's et al. (2011) approach - A frequency distribution approach to hotspot identification

# Step 3) Evaluate the presistency of the identified hotpost along the considered time period by using Colloca's et al. (2009) approach - Identifying fish nurseries using density and presistence areas



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
library(animation)




#### Spawners ####




## Set working directoy of the LGNB results
#setwd("D:/PhD/Project III/Results/Spawning_HotSpots/YearQuarter") #For YearQuarter resolution
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




# 1.2) Transform the abundance to Kasper's scale
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A3_t <- apply(A3,2,concTransform)
A4_t <- apply(A4,2,concTransform)
A5_t <- apply(A5,2,concTransform)



# 1.3) Calculate the mean abundance within year
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A3_meanAbuYear <- data.frame(Y2005 = rowMeans(A3_t[,c(1:3)]),
                             Y2006 = rowMeans(A3_t[,c(4:6)]),
                             Y2007 = rowMeans(A3_t[,c(7:9)]),
                             Y2008 = rowMeans(A3_t[,c(10:12)]),
                             Y2009 = rowMeans(A3_t[,c(13:15)]),
                             Y2010 = rowMeans(A3_t[,c(16:18)]),
                             Y2011 = rowMeans(A3_t[,c(19:21)]),
                             Y2012 = rowMeans(A3_t[,c(22:24)]),
                             Y2013 = rowMeans(A3_t[,c(25:27)]),
                             Y2014 = rowMeans(A3_t[,c(28:30)]),
                             Y2015 = rowMeans(A3_t[,c(31:33)]),
                             Y2016 = rowMeans(A3_t[,c(34:36)]),
                             Y2017 = rowMeans(A3_t[,c(37:39)]),
                             Y2018 = rowMeans(A3_t[,c(40:42)]),
                             Y2019 = rowMeans(A3_t[,c(43:45)]))



A4_meanAbuYear <- data.frame(Y2005 = rowMeans(A4_t[,c(1:3)]),
                             Y2006 = rowMeans(A4_t[,c(4:6)]),
                             Y2007 = rowMeans(A4_t[,c(7:9)]),
                             Y2008 = rowMeans(A4_t[,c(10:12)]),
                             Y2009 = rowMeans(A4_t[,c(13:15)]),
                             Y2010 = rowMeans(A4_t[,c(16:18)]),
                             Y2011 = rowMeans(A4_t[,c(19:21)]),
                             Y2012 = rowMeans(A4_t[,c(22:24)]),
                             Y2013 = rowMeans(A4_t[,c(25:27)]),
                             Y2014 = rowMeans(A4_t[,c(28:30)]),
                             Y2015 = rowMeans(A4_t[,c(31:33)]),
                             Y2016 = rowMeans(A4_t[,c(34:36)]),
                             Y2017 = rowMeans(A4_t[,c(37:39)]),
                             Y2018 = rowMeans(A4_t[,c(40:42)]),
                             Y2019 = rowMeans(A4_t[,c(43:45)]))




A5_meanAbuYear <- data.frame(Y2005 = rowMeans(A5_t[,c(1:3)]),
                             Y2006 = rowMeans(A5_t[,c(4:6)]),
                             Y2007 = rowMeans(A5_t[,c(7:9)]),
                             Y2008 = rowMeans(A5_t[,c(10:12)]),
                             Y2009 = rowMeans(A5_t[,c(13:15)]),
                             Y2010 = rowMeans(A5_t[,c(16:18)]),
                             Y2011 = rowMeans(A5_t[,c(19:21)]),
                             Y2012 = rowMeans(A5_t[,c(22:24)]),
                             Y2013 = rowMeans(A5_t[,c(25:27)]),
                             Y2014 = rowMeans(A5_t[,c(28:30)]),
                             Y2015 = rowMeans(A5_t[,c(31:33)]),
                             Y2016 = rowMeans(A5_t[,c(34:36)]),
                             Y2017 = rowMeans(A5_t[,c(37:39)]),
                             Y2018 = rowMeans(A5_t[,c(40:42)]),
                             Y2019 = rowMeans(A5_t[,c(43:45)]))




# 1.4) Generate raster files from the estimated abundances
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We need to do this step if we want to produce the maps on ggplot.
# To do so, we first neet do create a dataframe with the grids long/lat and the abundance associated to this locations.
# Afterwards we need to create an "empty raster" because we have an unregularly spaced grid

load("results_WBScod_m1_A3_both_No_One_noprofile_.RData")


# Create a dataframe with the grid lon/lat and the associated abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A3_meanAbuYear <- cbind(gr,A3_meanAbuYear)
A4_meanAbuYear <- cbind(gr,A4_meanAbuYear)
A5_meanAbuYear <- cbind(gr,A5_meanAbuYear)



# Create an empty rasterfile based on the dataframe from the previous step
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
e <- extent(as.matrix(gr))
#r <- raster(e,ncol=55,nrow=55) #WBS cod for a 5x5 km grid (in YearQuarter case)
#r <- raster(e,ncol=33,nrow=37) #WBS cod for a 10x10 km grid (in YearMonth case)
r <- raster(e,ncol=30,nrow=30) #WBS cod for a 10x10 km grid (in YearMonth case)



abulist <- list(A3_meanAbuYear,A4_meanAbuYear,A5_meanAbuYear)
names(abulist) <- c("A3","A4","A5")





# Convert df from wide to long format
abulist_long <- list()
for(i in seq_along(abulist)){
  abulist_long[[i]] <- gather(abulist[[i]],key=Year, value=Abundance, -c(lon,lat))
}


# Set Year to factor and put it in the right order
for(i in seq_along(abulist_long)){
  abulist_long[[i]]$Year <- as.factor(abulist_long[[i]]$Year)
  abulist_long[[i]]$Year <- factor(abulist_long[[i]]$Year, levels = c(paste("Y",2005:2019,sep="")))
}


# Rasterize each abundance info
abulist_longsplit <- list()
for(i in seq_along(abulist_long)){
  abulist_longsplit[[i]] <- split(abulist_long[[i]], list(abulist_long[[i]]$Year))
}

#str(abulist_longsplit)


lraster <- rep(list(vector("list", 15)),3) #we have 15 years of data (2005-2019)


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
r.spdf2 <- rep(list(vector("list", 15)),3)
r.df2 <- rep(list(vector("list", 15)),3)
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




titles <- as.factor(c(paste(2005:2019)))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Go for the plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pal <- fish(99, option = "Epinephelus_lanceolatus",end=0.8)



# Age-3 plots
A3_plot <- list()
for(i in 1:15){
  A3_plot[[i]] <-  print(ggplot() +
                              geom_tile(data=r.df2[[1]][[i]],aes(x=x,y=y,fill = layer)) +
                              ggtitle(titles[[i]]) +
                              geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                              scale_fill_gradientn(colours = pal) +
                              coord_map() +
                              scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                              scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
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
                                  plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                                  panel.border = element_rect(colour = "black", fill=NA, size=2),
                                  plot.margin = unit(c(0, 0, 0, 0), "cm")))
    
}



# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")
# for (i in 1:15) {
#   file_name = paste("A3_year_", i, ".tiff", sep="")
#  #tiff(file_name, units="in", width=8, height=11.5, res=300)
#   tiff(file_name, units="in", width=6, height=9.5, res=300)
#   print(A3_plot[[i]])
#   dev.off()
# }
# 




# Age-4 plots
A4_plot <- list()
for(i in 1:15){
  A4_plot[[i]] <-  print(ggplot() +
                           geom_tile(data=r.df2[[2]][[i]],aes(x=x,y=y,fill = layer)) +
                           ggtitle(titles[[i]]) +
                           geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                           scale_fill_gradientn(colours = pal) +
                           coord_map() +
                           scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                           scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
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
                             plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                             panel.border = element_rect(colour = "black", fill=NA, size=2),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")))
  
}

# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")
# for (i in 1:15) {
#   file_name = paste("A4_year_", i, ".tiff", sep="")
#   #tiff(file_name, units="in", width=8, height=11.5, res=300)
#   tiff(file_name, units="in", width=6, height=9.5, res=300)
#   print(A4_plot[[i]])
#   dev.off()
# }



# Age-5+ plots
A5_plot <- list()
for(i in 1:15){
  A5_plot[[i]] <-  print(ggplot() +
                           geom_tile(data=r.df2[[3]][[i]],aes(x=x,y=y,fill = layer)) +
                           ggtitle(titles[[i]]) +
                           geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                           scale_fill_gradientn(colours = pal) +
                           coord_map() +
                           scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                           scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
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
                             plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                             panel.border = element_rect(colour = "black", fill=NA, size=2),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")))
  
}

# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")
# for (i in 1:15) {
#   file_name = paste("A5_year_", i, ".tiff", sep="")
#   #tiff(file_name, units="in", width=8, height=11.5, res=300)
#   tiff(file_name, units="in", width=6, height=9.5, res=300)
#   print(A5_plot[[i]])
#   dev.off()
# }
















#################################################### Backup stuff ###########################################
# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/")
# #tiff("Spawners_YearAverage_2005-2019.tiff", units="in", width=8, height=11.5, res=350)
# tiff("Spawners_YearAverage_2005-2019.tiff", units="cm", width=21, height=29, res=350)
# 
# grid.arrange(A3_plot[[1]],A4_plot[[1]],A5_plot[[1]],
#              A3_plot[[2]],A4_plot[[2]],A5_plot[[2]],
#              A3_plot[[3]],A4_plot[[3]],A5_plot[[3]],
#              A3_plot[[4]],A4_plot[[4]],A5_plot[[4]],
#              A3_plot[[5]],A4_plot[[5]],A5_plot[[5]],
#              A3_plot[[6]],A4_plot[[6]],A5_plot[[6]],
#              A3_plot[[7]],A4_plot[[7]],A5_plot[[7]],
#              A3_plot[[8]],A4_plot[[8]],A5_plot[[8]],
#              A3_plot[[9]],A4_plot[[9]],A5_plot[[9]],
#              A3_plot[[10]],A4_plot[[10]],A5_plot[[10]],
#              A3_plot[[11]],A4_plot[[11]],A5_plot[[11]],
#              A3_plot[[12]],A4_plot[[12]],A5_plot[[12]],
#              A3_plot[[13]],A4_plot[[13]],A5_plot[[13]],
#              A3_plot[[14]],A4_plot[[14]],A5_plot[[14]],
#              A3_plot[[15]],A4_plot[[15]],A5_plot[[15]], ncol=3)
# dev.off()
# 



setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")

tiff("Spawners_Y1-Y8.tiff", units="cm", width=21, height=29, res=350)
grid.arrange(A3_plot[[1]],A4_plot[[1]],A5_plot[[1]],
             A3_plot[[2]],A4_plot[[2]],A5_plot[[2]],
             A3_plot[[3]],A4_plot[[3]],A5_plot[[3]],
             A3_plot[[4]],A4_plot[[4]],A5_plot[[4]],
             A3_plot[[5]],A4_plot[[5]],A5_plot[[5]],
             A3_plot[[6]],A4_plot[[6]],A5_plot[[6]],
             A3_plot[[7]],A4_plot[[7]],A5_plot[[7]],
             A3_plot[[8]],A4_plot[[8]],A5_plot[[8]],
             ncol=3)
dev.off()


setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")

tiff("Spawners_Y9-Y15.tiff", units="cm", width=21, height=29, res=350)
grid.arrange( A3_plot[[9]],A4_plot[[9]],A5_plot[[9]],
              A3_plot[[10]],A4_plot[[10]],A5_plot[[10]],
              A3_plot[[11]],A4_plot[[11]],A5_plot[[11]],
              A3_plot[[12]],A4_plot[[12]],A5_plot[[12]],
              A3_plot[[13]],A4_plot[[13]],A5_plot[[13]],
              A3_plot[[14]],A4_plot[[14]],A5_plot[[14]],
              A3_plot[[15]],A4_plot[[15]],A5_plot[[15]],
              ncol=3)
dev.off()




###################################################################################################
#### Spawners ####




## Set working directoy of the LGNB results
#setwd("D:/PhD/Project III/Results/Spawning_HotSpots/YearQuarter") #For YearQuarter resolution
setwd("D:/PhD/Project III/Results/Recruits_HotSpot/YearMonth") #For YearMonth resolution


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Step 1) Stack the age-specific abundances and retrieve total abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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


## A2
load("results_WBScod_m1_A2_both_No_One_noprofile_.RData")
A2 <- as.list(env1$sdr,"Estimate"); A2  <- as.data.frame(A2$eta_density)
rm(list=setdiff(ls(), ls(pattern=c("A0|A1|A2|gr|datatot"))))





# 1.2) Transform the abundance to Kasper's scale
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A0_t <- apply(A0,2,concTransform)
A1_t <- apply(A1,2,concTransform)
A2_t <- apply(A2,2,concTransform)



# 1.3) Calculate the mean abundance within year
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A0_meanAbuYear <- data.frame(Y2005 = rowMeans(A0_t[,c(1:12)]),
                             Y2006 = rowMeans(A0_t[,c(13:24)]),
                             Y2007 = rowMeans(A0_t[,c(25:36)]),
                             Y2008 = rowMeans(A0_t[,c(37:48)]),
                             Y2009 = rowMeans(A0_t[,c(49:60)]),
                             Y2010 = rowMeans(A0_t[,c(61:72)]),
                             Y2011 = rowMeans(A0_t[,c(73:84)]),
                             Y2012 = rowMeans(A0_t[,c(85:96)]),
                             Y2013 = rowMeans(A0_t[,c(97:108)]),
                             Y2014 = rowMeans(A0_t[,c(109:120)]),
                             Y2015 = rowMeans(A0_t[,c(121:132)]),
                             Y2016 = rowMeans(A0_t[,c(133:144)]),
                             Y2017 = rowMeans(A0_t[,c(145:156)]),
                             Y2018 = rowMeans(A0_t[,c(157:168)]),
                             Y2019 = rowMeans(A0_t[,c(169:180)]))



A1_meanAbuYear <- data.frame(Y2005 = rowMeans(A1_t[,c(1:12)]),
                             Y2006 = rowMeans(A1_t[,c(13:24)]),
                             Y2007 = rowMeans(A1_t[,c(25:36)]),
                             Y2008 = rowMeans(A1_t[,c(37:48)]),
                             Y2009 = rowMeans(A1_t[,c(49:60)]),
                             Y2010 = rowMeans(A1_t[,c(61:72)]),
                             Y2011 = rowMeans(A1_t[,c(73:84)]),
                             Y2012 = rowMeans(A1_t[,c(85:96)]),
                             Y2013 = rowMeans(A1_t[,c(97:108)]),
                             Y2014 = rowMeans(A1_t[,c(109:120)]),
                             Y2015 = rowMeans(A1_t[,c(121:132)]),
                             Y2016 = rowMeans(A1_t[,c(133:144)]),
                             Y2017 = rowMeans(A1_t[,c(145:156)]),
                             Y2018 = rowMeans(A1_t[,c(157:168)]),
                             Y2019 = rowMeans(A1_t[,c(169:180)]))




A2_meanAbuYear <- data.frame(Y2005 = rowMeans(A2_t[,c(1:12)]),
                             Y2006 = rowMeans(A2_t[,c(13:24)]),
                             Y2007 = rowMeans(A2_t[,c(25:36)]),
                             Y2008 = rowMeans(A2_t[,c(37:48)]),
                             Y2009 = rowMeans(A2_t[,c(49:60)]),
                             Y2010 = rowMeans(A2_t[,c(61:72)]),
                             Y2011 = rowMeans(A2_t[,c(73:84)]),
                             Y2012 = rowMeans(A2_t[,c(85:96)]),
                             Y2013 = rowMeans(A2_t[,c(97:108)]),
                             Y2014 = rowMeans(A2_t[,c(109:120)]),
                             Y2015 = rowMeans(A2_t[,c(121:132)]),
                             Y2016 = rowMeans(A2_t[,c(133:144)]),
                             Y2017 = rowMeans(A2_t[,c(145:156)]),
                             Y2018 = rowMeans(A2_t[,c(157:168)]),
                             Y2019 = rowMeans(A2_t[,c(169:180)]))



# 1.4) Generate raster files from the estimated abundances
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We need to do this step if we want to produce the maps on ggplot.
# To do so, we first neet do create a dataframe with the grids long/lat and the abundance associated to this locations.
# Afterwards we need to create an "empty raster" because we have an unregularly spaced grid

load("results_WBScod_m1_A2_both_No_One_noprofile_.RData")


# Create a dataframe with the grid lon/lat and the associated abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A0_meanAbuYear <- cbind(gr,A0_meanAbuYear)
A1_meanAbuYear <- cbind(gr,A1_meanAbuYear)
A2_meanAbuYear <- cbind(gr,A2_meanAbuYear)



# Create an empty rasterfile based on the dataframe from the previous step
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
e <- extent(as.matrix(gr))
#r <- raster(e,ncol=55,nrow=55) #WBS cod for a 5x5 km grid (in YearQuarter case)
#r <- raster(e,ncol=33,nrow=37) #WBS cod for a 10x10 km grid (in YearMonth case)
r <- raster(e,ncol=30,nrow=30) #WBS cod for a 10x10 km grid (in YearMonth case)



abulist <- list(A0_meanAbuYear,A1_meanAbuYear,A2_meanAbuYear)
names(abulist) <- c("A0","A1","A2")





# Convert df from wide to long format
abulist_long <- list()
for(i in seq_along(abulist)){
  abulist_long[[i]] <- gather(abulist[[i]],key=Year, value=Abundance, -c(lon,lat))
}


# Set Year to factor and put it in the right order
for(i in seq_along(abulist_long)){
  abulist_long[[i]]$Year <- as.factor(abulist_long[[i]]$Year)
  abulist_long[[i]]$Year <- factor(abulist_long[[i]]$Year, levels = c(paste("Y",2005:2019,sep="")))
}


# Rasterize each abundance info
abulist_longsplit <- list()
for(i in seq_along(abulist_long)){
  abulist_longsplit[[i]] <- split(abulist_long[[i]], list(abulist_long[[i]]$Year))
}

#str(abulist_longsplit)


lraster <- rep(list(vector("list", 15)),3) #we have 15 years of data (2005-2019)


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
r.spdf2 <- rep(list(vector("list", 15)),3)
r.df2 <- rep(list(vector("list", 15)),3)
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




titles <- as.factor(c(paste(2005:2019)))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Go for the plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pal <- fish(99, option = "Epinephelus_lanceolatus",end=0.8)



# Age-0 plots
A0_plot <- list()
for(i in 1:15){
  A0_plot[[i]] <-  print(ggplot() +
                           geom_tile(data=r.df2[[1]][[i]],aes(x=x,y=y,fill = layer)) +
                           ggtitle(titles[[i]]) +
                           geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                           scale_fill_gradientn(colours = pal) +
                           coord_map() +
                           scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                           scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
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
                             plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                             panel.border = element_rect(colour = "black", fill=NA, size=2),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")))
  
}



# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")
# for (i in 1:15) {
#   file_name = paste("A3_year_", i, ".tiff", sep="")
#  #tiff(file_name, units="in", width=8, height=11.5, res=300)
#   tiff(file_name, units="in", width=6, height=9.5, res=300)
#   print(A3_plot[[i]])
#   dev.off()
# }
# 




# Age-1 plots
A1_plot <- list()
for(i in 1:15){
  A1_plot[[i]] <-  print(ggplot() +
                           geom_tile(data=r.df2[[2]][[i]],aes(x=x,y=y,fill = layer)) +
                           ggtitle(titles[[i]]) +
                           geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                           scale_fill_gradientn(colours = pal) +
                           coord_map() +
                           scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                           scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
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
                             plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                             panel.border = element_rect(colour = "black", fill=NA, size=2),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")))
  
}

# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")
# for (i in 1:15) {
#   file_name = paste("A4_year_", i, ".tiff", sep="")
#   #tiff(file_name, units="in", width=8, height=11.5, res=300)
#   tiff(file_name, units="in", width=6, height=9.5, res=300)
#   print(A4_plot[[i]])
#   dev.off()
# }



# Age-5+ plots
A2_plot <- list()
for(i in 1:15){
  A2_plot[[i]] <-  print(ggplot() +
                           geom_tile(data=r.df2[[3]][[i]],aes(x=x,y=y,fill = layer)) +
                           ggtitle(titles[[i]]) +
                           geom_polygon(data = DKDF,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                           scale_fill_gradientn(colours = pal) +
                           coord_map() +
                           scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                           scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
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
                             plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                             panel.border = element_rect(colour = "black", fill=NA, size=2),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")))
  
}

# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")
# for (i in 1:15) {
#   file_name = paste("A5_year_", i, ".tiff", sep="")
#   #tiff(file_name, units="in", width=8, height=11.5, res=300)
#   tiff(file_name, units="in", width=6, height=9.5, res=300)
#   print(A5_plot[[i]])
#   dev.off()
# }
















#################################################### Backup stuff ###########################################



setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")

tiff("Juveniles_Y1-Y8.tiff", units="cm", width=21, height=29, res=350)
grid.arrange(A0_plot[[1]],A1_plot[[1]],A2_plot[[1]],
             A0_plot[[2]],A1_plot[[2]],A2_plot[[2]],
             A0_plot[[3]],A1_plot[[3]],A2_plot[[3]],
             A0_plot[[4]],A1_plot[[4]],A2_plot[[4]],
             A0_plot[[5]],A1_plot[[5]],A2_plot[[5]],
             A0_plot[[6]],A1_plot[[6]],A2_plot[[6]],
             A0_plot[[7]],A1_plot[[7]],A2_plot[[7]],
             A0_plot[[8]],A1_plot[[8]],A2_plot[[8]],
             ncol=3)
dev.off()


setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Abundance_maps")

tiff("Juveniles_Y9-Y15.tiff", units="cm", width=21, height=29, res=350)
grid.arrange( A0_plot[[9]],A1_plot[[9]],A2_plot[[9]],
              A0_plot[[10]],A1_plot[[10]],A2_plot[[10]],
              A0_plot[[11]],A1_plot[[11]],A2_plot[[11]],
              A0_plot[[12]],A1_plot[[12]],A2_plot[[12]],
              A0_plot[[13]],A1_plot[[13]],A2_plot[[13]],
              A0_plot[[14]],A1_plot[[14]],A2_plot[[14]],
              A0_plot[[15]],A1_plot[[15]],A2_plot[[15]],
              ncol=3)
dev.off()






############################### Backup stuff

# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/")
# #tiff("Spawners_YearAverage_2005-2019.tiff", units="in", width=8, height=11.5, res=350)
# tiff("Spawners_YearAverage_2005-2019.tiff", units="cm", width=21, height=29, res=350)
# 
# grid.arrange(A3_plot[[1]],A4_plot[[1]],A5_plot[[1]],
#              A3_plot[[2]],A4_plot[[2]],A5_plot[[2]],
#              A3_plot[[3]],A4_plot[[3]],A5_plot[[3]],
#              A3_plot[[4]],A4_plot[[4]],A5_plot[[4]],
#              A3_plot[[5]],A4_plot[[5]],A5_plot[[5]],
#              A3_plot[[6]],A4_plot[[6]],A5_plot[[6]],
#              A3_plot[[7]],A4_plot[[7]],A5_plot[[7]],
#              A3_plot[[8]],A4_plot[[8]],A5_plot[[8]],
#              A3_plot[[9]],A4_plot[[9]],A5_plot[[9]],
#              A3_plot[[10]],A4_plot[[10]],A5_plot[[10]],
#              A3_plot[[11]],A4_plot[[11]],A5_plot[[11]],
#              A3_plot[[12]],A4_plot[[12]],A5_plot[[12]],
#              A3_plot[[13]],A4_plot[[13]],A5_plot[[13]],
#              A3_plot[[14]],A4_plot[[14]],A5_plot[[14]],
#              A3_plot[[15]],A4_plot[[15]],A5_plot[[15]], ncol=3)
# dev.off()
# 



layOut <- rbind(c(1,2,3),
                c(4,5,6),
                c(7,8,9),
                c(10,11,12),
                c(13,14,15),
                c(16,17,18),
                c(19,20,21),
                c(22,23,24),
                c(25,26,27),
                c(28,29,30),
                c(31,32,33),
                c(34,35,36),
                c(37,38,39),
                c(40,41,42),
                c(43,44,45))

finalplot <- grid.arrange(A3_plot[[1]],A4_plot[[1]],A5_plot[[1]],
                          A3_plot[[2]],A4_plot[[2]],A5_plot[[2]],
                          A3_plot[[3]],A4_plot[[3]],A5_plot[[3]],
                          A3_plot[[4]],A4_plot[[4]],A5_plot[[4]],
                          A3_plot[[5]],A4_plot[[5]],A5_plot[[5]],
                          A3_plot[[6]],A4_plot[[6]],A5_plot[[6]],
                          A3_plot[[7]],A4_plot[[7]],A5_plot[[7]],
                          A3_plot[[8]],A4_plot[[8]],A5_plot[[8]],
                          A3_plot[[9]],A4_plot[[9]],A5_plot[[9]],
                          A3_plot[[10]],A4_plot[[10]],A5_plot[[10]],
                          A3_plot[[11]],A4_plot[[11]],A5_plot[[11]],
                          A3_plot[[12]],A4_plot[[12]],A5_plot[[12]],
                          A3_plot[[13]],A4_plot[[13]],A5_plot[[13]],
                          A3_plot[[14]],A4_plot[[14]],A5_plot[[14]],
                          A3_plot[[15]],A4_plot[[15]],A5_plot[[15]],                            # box plot and scatter plot
                          ncol = 3, nrow = 15, 
                          layout_matrix = layOut)



ggsave("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Spawners_YearAverage_2005-2019.png",
       finalplot, width=90, height=130,units="cm",limitsize=F)


