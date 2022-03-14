##########################################################################################
#                                                                                        #
##                  Plot abundance hotspots of juveniles (recruits)                 ##
##                                    (Rufener et al.)                                  ##
#                                                                                        #
##########################################################################################

# Last update: March 2022

# Code written and mantained by Marie-Christine Rufener
# Contact < macrufener@gmail.com > for any query or to report code issues.



# The following script corresponds to the figures in the Supplementary Material 
# in which the inter-annual abundance dyanmic is displayed.


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 1: Set default inputs & R libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1.1) Choose life stage for which the abundance hotspot should be identified; 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Juveniles: A0-A2
## Spawners: A3-A5+


Lstage <- c("Juveniles","Spawners")[2] # Default = Juveniles




# 1.2) Load R libraries
#~~~~~~~~~~~~~~~~~~~~~~~~
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
library(fishualize)







#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 2: Load data files & data wrangling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note: the LGNB-SDM output provides a dataframe where each column is a time-period.
# Since we ran the model from 2005-2019 on a monthly basis, this means that we will
# have 180 columns (V1-V180).

# For the juveniles we will use all months (i.e., 180 months along the timeseries),
# whereas for spawners we will use only the first three months of each year that
# corresponds to the spawning perios (thus, 45 months along the timeseries)



# 2.1) Load the results files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if(Lstage == "Juveniles"){
  
  ## Set WD where the juveniles results are stored
  #setwd("~/Data/Hotspots/Recruits/")
  setwd("C:/Users/mruf/Documents/Fish_Closures/Data/Hotspots/Recruits/")
  
  
  
  ## A0
  load("A0.RData")
  rm(list=setdiff(ls(), ls(pattern=c("Lstage|A0|A1|A2|A3|A4|A5|gr|spawn_month"))))
  
  
  ## A1
  load("A1.RData")
  rm(list=setdiff(ls(), ls(pattern=c("Lstage|A0|A1|A2|A3|A4|A5|gr|spawn_month"))))
  
  
  ## A2
  load("A2.RData")
  rm(list=setdiff(ls(), ls(pattern=c("Lstage|A0|A1|A2|A3|A4|A5|gr|spawn_month"))))
  
  
  
} else if(Lstage == "Spawners"){
  
  
  ## Set WD where the recruits results are stored
  #setwd("~/Data/Hotspots/Spawners/")
  setwd("C:/Users/mruf/Documents/Fish_Closures/Data/Hotspots/Spawners/")
  
  
  
  
  ##  Select columns corresponding to the spawning monhts (JAN-MARCH)
  spawn_months <- c("V1","V2","V3", #2005
                    "V13","V14","V15", #2006
                    "V25","V26","V27", #2007
                    "V37","V38","V39", #2008
                    "V49","V50","V51", #2009
                    "V61","V62","V63", #2010
                    "V73","V74","V75", #2011
                    "V85","V86","V87", #2012
                    "V97","V98","V99", #2013
                    "V109","V110","V111", #2014
                    "V121","V122","V123", #2015
                    "V133","V134","V135", #2016
                    "V145","V146","V147", #2017
                    "V157","V158","V159", #2018
                    "V169","V170","V171") #2019
  
  
  ## A3
  load("A3.RData")
  A3 <- A3[,spawn_months] #Select only the columns corresponding to spawning period
  colnames(A3) <- paste("V",1:ncol(A3),sep="") #Rename for readbility

  rm(list=setdiff(ls(), ls(pattern=c("Lstage|A0|A1|A2|A3|A4|A5|gr|spawn_month"))))
  
  
  ## A4
  load("A4.RData")
  A4 <- A4[,spawn_months] #Select only the columns corresponding to spawning period
  colnames(A4) <- paste("V",1:ncol(A4),sep="") #Rename for readbility

  
  rm(list=setdiff(ls(), ls(pattern=c("Lstage|A0|A1|A2|A3|A4|A5|gr|spawn_month"))))
  
  
  ## A5
  load("A5.RData")
  A5 <- A5[,spawn_months] #Select only the columns corresponding to spawning period
  colnames(A5) <- paste("V",1:ncol(A5),sep="") #Rename for readbility

  rm(list=setdiff(ls(), ls(pattern=c("Lstage|A0|A1|A2|A3|A4|A5|gr|spawn_month"))))
  
}



# 2.2) Standardize abundaces (for better visualization)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(Lstage == "Juveniles"){
  A0_t <- as.data.frame(apply(A0, 2, concTransform)) 
  A1_t <- as.data.frame(apply(A1, 2, concTransform)) 
  A2_t <- as.data.frame(apply(A2, 2, concTransform)) 
  
  abulist <- list(A0_t, A1_t, A2_t)
  
} else if(Lstage == "Spawners"){
  
  A3_t <- as.data.frame(apply(A3, 2, concTransform)) 
  A4_t <- as.data.frame(apply(A4, 2, concTransform)) 
  A5_t <- as.data.frame(apply(A5, 2, concTransform))  
  
  abulist <- list(A3_t, A4_t, A5_t)
  
}



# 2.3) Inset spatial coordinates of the prediction grid
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(i in seq_along(abulist)){
  abulist[[i]]$long <- gr$lon
  abulist[[i]]$lat <- gr$lat
}



# 2.3) Calculate mean abundance (within year)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# But first we need to reshape the dataset (wide -> long format),
# and then set a column for the year

if(Lstage == "Juveniles"){
  
    ## Reshape from wide to long  
    abulistr <- lapply(abulist, gather, Time, Abundance, V1:V180)
    
    
    ## Set column for year
    for(i in seq_along(abulistr)){
      abulistr[[i]]$Time <- as.factor(abulistr[[i]]$Time)
      
      abulistr[[i]]$Year <- ifelse(abulistr[[i]]$Time %in% paste("V", 1:12, sep=""), "2005",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 13:24, sep=""), "2006",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 25:36, sep=""), "2007",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 37:48, sep=""), "2008",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 49:60, sep=""), "2009",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 61:72, sep=""), "2010",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 73:84, sep=""), "2011",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 85:96, sep=""), "2012",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 97:108, sep=""), "2013",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 109:120, sep=""), "2014",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 121:132, sep=""), "2015",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 133:144, sep=""), "2016",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 145:156, sep=""), "2017",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 157:168, sep=""), "2018",
                            "2019" ))))))))))))))
      
      abulistr[[i]]$Year <- as.factor(abulistr[[i]]$Year)

    }
    
    
    
    
    
} else if (Lstage == "Spawners"){
  
    ## Reshape from wide to long
    abulistr <- lapply(abulist, gather, Time, Abundance, V1:V45)
    
    
    ## Set column for year
    for(i in seq_along(abulistr)){
      abulistr[[i]]$Time <- as.factor(abulistr[[i]]$Time)
      
      abulistr[[i]]$Year <- ifelse(abulistr[[i]]$Time %in% paste("V", 1:3, sep=""), "2005",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 4:6, sep=""), "2006",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 7:9, sep=""), "2007",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 10:12, sep=""), "2008",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 13:15, sep=""), "2009",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 16:18, sep=""), "2010",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 19:21, sep=""), "2011",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 22:24, sep=""), "2012",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 25:27, sep=""), "2013",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 28:30, sep=""), "2014",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 31:33, sep=""), "2015",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 34:36, sep=""), "2016",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 37:39, sep=""), "2017",
                            ifelse(abulistr[[i]]$Time %in% paste("V", 40:42, sep=""), "2018",
                            "2019" ))))))))))))))
      
      abulistr[[i]]$Year <- as.factor(abulistr[[i]]$Year)
      
    }
  
}


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Plotting predicted abundances 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 3.1) Convert abundance estimates to raster files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 3.1.1) Create an empty raster
e <- extent(as.matrix(gr))
r <- raster(e, ncol=30, nrow=30) 



## 3.1.2) Split list of data frames according to year
aburaster <- list()

for(i in seq_along(abulistr)){
  aburaster[[i]] <- split(abulistr[[i]], list(abulistr[[i]]$Year))
}


Nyears <- nlevels(abulistr[[1]]$Year)



## 3.1.4) Rasterize abundances
lraster <- rep(list(vector("list", Nyears)), 3) #Create an empty list of lists to store the results

for(i in seq_along(aburaster)){
  for(j in seq_along(aburaster[[i]])){
    lraster[[i]][[j]] <- rasterize(aburaster[[i]][[j]][,c("long","lat")], r, aburaster[[i]][[j]][,"Abundance"], fun=mean)
    lraster[[i]][[j]] <- disaggregate(lraster[[i]][[j]],2, method = "bilinear")
  }
}


## To see the progress....
image(lraster[[1]][[1]], col = tim.colors(99))



# 3.2) Go for the plot
#~~~~~~~~~~~~~~~~~~~~~~
# Plotting raster on ggplot isn't very straightforward.
# To do so, one needs first to convert the raster file into a dataframe.
# But before doing that, one needs to convert the raster to a spatialpixeldataframe (SPDF), 
# and then convert the SPDF to a conventional dataframe (DF).


## 3.2.1) Convert raster layers to a DF (raster -> SPDF -> DF)
# Combined
r.spdf2 <- rep(list(vector("list", Nyears)),3)
r.df2 <- rep(list(vector("list", Nyears)),3)

for(i in 1:length(lraster)){
  for(j in seq_along(lraster[[i]])){
    r.spdf2[[i]][[j]] <- as(lraster[[i]][[j]],"SpatialPixelsDataFrame")
    r.df2[[i]][[j]] <- as.data.frame(r.spdf2[[i]][[j]])
  }
}



## 3.2.2) Create shapefile of the study area
data("worldHiresMapEnv")
DK_coast_poly <- map("worldHires",  fill=TRUE, col="transparent",
                     plot=FALSE, xlim=c(9,15.5), ylim=c(54.5,58))
DK_coast_poly$names
IDs <- sapply(strsplit(DK_coast_poly$names, ":"), function(x) x[1])
DK_poly <- map2SpatialPolygons(DK_coast_poly, IDs=IDs,
                               proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))



## 3.2.3) Now we finally can plot the results...

titles <- as.factor(c(paste(2005:2019)))
pal <- fish(99, option = "Epinephelus_lanceolatus",end=0.8)



if(Lstage == "Juveniles"){

  
# Age-0 plots
A0_plot <- list()
for(i in 1:15){
  A0_plot[[i]] <-  print(ggplot() +
                           geom_tile(data=r.df2[[1]][[i]],aes(x=x,y=y,fill = layer)) +
                           ggtitle(titles[[i]]) +
                           geom_polygon(data = DK_poly, aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                           scale_fill_gradientn(colours = pal) +
                           coord_map() +
                           scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                           scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
                           theme_bw() +
                           theme(
                             legend.position = "none",
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.title = element_blank(),
                             plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                             panel.border = element_rect(colour = "black", fill=NA, size=2),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")))
  
}


# Age-1 plot
A1_plot <- list()
for(i in 1:15){
  A1_plot[[i]] <-  print(ggplot() +
                           geom_tile(data=r.df2[[2]][[i]],aes(x=x,y=y,fill = layer)) +
                           ggtitle(titles[[i]]) +
                           geom_polygon(data = DK_poly, aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                           scale_fill_gradientn(colours = pal) +
                           coord_map() +
                           scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                           scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
                           theme_bw() +
                           theme(
                             legend.position = "none",
                             axis.text.x = element_blank(),                             axis.title = element_blank(),
                             plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                             panel.border = element_rect(colour = "black", fill=NA, size=2),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")))
    }
  
# Age-2 plots
A2_plot <- list()
for(i in 1:15){
  A2_plot[[i]] <-  print(ggplot() +
                           geom_tile(data=r.df2[[3]][[i]],aes(x=x,y=y,fill = layer)) +
                           ggtitle(titles[[i]]) +
                           geom_polygon(data = DK_poly, aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                           scale_fill_gradientn(colours = pal) +
                           coord_map() +
                           scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                           scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
                           theme_bw() +
                           theme(
                             legend.position = "none",
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.title = element_blank(),
                             plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                             panel.border = element_rect(colour = "black", fill=NA, size=2),
                             plot.margin = unit(c(0, 0, 0, 0), "cm")))
}


  } else if(Lstage == "Spawners"){
    
    
    # Age-3 plots
    A3_plot <- list()
    for(i in 1:15){
      A3_plot[[i]] <-  print(ggplot() +
                               geom_tile(data=r.df2[[1]][[i]],aes(x=x,y=y,fill = layer)) +
                               ggtitle(titles[[i]]) +
                               geom_polygon(data = DK_poly,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                               scale_fill_gradientn(colours = pal) +
                               coord_map() +
                               scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                               scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
                               theme_bw() +
                               theme(
                                 legend.position = "none",
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.title = element_blank(),
                                 plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                                 panel.border = element_rect(colour = "black", fill=NA, size=2),
                                 plot.margin = unit(c(0, 0, 0, 0), "cm")))
      
    }
    
    
    # Age-4 plot
    A4_plot <- list()
    for(i in 1:15){
      A4_plot[[i]] <-  print(ggplot() +
                               geom_tile(data=r.df2[[2]][[i]],aes(x=x,y=y,fill = layer)) +
                               ggtitle(titles[[i]]) +
                               geom_polygon(data = DK_poly,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                               scale_fill_gradientn(colours = pal) +
                               coord_map() +
                               scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                               scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
                               theme_bw() +
                               theme(
                                 legend.position = "none",
                                 axis.text.x = element_blank(),                             axis.title = element_blank(),
                                 plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                                 panel.border = element_rect(colour = "black", fill=NA, size=2),
                                 plot.margin = unit(c(0, 0, 0, 0), "cm")))
    }
    
    # Age-5+ plots
    A5_plot <- list()
    for(i in 1:15){
      A5_plot[[i]] <-  print(ggplot() +
                               geom_tile(data=r.df2[[3]][[i]],aes(x=x,y=y,fill = layer)) +
                               ggtitle(titles[[i]]) +
                               geom_polygon(data = DK_poly,aes(x = long, y = lat, group = group),fill="grey70",colour="gray20") +
                               scale_fill_gradientn(colours = pal) +
                               coord_map() +
                               scale_x_continuous(limits=c(9, 15.5), expand = c(0,0)) +
                               scale_y_continuous(limits=c(53.7, 56.5),expand = c(0,0)) +
                               theme_bw() +
                               theme(
                                 legend.position = "none",
                                 axis.text.x = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.title = element_blank(),
                                 plot.title = element_text(hjust = 0.5,size=24,face="bold"),
                                 panel.border = element_rect(colour = "black", fill=NA, size=2),
                                 plot.margin = unit(c(0, 0, 0, 0), "cm")))
    }
    
}




## Now arrange and save results...


setwd("~/Figures/Supplementary_material/Abundance_maps")


if(Lstage == "Juveniles"){
  
  ## Results from 2005-2012
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


  ## Results from 2013-2019
  tiff("Juveniles_Y9-Y15.tiff", units="cm", width=21, height=29, res=350)
  grid.arrange(A0_plot[[9]],A1_plot[[9]],A2_plot[[9]],
               A0_plot[[10]],A1_plot[[10]],A2_plot[[10]],
               A0_plot[[11]],A1_plot[[11]],A2_plot[[11]],
               A0_plot[[12]],A1_plot[[12]],A2_plot[[12]],
               A0_plot[[13]],A1_plot[[13]],A2_plot[[13]],
               A0_plot[[14]],A1_plot[[14]],A2_plot[[14]],
               A0_plot[[15]],A1_plot[[15]],A2_plot[[15]],
               ncol=3)
  dev.off()



} else if(Lstage == "Spawners"){
  
  
  ## Results from 2005-2012
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
  
  ## Results from 20013-2019
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
  
}