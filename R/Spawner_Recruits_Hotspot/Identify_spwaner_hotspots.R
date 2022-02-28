

#############################################################################
#                                                                           #
#    Paper III: Identifying cod abundance hotspots in the WBS (part 1)      #
#                                                                           #
#############################################################################



####  Spawner Hotspots  ####



# I have run the LGNB model for the WBS cod on a YearMonth (2005-2019) resolution and grid size 10 x 10km.
# The model was run from age0-age5+, hence I have monthly maps from 2005-2019 for each age group.

# To identify spawner hotpspot, I will select only those months in which the peak of the spawning season occur.
# Following Hüssy (2011) and the 2019-ICES report (http://ices.dk/sites/pub/Publication%20Reports/Expert%20Group%20Report/acom/2019/WKBALTCOD2/WKBALTCOD2%202019.pdf)
# the main spawning peak occurs from JANUARY-MARCH every year.

# For the recruits hotspot, I will have to take all the months into consideration (see script: Identify_recruits_hotspots).



# In order to identify and evaluate the persistency of spawners (A3-A5+) abundance hotspot,I will have to do three consecutive steps:

# Step 1) Stack the YearMonth abundance layers of the individual age groups. For each YearMonth map, I then take the sum over all abundance values.
# This will provide me with a "combined abundance" layer for each YearMonth.

# Step 2) Take the generated YearMonth maps and evaluate the abundance hotspots following Bartolino's et al. (2011) approach - A frequency distribution approach to hotspot identification

# Step 3) Evaluate the presistency of the identified hotpost along the considered time period by using Colloca's et al. (2009) approach - Identifying fish nurseries using density and presistence areas



library(ggplot2)
library(ggpubr)
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


abulist <- list(A3,A4,A5); names(abulist) <- paste("Age",3:5,sep="")



# data("worldHiresMapEnv")
# DK_coast_poly <- map("worldHires",  fill=TRUE, col="transparent",
#                      plot=FALSE, xlim=c(9,15.5), ylim=c(54.5,58))
# DK_coast_poly$names
# IDs <- sapply(strsplit(DK_coast_poly$names, ":"), function(x) x[1])
# DK_poly <- map2SpatialPolygons(DK_coast_poly, IDs=IDs,
#                                proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
# plot(DK_poly,col=1,add=T)



# saveGIF({
#   for (i in 1:180){
#     plot(gr,type="n",xlab="Longitude",ylab="Latitude",las=1,xlim=c(9.25,15.4))
#     image(gr, concTransform(A2[,i]),
#           col=tim.colors(99),
#           #map=quote(map("worldHires",add=TRUE, fill=TRUE, col="grey70")),
#           #map=quote(plot(DK_poly,add=T,col="grey70")),
#           add=TRUE)
#     plot(DK_poly,col=1,add=T)
#     title(levels(datatot$YearMonth)[i])
#   }},
#   movie.name = "A2_Monthly.gif", interval = 0.2,
#   ani.width = 600)





# 1.2) Transform the abundance to natural scale
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
abulist_exp <- lapply(abulist,exp)


# Bind grid lon/lat and the associated abundance
# for(i in seq_along(abulist_exp)){
#   abulist_exp[[i]]$long <- gr$lon
#   abulist_exp[[i]]$lat <- gr$lat
# }
# 


# 1.3) Sum abundances of each YearMonth across all age groups
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This means, for example, that I take the first month in each age group 
# and sum these abundances for each row. Thus, I will have a dataframe
# with as many rows and columns as the original one.


dfstack <- NULL
for(i in seq_along(abulist_exp[[1]])){
p <- rowSums(sapply(abulist_exp, `[[`, i), na.rm = TRUE)
dfstack <- cbind(dfstack,p)
}

colnames(dfstack) <- colnames(abulist_exp[[1]]) #take an arbitray number among the list

# for(i in 1:ncol(dfstack)){
# image(gr, dfstack[,i],col=tim.colors(99))
# }



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Step 2) Evaluate abundance hotspots for each time step
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Following Bartolino's approach of cumulative relative frequency distribution (CRDF)
# A CRDF curve is approximated by plotting the relative valure of a variable z (e.g., density),
# against the frequency distrituion of the same variable. The two axes of the plot are given by:
# x=z/zm  ; where z is the variable of interest and zm is the maximum value that z assumes in the study area
# y=M(x)/N   ; where M(x) is the number of cases (i.e, points, pixels, areas) a value smaller than x is encountered,
# and N is the total number of cases



# 2.1) Compute the CRDF from Bartolino et al. (2011)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# First we need to order the abundance values of each month from smallest to largest
# Secondly, we need to sort these values from smalles to largest.
# Then we apply Bartolino's formula, i.e., we take the abundance value of a certain cell
# and divide it by the max abundance.
# 

# Function to define the X-axis of Bartolino's method
concTransform2 <- function(x) 
{
  i <- order(x)
  ys <- sort(x)
  p <- ys/max(ys)
  #x[i] <- p
  #x
  p
}


# Function to define the Y-axis of Bartolino's method
abu_order <- function(x) 
{
  s <- 1:length(x)
  l <- length(x)
  z <- s/l
  z
}


## Define X-axis
Xaxis <- apply(dfstack, 2, concTransform2)


## Y-axis
Yaxis <- apply(dfstack, 2, abu_order)

# Check what happens
for(i in 1:ncol(Xaxis)){
  plot(Xaxis[,i],Yaxis[,i],type="s",main=colnames(dfstack)[i])
}



# 2.2) Set the abundance treshold
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we still apply Bartolino's method, which defines as treshold
# the value demarked by a 45degree tangent. Thus, values below the 45o will not be regarded
# as an abundance hotspot, and vice-versa



# Aqui eu pego o valor aproximado da tangente de 45graus com a ajuda do locator.

# Run lines below if running the script for the first time
# treshold <- list()
# for(i in 1:ncol(Xaxis)){ #either Xaxis or Yaxis, never mind
#   plot(Xaxis[,i],Yaxis[,i],type="s",lwd=2,main=colnames(dfstack)[i])
#   abline(0,1,col=3,lwd=2)
#   treshold[[i]] <- locator()
# }  
# 
# 
# ## Keep only the x-values of the treshod
# treshold2 <- NULL
# for(i in seq_along(treshold)){
#   treshold2[i] <- treshold[[i]][1] #1 stands for the "x" element; 2 would be the "y" element of the locator function
#   treshold2 <- unlist(treshold2)
# }
# summary(treshold2);boxplot(treshold2)
# 
# #write.csv(treshold2,"Spawner_Abundance_treshold.csv")



#Load saved object from the tresholds
treshold2 <- read.csv("Spawner_Abundance_treshold.csv") 
treshold2[,1]<-NULL



## Apply treshold value to each time-step
concTransform3 <- function(x) 
{
  i <- order(x)
  ys <- sort(x)
  p <- ys/max(ys)
  x[i] <- p
  x
  #p
}

# Just to check
for(i in 1:ncol(dfstack)){
  image(gr, concTransform3(dfstack[,i]) >= treshold2[i,])
  title(colnames(dfstack)[i])
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Step 3) Evaluate the persistency of these hotspots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To evaluate the persistency of the previously identified hotspots,
# we will adopt the same approach as in Colloca et al. (2009) (actually it is from Fiorentino et al, 2003; see references therein)
# They identify persistent areas as follows:

# For each time-step, identify the spatial grid ID that was a hotpost.
# This produces a binary variable: 1 where grid ID is a hotpost, 0 for gird ID that is NOT a hotspot


persistency <- NULL
for(i in 1:ncol(dfstack)){
  p <- concTransform3(dfstack[,i]) >= treshold2[i,]
  persistency <- cbind(persistency, p)
}

colnames(persistency) <- colnames(dfstack)

persistency2 <- as.data.frame(ifelse(persistency=="TRUE",1,0)) #Set TURE/FALSE to 1/0 format
table(persistency[,1]); table(persistency2[,1]) #Doubble check that everything is ok


head(persistency2)

persistency2$sum <- rowSums(persistency2)
persistency2$Index <- persistency2$sum/45
persistency2$gr_lon <- gr$lon
persistency2$gr_lat <- gr$lat




setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Shapefile/ICES_areas/")
EB <- readOGR(".","25_26")

image(gr, persistency2$Index >= 0.8) #I need to decide what value I use here as a treshold
plot(gr,add=T)
plot(EB,add=T,lwd=2)



# persistency3 <- persistency2
# # promote to spatial points data frame and define CRS of points
# coordinates(persistency3) = ~gr_lon + gr_lat
# 
# crs(persistency3) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" 

box1 <- drawPoly() #draws polygon in the Kattegat - However, this is already a closure area. so leave it out
box2 <- drawPoly() #draws polygon in the WBS, close to Bornholm



poly_box1 <- as(box1, "SpatialPolygonsDataFrame" )
poly_box2 <- as(box2, "SpatialPolygonsDataFrame" )





## Save polygons
## Save polygons
setwd("D:/PhD/Project III/Results/Spawning_HotSpots/YearMonth")
writeOGR(poly_box2, dsn = '.', layer = 'Spawner_box2', driver = "ESRI Shapefile")




# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Data/Shapefile/")
# DK <- readOGR(".", "DK") #Importing contracted DK shapefile with width=-0.010
# proj4string(DK) <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

# setwd("D:/PhD/Project III/Results/Spawning_HotSpots/YearMonth")
# 
# 
# data("worldHiresMapEnv")
# DK_coast_poly <- map("worldHires",  fill=TRUE, col="transparent",
#                      plot=FALSE, xlim=c(9.5,15), ylim=c(53.7,56))
# DK_coast_poly$names
# IDs <- sapply(strsplit(DK_coast_poly$names, ":"), function(x) x[1])
# DK_poly <- map2SpatialPolygons(DK_coast_poly, IDs=IDs,
#                                proj4string=CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"))
# 
# 
# 
# plot(DK_poly,col=1, xlim=c(9.5,15),ylim=c(53.7,56))
# plot(poly_df1,col=2,add=T)
# plot(poly_df2,col="blue",add=T)










################################################################################################## 


# Extra curious stuff for plotting

# Plot the CRDF for all time-steps
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(Xaxis[,1],Yaxis[,1],type="s",lwd=2)
for(i in 1:ncol(Xaxis)){ #either Xaxis or Yaxis, never mind
  lines(Xaxis[,i],Yaxis[,i],lwd=2)
}


# In ggplot
Xaxis_long <- tidyr::gather(as.data.frame(Xaxis), TimeStep, measurement, V1:V45, factor_key=TRUE)
Yaxis_long <- tidyr::gather(as.data.frame(Yaxis), TimeStep, measurement, V1:V45, factor_key=TRUE)

df_long <- data.frame(TimeStep=Xaxis_long$TimeStep, Xmeasure = Xaxis_long$measurement,
                      Ymeasure = Yaxis_long$measurement)




Y2005 <- c(paste("V",1:3,sep="")) 
Y2006 <- c(paste("V",4:6,sep="")) 
Y2007 <- c(paste("V",7:9,sep="")) 
Y2008 <- c(paste("V",10:12,sep="")) 
Y2009 <- c(paste("V",13:15,sep="")) 
Y2010 <- c(paste("V",16:18,sep="")) 
Y2011 <- c(paste("V",19:21,sep="")) 
Y2012 <- c(paste("V",22:24,sep="")) 
Y2013 <- c(paste("V",25:27,sep="")) 
Y2014 <- c(paste("V",28:30,sep=""))  
Y2015 <- c(paste("V",31:33,sep=""))  
Y2016 <- c(paste("V",34:36,sep=""))  
Y2017 <- c(paste("V",37:39,sep=""))  
Y2018 <- c(paste("V",40:42,sep=""))  
Y2019 <- c(paste("V",43:45,sep=""))  



df_long$Year <- ifelse(df_long$TimeStep %in% Y2005,"2005",
                       ifelse(df_long$TimeStep %in% Y2006,"2006",
                              ifelse(df_long$TimeStep %in% Y2007,"2007", 
                                     ifelse(df_long$TimeStep %in% Y2008,"2008", 
                                            ifelse(df_long$TimeStep %in% Y2009,"2009", 
                                                   ifelse(df_long$TimeStep %in% Y2010,"2010", 
                                                          ifelse(df_long$TimeStep %in% Y2011,"2011", 
                                                                 ifelse(df_long$TimeStep %in% Y2012,"2012", 
                                                                        ifelse(df_long$TimeStep %in% Y2013,"2013", 
                                                                               ifelse(df_long$TimeStep %in% Y2014,"2014", 
                                                                                      ifelse(df_long$TimeStep %in% Y2015,"2015", 
                                                                                             ifelse(df_long$TimeStep %in% Y2016,"2016",
                                                                                                    ifelse(df_long$TimeStep %in% Y2017,"2017",
                                                                                                           ifelse(df_long$TimeStep %in% Y2018,"2018",
                                                                                                                  "2019"))))))))))))))







# ggplot(data=df_long, aes(x=Xmeasure, y=Ymeasure, col=TimeStep)) +
#   geom_line(size=1) +
#   theme_bw()+
#   scale_colour_grey(end = 0) +
#   labs(x = "Relative density", y="Cumulative frequency") +
#   theme(panel.border = element_blank(),
#         axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
#         axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
#         
#         plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
#         
#         axis.text.x = element_text(face="bold",size=16),
#         axis.text.y = element_text(face="bold",size=16),
#         axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=18,face="bold"),
#         axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=18,face="bold"),
#         #axis.line = element_line(size=1, colour = "black"),
#         
#         legend.position = "none",
#         
#         plot.margin = unit(c(1,1,1,1),"cm"))

#setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures")
#ggsave("CDRF_spawners.png",dpi=450, width = 15, height = 12, units = "cm")




fish_palettes()


#fishualize(n = 15, option = "Hypsypops_rubicundus", end = 0.9)
fishualize(n = 15, option = "Acanthurus_leucosternon", end = 0.9)
#fishualize(n = 15, option = "Coryphaena_hippurus", end = 0.9)
fishualize(n = 15, option = "Epinephelus_lanceolatus", end = 0.9)
#fishualize(n = 15, option = "Etheostoma_spectabile", end = 0.9)
#fishualize(n = 15, option = "Lepomis_megalotis", end = 0.9)
fishualize(n = 15, option = "Oxymonacanthus_longirostris", end = 0.9)
fishualize(n = 15, option = "Thalassoma_bifasciatum", end = 0.9)


pal <- fish(15, option = "Epinephelus_lanceolatus",end=0.8)

#ggplot(data=df_long, aes(x=Xmeasure, y=Ymeasure, col=TimeStep)) +
#ggplot(data=df_long, aes(x=Xmeasure, y=Ymeasure, col=TimeStep)) +
p1 <- ggplot(data=df_long, aes(x=Xmeasure, y=Ymeasure, col=Year)) +
  geom_line(size=1) +
  theme_bw()+
  scale_colour_manual(values=rev(pal)) +
  labs(x = "Relative density", y="Cumulative frequency") +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        
        plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
        
        axis.text.x = element_text(face="bold",size=16),
        axis.text.y = element_text(face="bold",size=16),
        axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=18,face="bold"),
        axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=18,face="bold"),
        #axis.line = element_line(size=1, colour = "black"),
        
        legend.position = "right",
        legend.title = element_text(size = 16,face="bold"),
        legend.text = element_text(size = 15),
        legend.key.height  = unit(0.5,"cm"),
        
        
        plot.margin = unit(c(1,1,1,1),"cm"))



# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures")
# ggsave("CRDF_recruits.png",dpi=450, width = 15, height = 12, units = "cm")








# Boxplot of the selected treshold value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
boxplot(treshold2)




p2 <- ggplot(treshold2, aes(x= "", y=x)) +
  geom_boxplot(notch=T,color="black", fill="gray70", alpha=0.3,lwd=1) +
  # geom_violin(trim=F,color="black", fill="gray70", alpha=0.3,lwd=1) +
  # stat_summary(fun.data=mean_sdl, size=1,
  #              geom="pointrange", color="red")+
  theme_bw() +
  #geom_jitter(width=0.1,alpha=0.2) +
  labs(y="CRDF tresholds",x="") +
  scale_fill_manual(values=c("#999999")) +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        
        plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
        
        axis.text.x = element_text(face="bold",size=16),
        axis.text.y = element_text(face="bold",size=16),
        axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=18,face="bold"),
        axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=18,face="bold"),
        #axis.line = element_line(size=1, colour = "black"),
        
        legend.position = "right",
        legend.title = element_text(size = 16,face="bold"),
        legend.text = element_text(size = 15),
        legend.key.height  = unit(0.5,"cm"),
        
        
        plot.margin = unit(c(1,1,1,1),"cm"))




# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures")
# ggsave("CRDF_treshold_recruits.png",dpi=450, width = 13, height = 10, units = "cm")





# Plot the density distribution and show the selected area based on the treshold value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)


dens <- density(Xaxis[,22]) #Select an arbitray example
trs <- treshold2[22,] #Select number according to the above one


data <- tibble(x = dens$x, y = dens$y) %>% 
  mutate(variable = case_when(
    (x >= trs & x <= 1) ~ "On", 
    (x >= 0 & x <= trs) ~ "Off",
    TRUE ~ NA_character_))
#> Warning: package 'bindrcpp' was built under R version 3.4.4

p3 <- ggplot(data, aes(x, y)) + geom_line() +
  geom_area(data = filter(data, variable == 'On'), fill = pal[3],alpha=0.3) + 
  #geom_area(data = filter(data, variable == 'Off'), fill = 'light blue') +
  geom_line(size=1) +
  theme_pubclean() +
  #labs(x = "Abundance", y="Density", title="October - 2019") +
  labs(x = "Abundance", y="Density") +
  scale_x_continuous(limits=c(min(dens$x),max(dens$x)),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme(panel.border = element_blank(),
        axis.line.x = element_line(size = 1, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 1, linetype = "solid", colour = "black"),
        
        plot.title = element_text(hjust = 0.5, margin=margin(b=15),size=18,face="bold"),
        
        axis.text.x = element_text(face="bold",size=16),
        axis.text.y = element_text(face="bold",size=16),
        axis.title.y = element_text(margin=margin(t=0,r=20,b=0,l=0),size=18,face="bold"),
        axis.title.x = element_text(margin=margin(t=20,r=0,b=0,l=0),size=18,face="bold"),
        #axis.line = element_line(size=1, colour = "black"),
        
        legend.position = "right",
        legend.title = element_text(size = 16,face="bold"),
        legend.text = element_text(size = 15),
        legend.key.height  = unit(0.5,"cm"),
        
        
        plot.margin = unit(c(1,1,1,1),"cm"))



# setwd("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures")
# ggsave("Frequency_treshold_recruits.png",dpi=450, width = 15, height = 12, units = "cm")




#### All plot in one row


finalplot <- grid.arrange(p1,p2,p3,                 
                          ncol = 3, nrow =1)

ggsave("C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Spawners_CRDFs.png",
       finalplot, width = 45, height = 15, units = "cm")
