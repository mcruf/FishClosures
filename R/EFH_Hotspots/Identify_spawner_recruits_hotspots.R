############################################################################################
#                                                                                          #
##                  Identify abundance hotspots of Essential Fish habitats                ##
##                                    (Rufener et al.)                                    ##
#                                                                                          #
############################################################################################

# Last update: February 2023

# Code written and mantained by Marie-Christine Rufener
# Contact < macrufener@gmail.com > for any query or to report code issues.


# In the first script (LGNB_SpawnerRecruits.R) script we have run the LGNB-SDM 
# for WB cod sepparately for each age group (age 0 - age5+).
# The model was run on a monthly basis from 2005-2019, which means that we get
# predicted abundance maps for each month of the time series.

# We will take these results to identify the persistency of the following abudnance hotspots:
# - Recruits (A0-A1)
# - Spawners (A2-A5+)
# - Old-spawners (A5+) 
# - Feeding grounds 

# The present script is organized as follows:

# 1) Set default inputs & R libraries
# 2) Load data files
# 3) Evaluate abundance hotspot persistency



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 1: Set default inputs & R libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1.1) Choose life stage for which the abundance hotspot should be identified; 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Juveniles: A0-A2
## Spawners: A3-A5+


Lstage <- c("Juveniles","Spawners")[1] # Default = Juveniles



# 1.2) Load R libraries
#~~~~~~~~~~~~~~~~~~~~~~~~
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
library(tidyverse)
library(gridExtra)




#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 2: Load data files 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Note: the LGNB-SDM output provides a dataframe where each column is a time-period.
# Since we ran the model from 2005-2019 on a monthly basis, this means that we will
# have 180 columns (V1-V180).

# To identify juvenile hotpspots, we will have to take all the months into consideration, in opposit to
# the spawner hotspot, as for the latter we know upfront the monhts corresponding to spawning (JAN-MAR).
# Whereas for the recruits we will stack the abundance layers from all months of the time series, for 
# spawners we will only stack the abundance layers corresponding to the first three months of each year.



if(Lstage == "Juveniles"){
  
  ## Set WD where the recruits results are stored
  setwd("~/Data/Hotspots/Recruits/")
  #setwd("C:/Users/mruf/Documents/Fish_Closures/Data/Hotspots/Recruits/")
  
  
  
  ## A0
  load("A0.RData")
  rm(list=setdiff(ls(), ls(pattern=c("Lstage|A0|A1|A2|A3|A4|A5|gr|spawn_month"))))
  
  
  ## A1
  load("A1.RData")
  rm(list=setdiff(ls(), ls(pattern=c("Lstage|A0|A1|A2|A3|A4|A5|gr|spawn_month"))))
  
  
  ## A2
  load("A2.RData")
  rm(list=setdiff(ls(), ls(pattern=c("Lstage|A0|A1|A2|A3|A4|A5|gr|spawn_month"))))
  
  
  ## Move all results into a list
  abulist <- list(A0,A1,A2); names(abulist) <- paste("Age",0:2,sep="")
  
  
  
  
} else if(Lstage == "Spawners"){
  
  
  ## Set WD where the recruits results are stored
  setwd("~/Data/Hotspots/Spawners/")
  #setwd("C:/Users/mruf/Documents/Fish_Closures/Data/Hotspots/Spawners/")
  
  
  
  
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
  
  
  ## Move all results into a list
  abulist <- list(A3,A4,A5); names(abulist) <- paste("Age",3:5,sep="")
  
  
  
}


# 2.1) Quick 'n dirty plotting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Just to have a quick look into the predicted
# abundance fields


## 2.1.1) Retrieve base map of Denmark
data("worldHiresMapEnv")
DK_coast_poly <- map("worldHires",  fill=TRUE, col="transparent",
                     plot=FALSE, xlim=c(9,15.5), ylim=c(54.5,58))
DK_coast_poly$names
IDs <- sapply(strsplit(DK_coast_poly$names, ":"), function(x) x[1])
DK_poly <- map2SpatialPolygons(DK_coast_poly, IDs=IDs,
                               proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))


## 2.1.2) Recreate monthly time stamps from time series
tstep <- seq(as.Date("2005-01-01"), as.Date("2019-12-31"), by = "1 month")
YearMonth <- as.factor(format(as.Date(tstep), "%Y-%m"))


## 2.1.3) Go for the plot (just take an arbitrary age group and time period)
plot(gr,type = "n",xlab="Longitude",ylab="Latitude",las=1,xlim=c(9.25,15.4))
image(gr, concTransform(abulist[[1]][,15]), #abulist[[1]] = Age0, time step = 15
      col = tim.colors(99),
      add=TRUE)
plot(DK_poly,col=1,add=T)
title(main = levels(YearMonth)[15])
mysubtitle = names(abulist)[1]


## To produce gif to see the spatio-temporal abundance dynamics
# saveGIF({
#   for(age in 1:length(abulist)){
#     for (time in 1:ncol(abulist[[age]])){
#       
#     plot(gr,type = "n",xlab="Longitude",ylab="Latitude",las=1,xlim=c(9.25,15.4))
#     image(gr, concTransform(abulist[[age]][,time]),
#           col = tim.colors(99),
#           #map=quote(map("worldHires",add=TRUE, fill=TRUE, col="grey70")),
#           #map=quote(plot(DK_poly,add=T,col="grey70")),
#           add=TRUE)
#     plot(DK_poly,col=1,add=T)
#     title(main = levels(YearMonth)[time])
#     mysubtitle = names(abulist)[age]
#     mtext(side = 3, line = 0.25, mysubtitle,)    
#       }
#    }
# },
#   
# for(age in 1:length(abulist)){
#   movie.name = paste(names(abulist)[age], ".gif", sep="")
#   },
# 
#   interval = 0.2,
#   ani.width = 600)



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 3: Evaluate abundance hotspot persistency
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Step 1) Stack the YearMonth abundance layers of the individual age groups. 
## For each YearMonth map, we then sum over all abundance values across the spatial
## grid IDs. This provides a "total juvenile abundance" layer for each YearMonth,
## and is essentially analogous to the predict first, assemble later approach (Ferrier and Guisan, 2006)

### Step 2) Take the generated YearMonth maps and evaluate the abundance 
## hotspots following Bartolino's et al. (2011) approach - A frequency 
## distribution approach to hotspot identification

### Step 3) Evaluate the presistency of the identified hotpost along 
## the considered time period by using Colloca's et al. (2009) approach - 
## Identifying fish nurseries using density and presistence areas




# 3.1) Stack YearMonth abundance layers and sum over the values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 3.2.1) Transform the abundance to natural scale
## (The LGNB-SDM model provides output on a log-scale)
abulist_exp <- lapply(abulist, exp)


## 3.2.2) Stack abundance layers
dfstack <- NULL

for(age in seq_along(YearMonth)){
  p <- rowSums(sapply(abulist_exp, `[[`, age), na.rm = TRUE)
  dfstack <- cbind(dfstack, p)
}

colnames(dfstack) <- colnames(abulist_exp[[1]]) #take an arbitray number among the list

## To see the progress...
# for(i in 1:ncol(dfstack)){
# image(gr, dfstack[,i],col=tim.colors(99))
# }




# 3.2) Identify abundance hotspots 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For each time step in each age group

# Following Bartolino's approach: the cumulative relative frequency distribution (CRDF)
# A CRDF curve is approximated by plotting the relative value of a variable z 
# (e.g., density), against the frequency distrituion of the same variable.
# The two axes of the plot are given by:

# x=z/zm  ; where z is the variable of interest (here abundance density) and
# zm is the maximum value that z assumes in the study area.

# y=M(x)/N  ; where M(x) is the number of cases (i.e, points, pixels, areas) a value smaller than x is encountered,
# and N is the total number of cases



# We have to define two different functions, one describing the x-axis
# and the other the y-axis of the CRDF curve.
# For the x-axis, we basically have to conduct three steps:
# 1) Order the abundance values of each month from smallest to largest
# 2)Sort these values from smallest to largest.
# 3) Apply Bartolino's formula, i.e., we take the abundance value of a given 
# grid cell of the spatial grid and divide it by the max abundance.



## 3.2.1) Function to define the X-axis of Bartolino's method
fxax <- function(x){
  i  <- order(x)
  ys <- sort(x)
  p  <- ys/max(ys)
  p
}



### 3.2.2) Function to define the Y-axis of Bartolino's method
fyax <- function(x) {
  s <- 1:length(x)
  l <- length(x)
  z <- s/l
  z
}



### 3.2.3) Define the X and Y axes

Xaxis <- apply(dfstack, 2, fxax) #X-axis
Yaxis <- apply(dfstack, 2, fyax) #Y-axis


### Check the progress...

## Plot the CRDF curves individually
par(mfrow=c(2,2))
for(i in 1:ncol(Xaxis)){
  plot(Xaxis[,i],Yaxis[,i],type="s",main=colnames(dfstack)[i])
}


## Plot the CRDF curves of all time steps
par(mfrow=c(1,1))
plot(Xaxis[,1],Yaxis[,1],type="s",lwd=2, main=Lstage)
for(i in 1:ncol(Xaxis)){ #either Xaxis or Yaxis, never mind
  lines(Xaxis[,i],Yaxis[,i],lwd=2)
}



### 3.2.4) Define the abundance treshold

## Here we still apply Bartolino's method, which defines the hotspot treshold as the value
## delimited by a 45degree tangent in the CRDF curve from the plots above. 
## Values below the 45o tangent are not considered as an abundance hotspot.

treshold <- list()

for(i in 1:ncol(Xaxis)){ #either Xaxis or Yaxis, never mind
  plot(Xaxis[,i],Yaxis[,i],type="s",lwd=2, main=colnames(dfstack)[i])
  abline(0,1,col=3,lwd=2)
  
  #treshold[[i]] <- locator() #Manual way of defining threshold
  
  ## Automatized way to find the tangent (written by Francois Bastardie for NORDFO project)
  idx_max_b <-  which.max(Yaxis[,i] - Xaxis[,i])  # b=y-x knowing a 45 degree line has slope=1 and line equation is then y=x+b. Therefore, max of b gives the param of the line having the highest intersecting (tangent) point to the cumulated obs curve
  thres_x   <- Xaxis[idx_max_b, i]
  thres_y   <- Yaxis[idx_max_b, i]
  lines(Xaxis[,i], Xaxis[,i]+ (Yaxis[idx_max_b,i] - Xaxis[idx_max_b, i]), col=3)
  treshold[[i]] <- list(x=thres_x, y=thres_y)
  
}


## Keep only the x-values of the treshod
tx <- NULL
for(i in seq_along(treshold)){
  tx[i] <- treshold[[i]][1] #1 stands for the "x" element; 2 would be the "y" element of the locator function
  tx <- unlist(tx)
}

#write.csv(tx,paste(Lstage, "_Abundance_treshold",".csv", sep=""))



### 3.2.5) Apply the abundance treshold for each time step
ConcTransform <- function(x) # Based on Bartolino et al. (2011)
{
  i <- order(x)
  ys <- sort(x)
  p <- ys/max(ys)
  x[i] <- p
  x
  #p
}


# Just to check
collect_gravity_points <- NULL

for(i in 1:ncol(dfstack)){
  image(gr, ConcTransform(dfstack[,i]) >= tx[i])
  title(colnames(dfstack)[i])
  
  
  # line below taken from Francois Bastardie code (NORDFO project)
  gravity_point <- apply(gr[dfstack[,i] >= tx[i],], 2, mean)
  points(gravity_point, pch="*", cex=5, col="black")
  
}




# 3.3) Identify the persistency of the abundance hotspots 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To evaluate the persistency of the previously identified hotspots,
# we will adopt the same approach as in Colloca et al. (2009) (actually it is from Fiorentino et al, 2003; see references therein)
# They identify persistent areas as follows:

# For each time-step, identify the spatial grid ID that was a hotpost.
# This produces a binary variable: 1 where grid ID is a hotpost, 0 for gird ID that is NOT a hotspot


threshold_for_persistency <- 0.8 #Conservative treshold based on STECF recommendations (0.75)



### 3.3.1) Identify persistent hotspots
persistency <- NULL

for(i in 1:ncol(dfstack)){
  p <- ConcTransform(dfstack[,i]) >= tx[i]
  persistency <- cbind(persistency, p)
}

colnames(persistency) <- colnames(dfstack)

persistency2 <- as.data.frame(ifelse(persistency=="TRUE",1,0)) #Set TURE/FALSE to 1/0 format
table(persistency[,1]); table(persistency2[,1]) #Doubble check that everything is ok


head(persistency2)

### 3.3.2) Add relevant columns
persistency2$sum <- rowSums(persistency2)
persistency2$Index <- persistency2$sum/45
persistency2$gr_lon <- gr$lon
persistency2$gr_lat <- gr$lat


### 3.3.3) Go for the plot
image(gr, persistency2$Index >= threshold_for_persistency) #I need to decide what value I use here as a treshold
plot(gr,add=T)
plot(DK_poly, col=1, add=T)



### 3.3.4) Make polygon of the persistent hotpot areas
# FIXME: Automatize steps below; for now they are done manually based on
# the plot above.

# NOTE: Only run the line below when running the script for the first time.
# The polygons are already stored in the ~/Data/Hotspot/ folder


# if(Lstage == "Recruits"){
#   
#   box1 <- drawPoly() #draws polygon in the Kattegat
#   box2 <- drawPoly() #draws polygon in the upper part of Bornholm
#   box3 <- drawPoly() #draws polygon in the lower part of Bornholm
#   
#   poly_box1 <- as(box1, "SpatialPolygonsDataFrame" )
#   poly_box2 <- as(box2, "SpatialPolygonsDataFrame" )
#   poly_box3 <- as(box3, "SpatialPolygonsDataFrame" )
#   
#   setwd("~/Data/Hotspots/Recruits")
#   writeOGR(poly_box1, dsn = '.', layer = 'Recruits_box1', driver = "ESRI Shapefile")
#   writeOGR(poly_box2, dsn = '.', layer = 'Recruits_box2', driver = "ESRI Shapefile")
#   writeOGR(poly_box3, dsn = '.', layer = 'Recruits_box3', driver = "ESRI Shapefile")
#   
# 
# } else if(Lstage == "Spawners"){
#   
#   box1 <- drawPoly() #draws polygon in the Kattegat - However, this is already a closure area. so leave it out
#   box2 <- drawPoly() #draws polygon in the WBS, close to Bornholm
#   
#   
#   poly_box1 <- as(box1, "SpatialPolygonsDataFrame" )
#   poly_box2 <- as(box2, "SpatialPolygonsDataFrame" )
#   
#   setwd("~/Data/Hotspots/Spawners")
#   writeOGR(poly_box1, dsn = '.', layer = 'Spawners_box1', driver = "ESRI Shapefile")
#   writeOGR(poly_box2, dsn = '.', layer = 'Spawners_box2', driver = "ESRI Shapefile")
# }


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><



##################################
#                                #
#   Plotting results for paper   #
#                                #
##################################

# Done with ggplot2.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Plot CRDF curves of all time steps in one plot 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Do some data wraggling
if(Lstage == "Recruits"){
  
  Xaxis_long <- tidyr::gather(as.data.frame(Xaxis), TimeStep, measurement, V1:V180, factor_key=TRUE)
  Yaxis_long <- tidyr::gather(as.data.frame(Yaxis), TimeStep, measurement, V1:V180, factor_key=TRUE)
  
  
  Y2005 <- c(paste("V",1:12,sep="")) 
  Y2006 <- c(paste("V",13:24,sep="")) 
  Y2007 <- c(paste("V",25:36,sep="")) 
  Y2008 <- c(paste("V",37:48,sep="")) 
  Y2009 <- c(paste("V",49:60,sep="")) 
  Y2010 <- c(paste("V",61:72,sep="")) 
  Y2011 <- c(paste("V",73:84,sep="")) 
  Y2012 <- c(paste("V",85:96,sep="")) 
  Y2013 <- c(paste("V",97:108,sep="")) 
  Y2014 <- c(paste("V",109:120,sep=""))  
  Y2015 <- c(paste("V",121:132,sep=""))  
  Y2016 <- c(paste("V",133:144,sep=""))  
  Y2017 <- c(paste("V",145:156,sep=""))  
  Y2018 <- c(paste("V",157:168,sep=""))  
  Y2019 <- c(paste("V",169:180,sep=""))  
  
  
  
  
} else if (Lstage == "Spawners"){
  
  Xaxis_long <- tidyr::gather(as.data.frame(Xaxis), TimeStep, measurement, V1:V45, factor_key=TRUE)
  Yaxis_long <- tidyr::gather(as.data.frame(Yaxis), TimeStep, measurement, V1:V45, factor_key=TRUE)
  
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
  
}



df_long <- data.frame(TimeStep=Xaxis_long$TimeStep, Xmeasure = Xaxis_long$measurement,
                      Ymeasure = Yaxis_long$measurement)



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



pal <- fish(15, option = "Epinephelus_lanceolatus",end=0.8) #fish_palettes() to see other palettes



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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Boxplot of the selected treshold values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


dtx <- data.frame(tvalues=tx)

p2 <- ggplot(dtx, aes(x= "", y=tvalues)) +
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





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Plot the density distribution and show the selected area based on the treshold value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dens <- density(Xaxis[,22]) #Select an arbitray example
trs <- tx[22] #Select number according to the above one


data <- tibble(x = dens$x, y = dens$y) %>% 
  mutate(variable = case_when(
    (x >= trs & x <= 1) ~ "On", 
    (x >= 0 & x <= trs) ~ "Off",
    TRUE ~ NA_character_))

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







#### All plot in one row


finalplot <- grid.arrange(p1,p2,p3,                 
                          ncol = 3, nrow =1)


# 
# ggsave("~/Figures/CRDFs.png",
#        finalplot, width = 45, height = 15, units = "cm")
