###################################################################################
#                                                                                 #
#                    Coupling spatio-temporal model to DISPLACE                   #
#                             Part 2: Coupling results                            #
#                                                                                 #
###################################################################################


library(TMB)
library(gridConstruct)
library(Matrix)
library(fields)
library(raster)
library(tidyr)
library(pals)
library(maptools)
library(ggplot2)
library(maptools)

#library(gridConstruct)
library(ggsn)
library(mapdata)
library(fields)
library(rgdal)

# SECTION 1: Predict abundance field one time-step ahead (core section for DISPLACE coupling)
# SECTION 2: Exract abundances for DISPLACE grid
# SECTION 3: Output for DISPLACE



# Multivariate normal distribution simulation function (based on precision rather variance)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# @ mu = mean of the abundance field
# @ prec = precision of the spatio-temporal covariance matrix
# @ n.sims = number of simulations
rmvnorm_prec <- function(mu, prec, n.sims) {
  z <- matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
  L <- Cholesky(prec, super=TRUE)
  z <- solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
  z <- solve(L, z, system = "Pt") ## z = Pt    %*% z
  z <- as.matrix(z)
  mu + z
}





#><><><><><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><>
########################                SECTION 1                ########################
#><><><><><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><>


# Load results
#~~~~~~~~~~~~~~~~
load("D:/PhD/Project III/Results/Size_Distribution_DISPLACE_input/WBScod_all_SizeGroups.RData") #fullres is a list of lists, where each individual list stores the results of a particular size-group
# names(fullres) #names of the lists(related to the DISPLACE size-groups)
# names(fullres[[1]]) #names of the objects present in each list
# image(fullres[[1]]$gr,fullres[[1]]$abundance[,1]) # To see an example of the estimated abundance fields



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.1) Retrieve spatio-temporal parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Get abundances for the last time-step (2019, with all 4 quarters)
abundances <- vector("list", length(fullres))
Q1 <- ncol(fullres[[1]]$abundance)-4+1
Q4 <- ncol(fullres[[1]]$abundance)
for(i in seq_along(fullres)){
  abundances[[i]] <- fullres[[i]]$abundance[,Q1:Q4]
}
names(abundances) <- names(fullres)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.2) Predict one-time step ahead
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To predict the abundance fields in t+1, we need to "draw" a new abundance field that is based
# on the spatio-temporal correlation parameters + precision matrix that were estimated for each stock-sizeGroup
# (retrieved in the coupling script part 1 (LGNB_DISPLACE_coupling_part01.R)).

# As such, we need to apply the rmvnorm_prec function, which basically simulates
# a multivariate Gaussian random distribution based on the input spatio-temporal corr. parameters and precision matrix.
# The function requires that the user specifies the number of simulations; usually,the more precise the abundance
# estimates, the more precise will be the forward prediction (thus, only a few simulations, or even only 1, would be required).
# However, when abundance estimates have high uncertainties (such as those of very small size-groups), the forward-predicted
# abundance fields tend to be very different from one simulation to another, and therefore a higher number of simulation
# is required to stabilize the predicted abundance field. Provided that it is a "half-simulation" framework,
# we can compute forward-predictions for all four quarters of the last time-period. 


Allspred <- vector("list", length(fullres))


NROW <- dim(fullres[[1]]$abundance)[1]
NCOL <- ncol(abundances[[1]])
DIM <- length(fullres)
tmp <- array(1:(NCOL*NROW), c(NROW,NCOL,DIM)) 

NSIM <- 2 #Choose the number of simulations and take the mean across the simulated fields



for(i in seq_along(abundances)){
  for(j in 1:NCOL){
    Abundance <- abundances[[i]][,j]
    #tmp[,j,i] <- rmvnorm_prec(mu = Abundance, prec = fullres[[i]]$scale*sqrt(1-fullres[[i]]$phi^2)*fullres[[i]]$Q, n.sims = 1) #Simulate and store results in the array
    tmpres <- as.data.frame(rmvnorm_prec(mu = Abundance, prec = fullres[[i]]$scale*sqrt(1-fullres[[i]]$phi^2)*fullres[[i]]$Q, n.sims = NSIM)) #Simulate and store results in the array
    tmpres <- rowMeans(tmpres[,1:NSIM])
    tmp[,j,i]  <- tmpres
    Allspred <- lapply(seq(dim(tmp)[3]), function(x) tmp[ , , x]) # Convert array back to list (easier to maniupulate)
  }
}


# Set abundances back to natural scale
# for(i in seq_along(Allspred)){
#   Allspred[[i]] <- apply(Allspred[[i]],2,exp)
# }


names(Allspred) <- names(fullres)

# load("D:/PhD/Project III/Results/Size_Distribution_DISPLACE_input/Crude_results_per_SizeGroup/results_WBScod_size_m1_S2_both_No_One.RData") 
# image(fullres[[5]]$gr, Allspred[[5]][,1],col=tim.colors(99)) #plotting example





######################                END OF SECTION 1            #######################





#><><><><><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><>
########################                SECTION 2                ########################
#><><><><><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><><><>><>



# First we need to create individual raster files, as the abundance output given in 
# SECTION 1 isn't a raster file. After that we use the lon/lat of the DISPLACE graphs 
# and extract the abundances according to each graph node


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.1) Create an empty rasterfile 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gr <- fullres[[1]]$gr # Get the grid from an arbitrary size-class (gr is the same throughout all size-classes)
e <- extent(as.matrix(gr))
r <- raster(e,ncol=80,nrow=80) #Adapt raster size according to grid size; These numbers are good for 5x5km grid (as model output)


# Include grid positions to dataframes
coords <- data.frame(lon=gr$lon,lat=gr$lat) #Retrieve grid positions from model output


Allspred <- lapply(Allspred, cbind, coords) #Bind grid positions

abundances <- lapply(abundances, cbind, coords) #Bind grid positions


for(i in seq_along(abundances)){
  colnames(abundances[[i]]) <- c("1","2","3","4","lon","lat")
}



# Convert df from wide to long format
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## For the abundance fields in time t+1
Allspredw <- list(NULL)
for(i in seq_along(Allspred)){
  Allspredw[[i]] <- gather(Allspred[[i]], key=Quarter,value=Abundance,-c(lon,lat))
}
names(Allspredw) <- names(Allspred) #name new list accordingly



## For the abundance fields in time t
abundancesw <- list(NULL)
for(i in seq_along(abundances)){
  abundancesw[[i]] <- gather(abundances[[i]], key=Quarter,value=Abundance,-c(lon,lat))
}
names(abundancesw) <- names(abundances) #name new list accordingly





# Include size-specific column to each df
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## For the abundance fields in time t+1
for( i in seq_along(Allspredw)){
  Allspredw[[i]]$Size <- rep(names(Allspredw)[i],nrow(Allspredw[[i]]))
}

Allspredw <- do.call("rbind", Allspredw) #Unlist
rownames(Allspredw) <- NULL


## For the abundance fields in time t
for( i in seq_along(abundancesw)){
  abundancesw[[i]]$Size <- rep(names(abundancesw)[i],nrow(abundancesw[[i]]))
}

abundancesw <- do.call("rbind", abundancesw) #Unlist
rownames(abundancesw) <- NULL




# Identify properly all quarters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## For the abundance fields in time t+1
Allspredw$Quarter <- ifelse(Allspredw$Quarter=="1","Q1",
                            ifelse(Allspredw$Quarter=="2","Q2",
                                   ifelse(Allspredw$Quarter=="3","Q3","Q4")))




## For the abundance fields in time t
abundancesw$Quarter <- ifelse(abundancesw$Quarter=="1","Q1",
                            ifelse(abundancesw$Quarter=="2","Q2",
                                   ifelse(abundancesw$Quarter=="3","Q3","Q4")))






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.2) Create raster based on abundances
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## For the abundance fields in time t+1
dflist_pred <- split(Allspredw,list(Allspredw$Quarter,Allspredw$Size))

abufields_pred <- list() 
for(i in 1:length(dflist_pred)){
  abufields_pred[[i]] <- rasterize(dflist_pred[[i]][,c("lon","lat")], r, dflist_pred[[i]][,"Abundance"], fun=mean)
  abufields_pred[[i]] <- disaggregate(abufields_pred[[i]],2,method="bilinear")
}
names(abufields_pred) <- names(dflist_pred) #name new list accordingly

#image(abufields_pred[[47]],col=tim.colors(99)) #out of 52; 47 is SG13-Q4






## For the abundance fields in time t
dflist_current <- split(abundancesw,list(abundancesw$Quarter,abundancesw$Size))

abufields_current <- list() 
for(i in 1:length(dflist_current)){
  abufields_current[[i]] <- rasterize(dflist_current[[i]][,c("lon","lat")], r, dflist_current[[i]][,"Abundance"], fun=mean)
  abufields_current[[i]] <- disaggregate(abufields_current[[i]],2,method="bilinear")
}
names(abufields_current) <- names(abufields_current) #name new list accordingly

image(abufields_current[[47]],col=tim.colors(99)) #out of 52; 47 is SG13-Q4








# Create shapefile for study area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data("worldHiresMapEnv")
DK_coast_poly <- map("worldHires",  fill=TRUE, col="transparent",
                     plot=FALSE, xlim=c(9,15.5), ylim=c(54.5,58))
DK_coast_poly$names
IDs <- sapply(strsplit(DK_coast_poly$names, ":"), function(x) x[1])
DK_poly <- maptools::map2SpatialPolygons(DK_coast_poly, IDs=IDs,
                                         proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))




par(mfrow=c(1,2))
image(abufields_current[[23]],col=tim.colors(15)) #out of 52; 47 is SG13-Q4
plot(DK_poly,add=T, col="gray70")

image(abufields_pred[[23]],col=tim.colors(15)) #out of 52; 47 is SG13-Q4
plot(DK_poly,add=T, col="gray70")





png(file="C:/Users/mruf/OneDrive - Danmarks Tekniske Universitet/PhD/Manuscript_03/Figures/Supplementary_material/Coupling_abundance_maps/predicted_current_abundances.png",
    width=3300, height=4600, res=300)


par(mfrow=c(3,2),mar=c(2,3,3,2))


image(abufields_current[[23]],col=tim.colors(15),las=1,xlab="",
      ylab="",axes=F,main="SG6 - 2019:Q4",cex.main=2.5,font.main=2) #titles[23]
plot(DK_poly,add=T, col="gray70")
box(lwd=2)

image(abufields_pred[[23]],col=tim.colors(15),las=1,xlab=""
      ,ylab="",axes=F,main="SG6 - 2020:Q1",cex.main=2.5,font.main=2) #
plot(DK_poly,add=T, col="gray70")
box(lwd=2)




image(abufields_current[[34]],col=tim.colors(15),las=1,xlab="",
      ylab="",axes=F,main="SG9 - 2019:Q4",cex.main=2.5,font.main=2) #titles[34]
plot(DK_poly,add=T, col="gray70")
box(lwd=2)

image(abufields_pred[[34]],col=tim.colors(15),las=1,xlab=""
      ,ylab="",axes=F,main="SG9 - 2020:Q1",cex.main=2.5,font.main=2) #
plot(DK_poly,add=T, col="gray70")
box(lwd=2)




image(abufields_current[[48]],col=tim.colors(15),las=1,xlab="",
      ylab="",axes=F,main="SG12 - 2019:Q4",cex.main=2.5,font.main=2) #titles[48]
plot(DK_poly,add=T, col="gray70")
box(lwd=2)

image(abufields_pred[[48]],col=tim.colors(15),las=1,xlab=""
      ,ylab="",axes=F,main="SG12 - 2020:Q1",cex.main=2.5,font.main=2) #
plot(DK_poly,add=T, col="gray70")
box(lwd=2)


dev.off()
