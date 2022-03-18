##########################################################################################
#                                                                                        #
##                         Coupling LGNB-SDM model to DISPLACE                          ##
##                      Part 2: Format outputs to fit in DISPLACE                       ##
##                                    (Rufener et al.)                                  ##
#                                                                                        #
##########################################################################################


# Last update: March 2022

# Code written and mantained by Marie-Christine Rufener
# Contact < macrufener@gmail.com > for any query or to report code issues.


# In the following script we take the LGNB-SDM outputs that were
# processed in the LGNB-DISPLACE_preProcessing.R and LGNB-DISPLACE_coupling_part01.R scripts
# and set them into the DISPLACE format.


# The script is divided into the following sections:

# SECTION 1: Load model and model results 
# SECTION 2: Keep original abundance fields from model 
# SECTION 3: Predict abundance field one time-step ahead (core section for DISPLACE coupling)
# SECTION 4: Exract abundances for DISPLACE grid
# SECTION 5: Output for DISPLACE

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


# CALLED FROM DISPLACE when dyn_pop_sce.option(Options::nbcpCoupling)
args <- commandArgs(trailingOnly = TRUE)

##args is now a list of character vectors
## First check to see if arguments are passed.
## Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
  print("No arguments supplied to nbcpCoupling.r: Take pop 0 and tstep 0 and sce nbcpcoupling and simu simu1")
  pop     <- 0
  tstep   <- 745
  sce     <- "nbcpcoupling"
  sim     <- "simu1"
  igraph  <- "a_graph56"
}else{
  pop      <- args[1]
  tstep    <- args[2]
  sce      <- args[3]
  sim      <- args[4]
  igraph   <- args[5]
}


igraph <- as.numeric(gsub("a_graph", "", igraph))

application <- "BalticSea"



path <- file.path("C:","Users","mruf","Documents","PHD_projects","Proj_3",paste0("DISPLACE_input_", application))


  

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
  
  
  
  
#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
########################                SECTION 1                ########################
#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


# Load results
#~~~~~~~~~~~~~~~~
load("~/Data/LGNB-DISPLACE/WBScod_all_SizeGroups.RData") #fullres is a list of lists, where each individual list stores the results of a particular size-group
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

# To see the progress....
image(fullres[[5]]$gr, Allspred[[5]][,1],col=tim.colors(99)) 



######################                END OF SECTION 1            #######################
  
  
  
  
  

#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
########################                SECTION 2                ########################
#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><



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

  
# Convert df from wide to long format
Allspredw <- list(NULL)
for(i in seq_along(Allspred)){
    Allspredw[[i]] <- gather(Allspred[[i]], key=Quarter,value=Abundance,-c(lon,lat))
}

names(Allspredw) <- names(Allspred) #name new list accordingly
  
  
# Include size-specific column to each df
for( i in seq_along(Allspredw)){
    Allspredw[[i]]$Size <- rep(names(Allspredw)[i],nrow(Allspredw[[i]]))
}
  
Allspredw <- do.call("rbind", Allspredw) #Unlist
rownames(Allspredw) <- NULL
  
  
# Identify properly all quarters
Allspredw$Quarter <- ifelse(Allspredw$Quarter=="1","Q1",
                              ifelse(Allspredw$Quarter=="2","Q2",
                                     ifelse(Allspredw$Quarter=="3","Q3","Q4")))
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.2) Create raster based on abundances
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dflist <- split(Allspredw,list(Allspredw$Quarter,Allspredw$Size))
  
abufields <- list() 
for(i in 1:length(dflist)){
    abufields[[i]] <- rasterize(dflist[[i]][,c("lon","lat")], r, dflist[[i]][,"Abundance"], fun=mean)
    abufields[[i]] <- disaggregate(abufields[[i]],2,method="bilinear")
}
names(abufields) <- names(dflist) #name new list accordingly
  
#image(abufields[[47]],col=tim.colors(99)) 
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.3) Extract abundances based on DISPLACE graph coords
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
loc <- read.table(file="~/Data/LGNB-DISPLACE/coord40.dat",sep="") # DISPLACE coords
loc <- as.matrix(as.vector(loc))
loc <- matrix(loc, ncol=3)
loc <- cbind(loc, 1:nrow(loc))
colnames(loc) <- c('lon', 'lat', 'harb', 'pt_graph')
  
loc <- as.data.frame(loc)
loc <- loc[loc$harb==0,] # remove points on harbours (all harb>0)
loc2 <- loc[,c("lon","lat")] # keep only lon/lat columns
  
  
dfa <- list()
for(i in seq_along(abufields)){
    dfa[[i]]<- raster::extract(abufields[[i]],loc2)
    dfa[[i]]<- data.frame(abundance=dfa[[i]], lon=loc2$lon, lat=loc2$lat,pt_graph=loc$pt_graph)
}
  
names(dfa) <- names(abufields)
  
  
  
  
  
  
  
######################                END OF SECTION 2            #######################
  
  
  
  
#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><
########################                SECTION 3                ########################
#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><

# Organize predicted abundances into suitable format for DISPLACE
# The output will be a dataframe cotaining the following columns:
# DISPLACE lon $ lat, Quarter
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.1) Include Quarter and Size columns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for(i in seq_along(dfa)){
    dfa[[i]]$Quarter <- factor(substr(names(dfa)[i], start = 1, stop = 2)) # Doubble check if it's correct!!
    dfa[[i]]$Size <- factor(substr(names(dfa)[i], start=4, stop=7)) # Doubble check if it's correct!!
}
  
dfa <- do.call("rbind",dfa) 
rownames(dfa) <- NULL
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.2) Transform to wide format
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
displace_dat <- spread(dfa, Size, abundance)
  
  
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.3) Normalize abundances
#~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Normalize function
#~~~~~~~~~~~~~~~~~~~~
NormAbu <- function(x){
    return(x/sum(x))
    
}
  
  
# Normalize abundance values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Can only be applied to df without the presence of NAs. Thus, we need to remove
# the DISPLACE coordinates where no abundance was associated.
idxna <- which(is.na(displace_dat$SG2)) #Can choose any arbitrary size-group column as the result will be the same
displace_dat <- displace_dat[-idxna,]
  
displace_dat[,c(5:ncol(displace_dat))] <- apply(displace_dat[,c(5:ncol(displace_dat))],2,NormAbu)
  
displace_dat$SG1 <- NA #Create a dummy column for size group 1 (0-5cm) - we didn't have data for it
  
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.4) Save predcited resuls
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Reorder columns
  
displace_dat <- displace_dat[,c("lon","lat","pt_graph","Quarter",
                                "SG1","SG2","SG3","SG4","SG5","SG6",
                                "SG7","SG8","SG9","SG10","SG11","SG12",
                                "SG13","SG14")]
  
  

#save(displace_dat,file="AbuDISPLACE_WBSCod.RData")
  
  