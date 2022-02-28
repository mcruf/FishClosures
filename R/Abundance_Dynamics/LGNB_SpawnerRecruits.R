
##########################################################################################
#                                                                                        #
##              Modelling the spatio-temporal abundance dynamics of juvenile            ##
##                    and adult cod stock along the Western Balitc Sea                  ##
##                                    (Rufener et al.)                                  ##
#                                                                                        #
##########################################################################################

# Last update: February 2022

# Code written and mantained by Marie-Christine Rufener
# Contact < macrufener@gmail.com > for any query or to report code issues.




# This is the very first script to be performed.
# The LGNB-SDM model (Rufener et al., 2021) is used to predict the spatial and temporal abundance dynamics
# of both juvenile and adult individuals of Western Balitc cod stock. 
# For technical details and a toy example of the model, please refer to:
# https://github.com/mcruf/LGNB
# https://doi.org/10.1002/eap.2453 

# The present script is divided into the following 8 sections:
## Section 1: Default inputs
## Section 2: Load data files & packages
## Section 3: Binding data files 
## Section 4: Grid building
## Section 5: Discretize commercial hauls 
## Section 6: Defining the preferential sampling, if present 
## Section 7: TMB processing 
## Section 8: Fitting the LGNB model 

# The rationale of the script is the same as in https://github.com/mcruf/LGNB, except that here
# the model is run on a monthly time resolution, and sepparately for the age groups. 
# This means that for each age group, we will get a map of predicted densities for each given time period. 
# These predicted abundances will later be used to identify persisent abundance hotspot of juveniles and spawners.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 1: Default inputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
STOCK <- c("WBS","KAT")[1] # Specify to which stock the data should be subsetted (Western Baltic Sea or Kattegat)
DATA  <- c("commercial", "survey", "both") [3] # Specify the desired input data for the model; default is commercial data-
RESPONSE <- c("SizeGroup","AgeGroup","Cohort")[2] #Choose whether the model is applied for each SizeGroup, AgeGroup, or on a Cohort basis (when Nage).Default is set to SizeGroup.
PS <- c("No","One","Two")[1] #Define how the sampling nature should be accounted for
TIME <- c("YearMonth","YearQuarter","Year")[1] #Define the temporal resolution; default is set on a monthly basis


# @ PS = "No" -> For both datasets (commercial, survey), no preferential sampling is accounted for.
# @ PS = "One" -> Preferential sampling is accounted only for the commercial data.
# @ PS = "Two" -> Preferential sampling is accounted in both survey and commercial data; applies only to the integrated model (DATA="both")



# Specify the model structure; default model is m2for either size groups, age groups or cohort
# Default size group is 5 (S5), age group is 3 (A3) and cohort from 2005.
if(RESPONSE == "SizeGroup"){
  MODEL_CONFIG <- "m1_S5" #Default model and SizeGroup 5
}else if(RESPONSE == "AgeGroup"){
  MODEL_CONFIG <- "m1_A6" #Default model and AgeGroup 6
} else if(RESPONSE == "Cohort")
  MODEL_CONFIG <- "m1_2008" #Default model when applied on cohort-basis (2005 cohort) 


MODEL_CONFIG <- strsplit(MODEL_CONFIG, "_")[[1]]
MODEL_FORMULA <- MODEL_CONFIG[1]



if(RESPONSE == "SizeGroup"){
  SIZE <- MODEL_CONFIG[2]
} else if(RESPONSE == "AgeGroup"){
  AGE <- MODEL_CONFIG[2]
} else if(RESPONSE == "Cohort")
  YEARCLASS <- MODEL_CONFIG[2]



# For scripting (Useful when running on a HPC)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
input <- parse(text=Sys.getenv("SCRIPT_INPUT"))
print(input)
eval(input)
stopifnot(DATA %in% c("commercial", "survey", "both"))


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 2: Load data files & R packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.1) Load helper functions and R libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("~/src/utilities.R")



#devtools::install_github("kaskr/gridConstruct",subdir="gridConstruct") # To install the gridConstruct package
mLoad(raster,rgeos,maptools,maps,data.table,dplyr,TMB,sp,
      DATRAS,gridConstruct,rgdal,geosphere,devtools,plyr,fields)



# 2.2.1) Load commercial fisheries data (fishery-depedent)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
comFULL <- readRDS("E:/OneDrive - DTU/PhD/Manuscript_03/Data/Commercial/commercial_FULL_Age.rds"); colnames(comFULL)[5] <- "haulduration_hours" #Change name for convencience
comFULL$stock <- ifelse(comFULL$Area=="21","KAT","WBS")



# Subset data only for WBS cod and for the most representative gears
commercial <- subset(comFULL, stock == STOCK & metiers %in% c("OTB_DEF_>=105_1_110","OTB_DEF_>=105_1_120"))


# Set data-specific column
commercial$Data <- factor(rep("commercial", nrow(commercial)))


# Drop unused factor levels
commercial[,c("Month","Year","Quarter","Area","metiers","Data","HLID")] <- lapply(commercial[,c("Month","Year","Quarter","Area","metiers","Data","HLID")], factor)



# Set haulduration to 1 (ensures that no offset is accounted in the commercial data)
commercial$haulduration_hours <- 1 #Note: log(1)=0!



#Calculate a 5+ group
#~~~~~~~~~~~~~~~~~~~~
commercial$age_5 <- rowSums(commercial[,c("age_5","age_6")])
commercial$age_6 <- NULL



# 2.2.2) Load scientific survey data (Fishery-indepdendent)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
survey <- readRDS("E:/OneDrive - DTU/PhD/Manuscript_03/Data/Survey/survey_FULL_clean.rds") #same as the one used in the paper


survey$stock <- ifelse(survey$Area=="21","KAT","WBS") #Stock ID



# Subset data for WBS cod only
survey <- filter(survey, stock == STOCK)



# Include data-specific identifier (to be used later in the combined model)
survey$Data <- factor(rep("survey", nrow(survey)))


# Remove info from length groups
survey[,14:27] <- NULL



# Create an age group for A5+ (to few data from A4 onwards,actually)
survey$age_5 <- rowSums(survey[,19:22])
survey[,20:22] <- NULL

survey[,19][is.na(survey[,19])] <- 0


survey[,c("Month","Year","Quarter","Area","Data","haul.id","Ship","Gear")] <- lapply(survey[,c("Month","Year","Quarter","Area","Data","haul.id","Ship","Gear")], factor)



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 3: Binding survey and commercial data files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Binding depends whether response variable is on a cohort basis or AgeGroup/SizeGroup basis



















# 2.1.1) Cohort-basis
#~~~~~~~~~~~~~~~~~~~~~
# First we need to create a dataframe to ensure that both datasets have the same timelevels,
# without any types of lacks (quarters or years -> otherwise this will create problems for the AR1 process)
# Then, we extract the cohorts based on the previous created dataframe and for each data type

if(RESPONSE == "Cohort"){
  df_cohort  <- extractCohortLevels(commercial,survey,yearclass = as.numeric(YEARCLASS)) #Df with equal time-steps
  NageGroup  <- length(grep("age_", names(commercial), value = TRUE))
  tim        <- (as.numeric(YEARCLASS) + NageGroup)-1 #
  df_cohort  <- subset(df_cohort, year %in% c(YEARCLASS:tim))
  
  cohort_com <- extract_cohort_quarter(commercial,df_cohort) #Extract cohort for commercial data
  cohort_sur <- extract_cohort_quarter(survey,df_cohort) #Extract cohort for survey data
  
  cohort_com <- transform(cohort_com, HaulDur=haulduration_hours,numYear = as.numeric( as.character(Year)))
  cohort_sur <- transform(cohort_sur, latStart=lat, lonStart=lon, latEnd=lat, lonEnd=lon,HLID=haul.id, numYear = as.numeric(as.character(Year)))
  
  
  # 2.1.2) AgeGroup-basis
  #~~~~~~~~~~~~~~~~~~~~~~
} else if (RESPONSE == "AgeGroup"){
  age_com   <- transform(commercial, HaulDur=haulduration_hours, numYear = as.numeric( as.character(Year)))
  age_sur   <- transform(survey, latStart=lat, lonStart=lon, latEnd=lat, lonEnd=lon,
                         HLID=haul.id, numYear = as.numeric(as.character(Year)))
  
} 


# 2.2) Bind both datasets
#~~~~~~~~~~~~~~~~~~~~~~~~
if(RESPONSE == "Cohort" & INCLUDE =="both"){
  datatot <- mybind(cohort_com, cohort_sur)
} else if(RESPONSE == "Cohort" & INCLUDE == "survey"){
  datatot <- cohort_sur
} else if(RESPONSE == "Cohort" & INCLUDE == "commercial"){
  datatot <- cohort_com
} else if (RESPONSE == "AgeGroup" & INCLUDE == "both"){
  datatot <- mybind(age_com, age_sur)
} else if (RESPONSE == "AgeGroup" & INCLUDE == "survey") {
  datatot <- age_sur
} else if (RESPONSE == "AgeGroup" & INCLUDE == "commercial")
  datatot <- age_com


# Create equally time spaced intervals - VERY important for the AR1 process
# timeLevels <- as.vector(t(outer(min(datatot$numYear):max(datatot$numYear), 1:4, paste)))
# datatot$YearQuarter <- factor(paste(datatot$Year, datatot$Quarter), levels=timeLevels)

timeLevels <- as.vector(t(outer(min(datatot$numYear):max(datatot$numYear), 1:12, paste)))
datatot$YearMonth <- factor(paste(datatot$Year, datatot$Month), levels=timeLevels)




# 2.3) Define response variable for model applied on AgeGroup our SizeGroup basis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# For AgeGroups
if(RESPONSE == "AgeGroup"){
  if(AGE == "A0"){
    datatot$Response <- as.numeric(paste(datatot$age_0))
  } else if(AGE == "A1"){
    datatot$Response <- as.numeric(paste(datatot$age_1))
  } else if (AGE == "A2") {
    datatot$Response <- as.numeric(paste(datatot$age_2))
  } else if (AGE == "A3"){
    datatot$Response <- as.numeric(paste(datatot$age_3))
  } else if (AGE == "A4"){
    datatot$Response <- as.numeric(paste(datatot$age_4))
  } else if (AGE == "A5"){
    datatot$Response <- as.numeric(paste(datatot$age_5))  
  } else if (AGE == "A6"){
    datatot$Response <- as.numeric(paste(datatot$age_6))
  } else if (AGE == "A7"){
    datatot$Response <- as.numeric(paste(datatot$age_7))
  } else if (AGE == "A8"){
    datatot$Response <- as.numeric(paste(datatot$age_8))
  }
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Building grid for the study area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Grid for both survey and commercial data data should be the same.
# Here we take the full commercial data to build the grid


# Creating a dataframe with mean values of long and lat (take as base the commercial data)
comFULL$lon_mean <- rowMeans(comFULL[,c("lonStart", "lonEnd")])
comFULL$lat_mean <- rowMeans(comFULL[,c("latStart", "latEnd")])

df <- data.frame(lon=comFULL$lon_mean, lat=comFULL$lat_mean)


# Building the grid
#if(.Platform$OS.type == "windows") setwd("C:/Users/mruf/Desktop/LGCP_MSPTOOLS/LGNB_test/Shapefile/") #Specify directory with shapefile folder
setwd("Shapefile")
grid <- gridConstruct3(df,km=10,scale=1.2) #10 km grid
setwd("..")
gr <- gridFilter(grid,df,icesSquare = T,connected=T)
# plot(gr)
# plot(DK_map,add=T,fill=T,col="grey70")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Discretize and associate hauls along grid cells 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 4.1) Creating a data frame containing the trawl ID, starting long/lat and ending long/lat
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat <- data.frame(sampleID=datatot$HLID, start_long=datatot$lonStart,
                  start_lat=datatot$latStart, end_long=datatot$lonEnd, end_lat=datatot$latEnd)

# Plotting trawl paths
#segments(dat$start_long,dat$start_lat, dat$end_long, dat$end_lat, col="red",lwd=1.2)


# 4.2) Define matrix for start/end long & lat
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p1 <- matrix(c(dat$start_long,dat$start_lat), ncol=2)
p2 <- matrix(c(dat$end_long,dat$end_lat), ncol=2)


# 4.3) Interpolate intermediate points at regular distance (default is 1km)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nbpts <- floor(distance(dat$start_long,dat$start_lat, dat$end_long, dat$end_lat) / 1) # one point every 1 km ~ reasonable for a 5x5 km grid


# 4.4) Get intermediate points 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
inter_pts <- gcIntermediate(p1,p2, n=nbpts, addStartEnd=FALSE) #inter_pts returns a list within several list. We need to convert this list to a single dataframe


# 4.5) Create a dataframe to specify the frequency of match between each haul and grid ID. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note that the haul Id MUST be a factor, where each level is the frequency of a particular haul crossing a specific grid ID
tmp <- lapply(1:length(inter_pts), function(i) {
  print(i)
  x <- inter_pts[[i]]
  colnames(x) <- c("lon", "lat")
  x <- as.data.frame(x)
  haul.id <- datatot$HLID[i]
  ind <- gridLocate(gr, x)
  data.frame(haul.id=haul.id, ind=ind, rowID = i) 
})
tmp2 <- do.call("rbind", tmp)
tmp2$haulid <- factor(tmp2$haul.id)
tmp2$gf <- factor(tmp2$ind, 1:nrow(gr))
tmp2 <- tmp2[c("haulid","gf","rowID")] 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5) Define support areas (to be used later in association to the alpha-parameter)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 5.1) For single support area
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Applied when using only one alpha parameter for each dataset)
datatot$split_area <- ifelse(as.character(datatot$Data)=="commercial", "commercial", "survey") #Same configuration as model with a single alpha parameter, where we have only one single support area describes the commercial data by aggregating all hauls of the time series.
datatot$split_area <- as.factor(datatot$split_area)

kk <- tmp2; kk$split <- datatot$split_area[tmp2$rowID]
SupportAreaMatrix    <- table(kk$gf, kk$split)
SupportAreaMatrix[]  <- SupportAreaMatrix>0
SupportAreaMatrix    <- ifelse(SupportAreaMatrix==0,FALSE,TRUE)


# 5.2) For multiple support areas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Applied when using either one alpha parameter (collapsing all alphas into one), 
# or multiple alphas for commercial data, and one alpha parameter for survey)
if(SUPPORT_AREA == "Several"){
  datatot$split_area2 <- ifelse(as.character(datatot$Data)=="commercial", as.character(datatot$YearQuarter), "survey") #Same configuration as model with a single alpha parameter, where we have only one single support area describing the commercial data.
  datatot$split_area2 <- as.factor(datatot$split_area2)
  
  kk2 <- tmp2; kk2$split <- datatot$split_area2[tmp2$rowID]
  SupportAreaMatrix2 <- table(kk2$gf, kk2$split)
  SupportAreaMatrix2[] <- SupportAreaMatrix2>0
  SupportAreaMatrix2 <- ifelse(SupportAreaMatrix2==0,FALSE,TRUE)
}


# 5.3) Setting support areas based on chosen input
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(INCLUDE == "commercial" & SUPPORT_AREA == "Several"){
  SupportAreaMatrix2[] <- SupportAreaMatrix[,1]
  SupportAreaMatrix <- SupportAreaMatrix2 
} else if(INCLUDE == "both" & SUPPORT_AREA == "Several"){
  SupportAreaMatrix2[,1:(ncol(SupportAreaMatrix2)-1)] <- SupportAreaMatrix[,1]
  SupportAreaMatrix <- SupportAreaMatrix2 
} else if(INCLUDE == "commercial" & SUPPORT_AREA == "One"){
  SupportAreaMatrix <- SupportAreaMatrix
} else if (INCLUDE == "both" & SUPPORT_AREA =="One"){
  SupportAreaMatrix <- SupportAreaMatrix
}


# Map haulid to data.frame rows (VERY IMPORTANT: haulid must match with the dataframe's row number (in increasing order))
rowid <- match(as.character(tmp2$haulid),as.character(datatot$HLID))
stopifnot(all(is.finite(rowid)))
rowid <- factor(rowid)



# 6) Specifying spatio-temporal model with correlation among ages (cohort analysis)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 6.1) Sparse matrices for GMRF: Q = Q0+delta*I
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Q0 <- -attr(gr,"pattern")
diag(Q0) <- 0
diag(Q0) <- -rowSums(Q0)
I <- .symDiagonal(nrow(Q0))



# 6.2) Complile TMB
#~~~~~~~~~~~~~~~~~~
if(.Platform$OS.type == "windows") setwd("C:/Users/mruf/Desktop/LGCP_MSPTOOLS/LGNB_test/") #Go back to main folder 
#Sys.setenv(PATH="%PATH%;C:/Rtools/gcc-4.6.3/bin;c:/Rtools/bin") #Run only when on windows
compile("model.cpp")
dyn.load(dynlib("model"))



# 6.3) TMB Data
#~~~~~~~~~~~~~~~
# TMB data are set in such way that it recognizes automatically wheter one is using FD or FID data. 
data <- list(
  #time = datatot$YearQuarter[rowid],
  time = datatot$YearMonth[rowid],
  gf = tmp2$gf  ,              
  rowid = rowid  ,
  response = datatot$Response,
  Q0 = Q0,
  I = I,
  Xpredict = matrix(0,0,0),
  Apredict = factor(numeric(0)),
  SupportAreaMatrix = SupportAreaMatrix,
  SupportAreaGroup = if(SUPPORT_AREA == "Several"){
    as.factor(datatot$split_area2)
  } else if(SUPPORT_AREA == "One"){
    as.factor(datatot$split_area)
  },
  Data=datatot$Data,
  h = mean(summary(as.polygons(gr))$side.length) #To be used later to plot the spatial decorrelation as a function of distance
)



# Guess time effects (needed by index calculation)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
guess_time_effects_in_beta <- function(data, time_levels) {
  cn <- colnames(data$X)
  cn <- gsub(" ",":",time_levels)
  tl <- gsub(" ",":",time_levels)
  li <- lapply(tl, function(nm) grep(nm, cn) )
  if (any( sapply(li, length) > 1 )) {
    print(lapply(li, function(i)cn[i]))
    warning("Time effect is not unique. Index calc will be omitted")
    return (integer(0))
  }
  found <- sapply(li, function(x)x[1])
  found[is.na(found)] <- 0
  ## NOTE: Output length = length(time_levels)
  ## NOTE: NAs coded as "-1" ===> SKIP index calc or crash !!!
  as.integer(found) - 1L
}




# Fitting the model
#~~~~~~~~~~~~~~~~~~~~

## 'profile' speeds up fitting but may be inconvenient
fit_model <- function(data, model_struct=NULL, with_static_field=FALSE, profile=TRUE) {
  time_levels <- levels(data$time) # There must be at least one time levels; They MUST match between the two data types!
  grid_nlevels <- nlevels(data$gf)
  data$doPredict <- 0
  data$offset <- model_struct$offset 
  
  if(FALSE) { ## FIXME: prediction disabled
    ## Stuff for prediction: Dummy dataset that matches the space time grid:
    ## Xpredict: design matrix for prediction
    ## Apredict: Area factor for prediction
    ##DFpredict <- expand.grid(gf=levels(data$gf), time=levels(data$time))
    DFpredict <- expand.grid(gf=levels(data$gf), time=levels(data$time))
    ## FIXME: We should include depth and covariates here !
    ##        But that requires depth on the entire grid...
    Xpredict <- model.matrix(~time, data=DFpredict) ## <-- gear removed
    stopifnot( all( colnames(Xpredict) %in% colnames(data$X) ) ) ## Validity check
    tmp <- matrix(0, nrow(Xpredict), ncol(data$X))
    tmp[,match(colnames(Xpredict), colnames(data$X))] <- Xpredict
    data$Xpredict <- tmp
    data$Apredict <- factor(icesSquare(gr))
  }
  ## Perhaps we want measure all indices relative to a fixed reference square:
  ##   plot(cod, plot.response = FALSE)
  ## "42G1" seems appropriate
  ## data$refindex <- which(levels(data$Apredict) == "42G1")
  data$refindex <- 0 ## <-- Disable
  
  parameters <- list(
    eta_density = matrix(0,nrow(Q0),length(time_levels)),
    eta_nugget = numeric(0),
    logdelta = -4,       # Check values
    logscale = 0,         # Check values
    logsd_nugget = 0,    # Check values
    time_corr = 2,       # Check values
    beta = rep(0, ncol(data$X)),
    logphi = rep(0, nlevels(data$Data) ),
    alpha = rep(0, nlevels(data$SupportAreaGroup))
  )
  parameters$eta_static <- rep(0, grid_nlevels * with_static_field )
  parameters$logdelta_static <- rep(0, 1 * with_static_field )
  parameters$logscale_static <- rep(0, 1 * with_static_field )
  
  ## Plugin model specification
  if(!is.null(model_struct)) {
    data$Xs                 <- model_struct$Xs
    data$X                  <- model_struct$Xf
    data$beta_r_fac         <- model_struct$beta_r_fac
    parameters$beta         <- model_struct$beta
    parameters$beta_r       <- model_struct$beta_r
    parameters$beta_s       <- model_struct$beta_s
    parameters$beta_r_logsd <- model_struct$beta_r_logsd
    data$which_beta_time    <- guess_time_effects_in_beta(data, time_levels)
  }
  
  ## Prior std dev on fixed effects (for robustness only)
  data$huge_sd <- 100
  
  map <- list()
  if(TRUE) map$logsd_nugget <- factor(NA)
  
  if(ALPHA == "No" & INCLUDE == "both"){
    map$alpha <- factor(rep(NA,nlevels(data$SupportAreaGroup)))
  } else if(ALPHA == "No" & INCLUDE == "commercial") {
    map$alpha <- factor(rep(NA,nlevels(data$SupportAreaGroup)))
  } else if(ALPHA == "No" & INCLUDE == "survey"){
    map$alpha <- factor(rep(NA,nlevels(data$SupportAreaGroup)))
  } else if(ALPHA == "Single"){
    lev <- levels(data$SupportAreaGroup)
    lev[lev=="survey"] <- NA
    map$alpha <- factor(lev)
  } else if(ALPHA == "Multi"){
    lev <- levels(data$SupportAreaGroup)
    lev[lev=="survey"] <- NA
    map$alpha <- factor(lev)
    # map$alpha = factor(map$alpha)
  } 
  
  
  if(profile && (length(parameters$beta)>0 || length(parameters$beta)>0) ) {
    profile <- c("beta")
  } else {
    profile <- NULL
  }
  
  obj <- MakeADFun(data, parameters, random=c("eta_density","eta_nugget","eta_static","beta_r"),
                   profile = profile,
                   map=map, DLL="model")
  
  obj$env$tracepar <-TRUE
  
  .GlobalEnv$obj <- obj ## Copy for debugging
  runSymbolicAnalysis(obj)
  fit <- nlminb(obj$par,obj$fn,obj$gr)
  if(FALSE) { ## FIXME: disabled
    rep <- obj$report(obj$env$last.par.best)
    rownames(rep$logindex) <- levels(data$Apredict)
    colnames(rep$logindex) <- levels(data$time)
  }
  
  sdr <- sdreport(obj)
  s <- summary(sdr)
  est <- s[rownames(s) != "eta_density" & rownames(s) != "eta_nugget",]
  s <- summary(sdr,p.value = TRUE)
  s1 <- s[rownames(s) == "beta",]
  rownames(s1) <- head(colnames(data$X), nrow(s1)) # extracting names of the fixed effects
  s.fixed <- s1
  s1 <- s[rownames(s) == "beta_r",]
  rownames(s1) <- tail(colnames(data$X), nrow(s1)) # extracting names of random effects
  s.random <- s1
  
  s1
  s2 <- s[rownames(s) == "eta_density",]
  return(environment())
}





# Build Matrices for the model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
buildModelMatrices <- function(fixed, random=NULL, ..., offset=NULL, static=NULL, data) {
  mm <- function(formula, data) {
    ##myna<-function(object,...){object[is.na(object)]<-0; object}
    mf <- model.frame(formula, data, na.action=na.pass)
    ans <- model.matrix(formula, mf)
    ans[is.na(ans)] <- 0
    ans
  }
  Xf <- mm(fixed, data=data)
  if(!is.null(random))
    Xr <- lapply(list(random, ...), mm, data=data)
  else
    Xr <- list(matrix(NA, nrow(Xf), 0))
  if(!is.null(static))
    Xs <- mm(static, data=attr(data,"static"))
  else
    #Xs <- matrix(NA, 0, 0) # Kasper's version; works on my local machine but not in hpc
    Xs <- matrix(NA, nrow(gr), 0) #works on both local machine and hpc
  nr <- sapply(Xr, ncol)
  nf <- ncol(Xf)
  beta <- rep(0, nf)
  beta_r <- rep(0, sum(nr))
  beta_r_fac <- factor(rep(1:length(nr), nr))
  beta_r_logsd <- rep(0, nlevels(beta_r_fac))
  offset <- eval(offset, data)
  ns <- ncol(Xs)
  beta_s <- rep(0, ns)
  if(!is.null(offset)) stopifnot(is.numeric(offset)) else offset <- numeric(0)
  list(Xf=cbind(Xf, do.call("cbind", Xr)), Xs=Xs, nr=nr, nf=nf, ns=ns, beta=beta, beta_s=beta_s, beta_r=beta_r, beta_r_fac=beta_r_fac, beta_r_logsd=beta_r_logsd, offset=offset)
}




# Fit models
#~~~~~~~~~~~~

# Note:
# All models include the time-period as fixed effect (YearQuarter), and we also account for the haul duration (column HaulDur in the data) as an offset.
# For the commercial data, we might think of including the vessel ID as a random effect (efid column in the data)
# and maybe the metier as a fixed effect (there are presently only two; see table(commercial$metiers)).
# For survey, we have two vessels (table(survey$Ship)), and the possibility of including them as fixed effect
# was also discussed
# The combined model includes an additional column, which is the data-specific identifies (see table(datatot$Data))
# But here I'm not sure whether the "metiers" and "Ship" fixed effect from the commercial and survey data, respectively,
# should also be included? In doubt, I included them here

## Reverse levels (to re-parameterize)
## FIXME: Ensure that "survey" always comes first
data$Data <- factor(data$Data, rev(levels(data$Data)) )
datatot$Data <- factor(datatot$Data, rev(levels(datatot$Data)) )

if (MODEL_FORMULA == "m1") {
  if(INCLUDE == "commercial"){
    m1 <- buildModelMatrices(~  -1 + YearMonth + metiers,~ -1 + efid, offset= quote(log(HaulDur)), data=datatot)
  } else if(INCLUDE == "survey"){
    m1 <- buildModelMatrices(~ -1 + YearMonth + Ship, offset = quote(log(HaulDur)), data=datatot)
  } else {
    #m1 <- buildModelMatrices(~ -1 + YearQuarter + Ship + Data + metiers, ~ -1 + efid, offset= quote(log(HaulDur)), data=datatot)
    m1 <- buildModelMatrices(~ -1 + YearMonth + Ship + Data + metiers, ~ -1 + efid, offset= quote(log(HaulDur)), data=datatot)
    
  }
  system.time( env1 <- fit_model(data, m1, with_static_field = F, profile=PROFILE) )
}

## No vessel specific random effects
if (MODEL_FORMULA == "m0") {
  if(INCLUDE == "commercial"){
    m0 <- buildModelMatrices(~  -1 + YearQuarter + metiers, offset= quote(log(HaulDur)), data=datatot)
  } else if(INCLUDE == "survey"){
    m0 <- buildModelMatrices(~ -1 + YearQuarter + Ship, offset = quote(log(HaulDur)), data=datatot)
  } else {
    m0 <- buildModelMatrices(~ -1 + YearQuarter + Ship + Data + metiers, offset= quote(log(HaulDur)), data=datatot)
  }
  system.time( env1 <- fit_model(data, m0, with_static_field = F, profile=PROFILE) )
}



#~~~~~~~~~~~~~
# Save results
#~~~~~~~~~~~~~


#chk <- checkConsistency(obj, n=1000)


#obj$env$L.created.by.newton <- NULL ## Trim off very large object before saving
#local({obj<-NULL}, get(grep("^env",ls(),value=T))) 


#pl <- as.list(env1$sdr,"Estimate")
#image(gr, concTransform(pl$eta_density[,1]),col=tim.colors(99))

OUTFILE  <-
  paste("results_WBScod", paste(MODEL_CONFIG, collapse="_"), INCLUDE, ALPHA, SUPPORT_AREA, "noprofile"[!PROFILE], ".RData", sep="_")
save.image(file=OUTFILE)

