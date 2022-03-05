##########################################################################################
#                                                                                        #
##                  Identify abundance hotspots of juveniles (recruits)                 ##
##                                    (Rufener et al.)                                  ##
#                                                                                        #
##########################################################################################

# Last update: March 2022

# Code written and mantained by Marie-Christine Rufener
# Contact < macrufener@gmail.com > for any query or to report code issues.


# In the first script (LGNB_SpawnerRecruits.R) script we have run the LGNB-SDM 
# for WB cod sepparately for each age group (age 0 - age5+).
# The model was run on a monthly basis from 2005-2019, which means that we get
# predicted abundance maps for each month of the time series.


# In order to identify and evaluate the persistency of recruits (A0-A2) and spawner abundance hotspot, we will have to follow three consecutive steps:

### Step 1) Stack the YearMonth abundance layers of the individual age groups. For each YearMonth map, we then sum over all abundance values across the spatial grid IDs.
### This provides an "total juvenile abundance" layer for each YearMonth, and is essentially analogous
### to the predict first, assemble later approach (Ferrier and Guisan, 2006)

### Step 2) Take the generated YearMonth maps and evaluate the abundance hotspots following Bartolino's et al. (2011) approach - A frequency distribution approach to hotspot identification

#### Step 3) Evaluate the presistency of the identified hotpost along the considered time period by using Colloca's et al. (2009) approach - Identifying fish nurseries using density and presistence areas






#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 1: Set default inputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Lstage <- c("Recruits","Spawners")[1] #Set appropriate life stage for which the abundance hotspot should be identified; default is "Recruits"




#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Section 2: R packages & Load data files 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 2.1) Load R libraries
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




# 2.2) Load results from LGNB-SDM
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Recruits: A0-A2
## Spawners: A3-A5+

# Note: the LGNB-SDM output provides a dataframe where each column is a time-period.
# Since we ran the model from 2005-2019 on a monthly basis, this means that we will
# have 180 columns (V1-V180).

# To identify recruits hotpspots, we will have to take all the months into consideration, in opposit to
# the spawner hotspot, as for the latter we know upfront the monhts corresponding to spawning (JAN-MAR).
# Whereas for the recruits we will stack the abundance layers from all months of the time series, for 
# spawners we will only stack the abundance layers corresponding to the first three months of each year.







if(Lstage == "Recruits"){
  
  
  ## Set WD where the recruits results are stored
  setwd("C:/Users/mruf/Documents/FishClosures/Data/Hotspots/Recruits/")
  
  
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
  
  
  
  abulist <- list(A0,A1,A2); names(abulist) <- paste("Age",0:2,sep="")
  
  
  
  
} else if(Lstage == "Spawners"){
  
  
  ## Set WD where the recruits results are stored
  setwd("C:/Users/mruf/Documents/FishClosures/Data/Hotspots/Spawners/")
  
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
  
  
  
}