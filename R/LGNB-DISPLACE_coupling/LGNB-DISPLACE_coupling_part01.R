##########################################################################################
#                                                                                        #
##                         Coupling LGNB-SDM model to DISPLACE                          ##
##                              Part 1: data pre-processing                             ##
##                                    (Rufener et al.)                                  ##
#                                                                                        #
##########################################################################################

# Last update: March 2022

# Code written and mantained by Marie-Christine Rufener
# Contact < macrufener@gmail.com > for any query or to report code issues.




# This script processes the LGNB-SDM model outputs to retain only the most
# important objects that will be used for the LGNB-DISPLACE coupling (script 2)
# This includes: 
# * spatio-tempral correlation parameters (phi, delta, scale)
# * Precision matrix (Q)
# * time periods (to identify the last time-period)
# * estimated abundances for each time period (transformed to natural scale)


#! Note that this script uses an extended version of the LGNB-SDM outputs.
#! More specifically, it includes the objective function of the model, which
#! will be used along this script. The resulting file is considerably larger
#! then the simplified output version and does not fit into the GitHub's repository.
#! Please request the full outputs on the above highlighted contact.



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Loading libraries & set working directory 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(TMB)
library(gridConstruct)
library(Matrix)
library(fields)
library(raster)
library(tidyr)


#setwd("E:/PhD/Project III/Results/Size_Distribution_DISPLACE_input/Full_results_per_SizeGroup")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) Load model results for each size group and set into a list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# FIXME: the loaded model is saved in "env1" because it corresponds to model 1 in the LGNB script;


# First load the grid

load("~/Raw_results_per_SizeGroup/results_WBScod_size_m1_S2_both_No_One.RData") 
rm(list=setdiff(ls(), c("gr")))



# SizeGroup 1 (0-5cm)
#~~~~~~~~~~~~~~~~~~~~~~~~

# There was no data for such small size group for neither survey nor commercial data.
# Thus, no LGNB model was applied to this size group.

# Create a dummy list
lSG1 <- ls()



# SizeGroup 2 (5-10cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG2 <- readRDS("WBS_cod_SG2.rds") 
time_corr <- SG2$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG2$lpb["logdelta"]) # delta (spatial correlation param.)

lSG2 <- list() # Put everyhing into a list
lSG2$abundance <- as.data.frame(as.list(SG2$sdr, "Estimate")[1]);
lSG2$abundance <- apply(lSG2$abundance,2,exp) #Put on natural scale
lSG2$gr <- gr
lSG2$time_period <- SG2$data$time

lSG2$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG2$delta <- exp(SG2$lpb["logdelta"]) # delta (spatial correlation param.)
lSG2$scale <- exp(SG2$lpb["logscale"]) # scale (spatial correlation param)
lSG2$Q <- SG2$obj$env$data$Q0 + delta * SG2$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2")))


# SizeGroup 3 (10-15cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG3 <- readRDS("WBS_cod_SG3.rds") 
time_corr <- SG3$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG3$lpb["logdelta"]) # delta (spatial correlation param.)

lSG3 <- list() # Put everyhing into a list
lSG3$abundance <- as.data.frame(as.list(SG3$sdr, "Estimate")[1]);
lSG3$abundance <- apply(lSG3$abundance,2,exp) #Put on natural scale
lSG3$gr <- gr
lSG3$time_period <- SG3$data$time

lSG3$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG3$delta <- exp(SG3$lpb["logdelta"]) # delta (spatial correlation param.)
lSG3$scale <- exp(SG3$lpb["logscale"]) # scale (spatial correlation param)
lSG3$Q <- SG3$obj$env$data$Q0 + delta * SG3$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3")))




# SizeGroup 4 (15-20cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG4 <- readRDS("WBS_cod_SG4.rds") 
time_corr <- SG4$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG4$lpb["logdelta"]) # delta (spatial correlation param.)

lSG4 <- list() # Put everyhing into a list
lSG4$abundance <- as.data.frame(as.list(SG4$sdr, "Estimate")[1]);
lSG4$abundance <- apply(lSG4$abundance,2,exp) #Put on natural scale
lSG4$gr <- gr
lSG4$time_period <- SG4$data$time

lSG4$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG4$delta <- exp(SG4$lpb["logdelta"]) # delta (spatial correlation param.)
lSG4$scale <- exp(SG4$lpb["logscale"]) # scale (spatial correlation param)
lSG4$Q <- SG4$obj$env$data$Q0 + delta * SG4$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4")))



# SizeGroup 5 (20-25cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG5 <- readRDS("WBS_cod_SG5.rds") 
time_corr <- SG5$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG5$lpb["logdelta"]) # delta (spatial correlation param.)

lSG5 <- list() # Put everyhing into a list
lSG5$abundance <- as.data.frame(as.list(SG5$sdr, "Estimate")[1]);
lSG5$abundance <- apply(lSG5$abundance,2,exp) #Put on natural scale
lSG5$gr <- gr
lSG5$time_period <- SG5$data$time

lSG5$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG5$delta <- exp(SG5$lpb["logdelta"]) # delta (spatial correlation param.)
lSG5$scale <- exp(SG5$lpb["logscale"]) # scale (spatial correlation param)
lSG5$Q <- SG5$obj$env$data$Q0 + delta * SG5$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5")))



# SizeGroup 6 (25-30cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG6 <- readRDS("WBS_cod_SG6.rds") 
time_corr <- SG6$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG6$lpb["logdelta"]) # delta (spatial correlation param.)

lSG6 <- list() # Put everyhing into a list
lSG6$abundance <- as.data.frame(as.list(SG6$sdr, "Estimate")[1]);
lSG6$abundance <- apply(lSG6$abundance,2,exp) #Put on natural scale
lSG6$gr <- gr
lSG6$time_period <- SG6$data$time

lSG6$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG6$delta <- exp(SG6$lpb["logdelta"]) # delta (spatial correlation param.)
lSG6$scale <- exp(SG6$lpb["logscale"]) # scale (spatial correlation param)
lSG6$Q <- SG6$obj$env$data$Q0 + delta * SG6$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5","lSG6")))



# SizeGroup 7 (30-35cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG7 <- readRDS("WBS_cod_SG7.rds") 
time_corr <- SG7$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG7$lpb["logdelta"]) # delta (spatial correlation param.)

lSG7 <- list() # Put everyhing into a list
lSG7$abundance <- as.data.frame(as.list(SG7$sdr, "Estimate")[1]);
lSG7$abundance <- apply(lSG7$abundance,2,exp) #Put on natural scale
lSG7$gr <- gr
lSG7$time_period <- SG7$data$time

lSG7$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG7$delta <- exp(SG7$lpb["logdelta"]) # delta (spatial correlation param.)
lSG7$scale <- exp(SG7$lpb["logscale"]) # scale (spatial correlation param)
lSG7$Q <- SG7$obj$env$data$Q0 + delta * SG7$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5","lSG6","lSG7")))


# SizeGroup 8 (35-40cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG8 <- readRDS("WBS_cod_SG8.rds") 
time_corr <- SG8$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG8$lpb["logdelta"]) # delta (spatial correlation param.)

lSG8 <- list() # Put everyhing into a list
lSG8$abundance <- as.data.frame(as.list(SG8$sdr, "Estimate")[1]);
lSG8$abundance <- apply(lSG8$abundance,2,exp) #Put on natural scale
lSG8$gr <- gr
lSG8$time_period <- SG8$data$time

lSG8$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG8$delta <- exp(SG8$lpb["logdelta"]) # delta (spatial correlation param.)
lSG8$scale <- exp(SG8$lpb["logscale"]) # scale (spatial correlation param)
lSG8$Q <- SG8$obj$env$data$Q0 + delta * SG8$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5","lSG6","lSG7","lSG8")))



# SizeGroup 9 (40-45cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG9 <- readRDS("WBS_cod_SG9.rds") 
time_corr <- SG9$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG9$lpb["logdelta"]) # delta (spatial correlation param.)

lSG9 <- list() # Put everyhing into a list
lSG9$abundance <- as.data.frame(as.list(SG9$sdr, "Estimate")[1]);
lSG9$abundance <- apply(lSG9$abundance,2,exp) #Put on natural scale
lSG9$gr <- gr
lSG9$time_period <- SG9$data$time

lSG9$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG9$delta <- exp(SG9$lpb["logdelta"]) # delta (spatial correlation param.)
lSG9$scale <- exp(SG9$lpb["logscale"]) # scale (spatial correlation param)
lSG9$Q <- SG9$obj$env$data$Q0 + delta * SG9$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5","lSG6","lSG7","lSG8","lSG9")))



# SizeGroup 10 (45-50cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG10 <- readRDS("WBS_cod_SG10.rds") 
time_corr <- SG10$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG10$lpb["logdelta"]) # delta (spatial correlation param.)

lSG10 <- list() # Put everyhing into a list
lSG10$abundance <- as.data.frame(as.list(SG10$sdr, "Estimate")[1]);
lSG10$abundance <- apply(lSG10$abundance,2,exp) #Put on natural scale
lSG10$gr <- gr
lSG10$time_period <- SG10$data$time

lSG10$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG10$delta <- exp(SG10$lpb["logdelta"]) # delta (spatial correlation param.)
lSG10$scale <- exp(SG10$lpb["logscale"]) # scale (spatial correlation param)
lSG10$Q <- SG10$obj$env$data$Q0 + delta * SG10$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5","lSG6","lSG7","lSG8","lSG9","lSG10")))


# SizeGroup 11 (50-55cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG11 <- readRDS("WBS_cod_SG11.rds") 
time_corr <- SG11$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG11$lpb["logdelta"]) # delta (spatial correlation param.)

lSG11 <- list() # Put everyhing into a list
lSG11$abundance <- as.data.frame(as.list(SG11$sdr, "Estimate")[1]);
lSG11$abundance <- apply(lSG11$abundance,2,exp) #Put on natural scale
lSG11$gr <- gr
lSG11$time_period <- SG11$data$time

lSG11$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG11$delta <- exp(SG11$lpb["logdelta"]) # delta (spatial correlation param.)
lSG11$scale <- exp(SG11$lpb["logscale"]) # scale (spatial correlation param)
lSG11$Q <- SG11$obj$env$data$Q0 + delta * SG11$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5","lSG6","lSG7","lSG8","lSG9","lSG10","lSG11")))


# SizeGroup 12 (55-60cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG12 <- readRDS("WBS_cod_SG12.rds") 
time_corr <- SG12$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG12$lpb["logdelta"]) # delta (spatial correlation param.)

lSG12 <- list() # Put everyhing into a list
lSG12$abundance <- as.data.frame(as.list(SG12$sdr, "Estimate")[1]);
lSG12$abundance <- apply(lSG12$abundance,2,exp) #Put on natural scale
lSG12$gr <- gr
lSG12$time_period <- SG12$data$time

lSG12$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG12$delta <- exp(SG12$lpb["logdelta"]) # delta (spatial correlation param.)
lSG12$scale <- exp(SG12$lpb["logscale"]) # scale (spatial correlation param)
lSG12$Q <- SG12$obj$env$data$Q0 + delta * SG12$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5","lSG6","lSG7","lSG8","lSG9","lSG10","lSG11","lSG12")))


# SizeGroup 13 (60-65cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG13 <- readRDS("WBS_cod_SG13.rds") 
time_corr <- SG13$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG13$lpb["logdelta"]) # delta (spatial correlation param.)

lSG13 <- list() # Put everyhing into a list
lSG13$abundance <- as.data.frame(as.list(SG13$sdr, "Estimate")[1]);
lSG13$abundance <- apply(lSG13$abundance,2,exp) #Put on natural scale
lSG13$gr <- gr
lSG13$time_period <- SG13$data$time

lSG13$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG13$delta <- exp(SG13$lpb["logdelta"]) # delta (spatial correlation param.)
lSG13$scale <- exp(SG13$lpb["logscale"]) # scale (spatial correlation param)
lSG13$Q <- SG13$obj$env$data$Q0 + delta * SG13$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5","lSG6","lSG7","lSG8","lSG9","lSG10","lSG11","lSG12","lSG13")))


# SizeGroup 14 (65+cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
SG14 <- readRDS("WBS_cod_SG14.rds") 
time_corr <- SG14$lpb["time_corr"] #untransformed time-correlation parameter
delta <- exp(SG14$lpb["logdelta"]) # delta (spatial correlation param.)

lSG14 <- list() # Put everyhing into a list
lSG14$abundance <- as.data.frame(as.list(SG14$sdr, "Estimate")[1]);
lSG14$abundance <- apply(lSG14$abundance,2,exp) #Put on natural scale
lSG14$gr <- gr
lSG14$time_period <- SG14$data$time

lSG14$phi <- time_corr / sqrt(1.0 + time_corr*time_corr) # phi (time correlation param.)
lSG14$delta <- exp(SG14$lpb["logdelta"]) # delta (spatial correlation param.)
lSG14$scale <- exp(SG14$lpb["logscale"]) # scale (spatial correlation param)
lSG14$Q <- SG14$obj$env$data$Q0 + delta * SG14$obj$env$data$I # Precision matrix

rm(list=setdiff(ls(), c("gr","lSG1","lSG2","lSG3","lSG4","lSG5","lSG6","lSG7","lSG8","lSG9","lSG10","lSG11","lSG12","lSG13","lSG14")))





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Make a list of all size-group lists
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fullres <- list(SG2=lSG2,SG3=lSG3,SG4=lSG4,SG5=lSG5,SG6=lSG6,SG7=lSG7,
                SG8=lSG8,SG9=lSG9,SG10=lSG10,SG11=lSG11,SG12=lSG12,SG13=lSG13,SG14=lSG14)

# Set colnames based on timesteps
for(i in seq_along(fullres)){
  colnames(fullres[[i]]$abundance) <- levels(fullres[[i]]$time_period) 
}



# Save full list (to be used in script 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save(fullres, file="~/Data/LGNB-DISPLACE/WBScod_all_SizeGroups.RData") 
