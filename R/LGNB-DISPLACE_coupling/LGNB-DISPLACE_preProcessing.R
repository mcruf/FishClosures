##########################################################################################
#                                                                                        #
##                         Coupling LGNB-SDM model to DISPLACE                          ##
##                                    (Rufener et al.)                                  ##
#                                                                                        #
##########################################################################################

# Last update: March 2022

# Code written and mantained by Marie-Christine Rufener
# Contact < macrufener@gmail.com > for any query or to report code issues.



# This is a pre-processing script before moving to the two LGNB-DISPLACE coupling scripts.
# In order to assure the coupling, we need to retrieve some important parameters from the
# spatio-temporal correlation parameters. To do so, we have first to re-evaluate
# the objective function from the LGNB result output, and from there retrieve
# the spatio-temporal parameters and the hessian matrix.

#! Note that this script uses an extended version of the LGNB-SDM outputs.
#! More specifically, it includes the objective function of the model, which
#! will be used along this script. The resulting file is considerably larger
#! then the simplified output version and does not fit into the GitHub's repository.
#! Please request the full outputs on the above highlighted contact.



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Loading libraries & functions, set working directory and load the results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(TMB)
library(gridConstruct)
library(Matrix)
library(fields)
library(raster)
library(tidyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.2) Load LGNB-SDM model 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("C:/Users/mruf/Documents/Fish_Closures/src")
#compile("model.cpp") # Compile model only when running script for the first time
dyn.load(dynlib("LGNB")) # Load the same C++ model used to generate the loaded results!




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.3) Load model results for each size group 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Raw_results_perSizeGroup/")


# FIXME: the loaded model is saved in "env1" because it corresponds to model 1 in the LGNB script;



# SizeGroup 1 (0-5cm)
#~~~~~~~~~~~~~~~~~~~~~~~~

# There was no data for such small size group for neither survey nor commercial data.
# Thus, no LGNB model was applied to this size group.



# SizeGroup 2 (5-10cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S2_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h


SG2 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","gr")))


# SizeGroup 3 (10-15cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S3_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG3 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3", "gr")))


# SizeGroup 4 (15-20cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S4_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG4 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4", "gr")))


# SizeGroup 5 (20-25cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S5_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG5 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5", "gr")))


# SizeGroup 6 (25-30cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S6_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG6 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5","SG6","gr")))


# SizeGroup 7 (30-35cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S7_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG7 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5","SG6","SG7","gr")))


# SizeGroup 8 (35-40cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S8_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG8 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5","SG6","SG7","SG8","gr")))


# SizeGroup 9 (40-45cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S9_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG9 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5","SG6","SG7","SG8","SG9","gr")))


# SizeGroup 10 (45-50cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S10_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG10 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5","SG6","SG7","SG8","SG9","SG10","gr")))


# SizeGroup 11 (50-55cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S11_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG11 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5","SG6","SG7","SG8","SG9","SG10","SG11","gr")))


# SizeGroup 12 (55-60cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S12_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG12 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5","SG6","SG7","SG8","SG9","SG10","SG11","SG12","gr")))


# SizeGroup 13 (60-65cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S13_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG13 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5","SG6","SG7","SG8","SG9","SG10","SG11","SG12","SG13","gr")))


# SizeGroup 14 (65+cm)
#~~~~~~~~~~~~~~~~~~~~~~~~
load("results_WBScod_size_m1_S14_both_No_One.RData") 
obj <- env1$obj # Retrieve obj from model
obj$fn(env1$fit$par)
lpb <- obj$env$last.par.best #Get spatio-temporal parameters
r <- obj$env$random # Get latent (random) variables
h <- obj$env$spHess(lpb, random=TRUE) #Hessian matrix based on the spatio-temporal models;This is where we need the loaded model.cpp file (done in section 1.2)
#image(h)

env1$lpb <- lpb
env1$r   <- r
env1$h   <- h

SG14 <- env1
rm(list=setdiff(ls(), c("SG1","SG2","SG3","SG4","SG5","SG6","SG7","SG8","SG9","SG10","SG11","SG12","SG13","SG14","gr")))



# Save processed outputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~
# save <- paste("SG",2:14, sep="")
# 
# for(i in seq_along(save)){
#   saveRDS(i,"E:/PhD/Project III/Results/Size_Distribution_DISPLACE_input/Full_results/WBS_cod_SG2.rds")
#   
# }

