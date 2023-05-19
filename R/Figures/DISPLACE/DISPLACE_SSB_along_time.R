#####################################################################################
#                                                                                   #
#                      Evaluate SSB along simulation period                         #
#                                                                                   #
#####################################################################################



#~~~~~~~~~~~~~~~
# Load libraries
#~~~~~~~~~~~~~~~

library(dplyr)
library(ggplot2)
library(viridis)
library(ggsci)


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Process simulation outputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~
# General inputs
#~~~~~~~~~~~~~~~~

#scenario <- c("scelgnbcouplingnoclosure", "scenbcpcouplingnoclosure")[1] #scenario with 'ITQ', scenario without 'ITQ'
scenarioname <- c(#"scelgnbcouplingnoclosurenoitq", #No closure no ITQ
                  "scelgnbcouplingnoclosure", # No closure with ITQ
                  #"scelgnbcouplingwclosurenoitq", #With closure no ITQ
                  "scelgnbcouplingwclosure", #With closure with ITQ
                  "scelgnbcouplingfeedclosure", #Feeding closure with ITQ
                  "scelgnbcouplingnurseclosure", #Nursery closure with ITQ
                  "scelgnbcouplingspwclosure", #Spawning closure with ITQ
                  "scelgnbcouplingoldspwclosure") #Old spawner closure



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process simulation outputs 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

results <- list() # To store all scenario outputs into list



for(sce in seq_along(scenarioname)){
  
  scenario <- scenarioname[sce]
  
  
  # Load population dyn files
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Adapt to your own path
  main.path <-  file.path("/Users", "marie-christinerufener", "Desktop", "Review_FR", "R2", "Results", "DISPLACE", "DISPLACE_outputs", "BalticSea", scenario)

  file <- grep("popdyn_SSB_simu",list.files(main.path))
  file <- paste(main.path, list.files(main.path)[file], sep="/")
  
  
  pop_dyn <- list()
  for(i in seq_along(file)){
    pop_dyn[[i]] <- read.table(file[[i]])
  }
  
  
  
  ## Bind list file into single data frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for(nsim in seq_along(pop_dyn)){
    pop_dyn[[nsim]]$nsimu <- paste('nsimu', nsim, sep='')
  }
  
  
  allsim <- do.call('rbind', pop_dyn) 
  colnames(allsim) <- c("tstep", "stk", paste0("SSB",0:13), 'nsimu')
  
  
  
  ## Filter data for desired species & time steps
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  library(dplyr)
  
  tsteps <- levels(factor(allsim$tstep))
  
  #cod <- filter(allsim, pop == 2 & tstep %in% tstep_months$V1); rownames(cod) <- NULL
  dat <- filter(allsim, stk == 2); rownames(dat) <- NULL
  
  
  ## Calculate total SSB abundace
  dat$totpop <- apply(dat[,paste0("SSB",0:13)], 1, sum)
  
  
  ## Specify scenario
  dat$scenario <- paste(scenario)
  
  
  ## Put all scenarios into list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  results[[sce]] <- dat
  
  }
  

allsim <- do.call('rbind', results) # Now bind list into single data frame


#~~~~~~~~~~~~
# 2) Plotting
#~~~~~~~~~~~~

## Reorder Scenario level order
allsim$scenario <- as.factor(allsim$scenario)
allsim$scenario <- factor(allsim$scenario, levels = scenarioname )

## Rename scenario names
levels(allsim$scenario)

allsim$scenario <- recode_factor(allsim$scenario, 
                                 # scelgnbcouplingnoclosurenoitq = 'Baseline + No ITQ',
                                 scelgnbcouplingnoclosure = 'Baseline',
                                 # scelgnbcouplingwclosurenoitq = 'Seasonal Spawning Closure + No ITQ',
                                 scelgnbcouplingwclosure = 'Seasonal Spawning Closure',
                                 scelgnbcouplingfeedclosure = 'Feeding Area Closure',
                                 scelgnbcouplingnurseclosure = 'Nursery Area Closure',
                                 scelgnbcouplingspwclosure = 'Spawning Area Closure',
                                 scelgnbcouplingoldspwclosure = 'Old Spawner Area Closure')


library(ggpubr)

allsim %>% 
  filter(scenario == 'Old Spawner Area Closure') %>%
  mutate(nsimu = factor(nsimu, levels=paste('nsimu',1:20, sep =''))) %>%
  
  ggplot(aes(x=as.character(tstep), y = totpop, group = scenario, col=nsimu)) + 
  geom_point() +
  geom_line() +
  facet_wrap(. ~ nsimu, ncol=5) +
  #ggtitle(scenario) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.text = element_text(size=11),
        axis.title = element_text(size =13, face ='bold'),
        strip.background = element_rect(fill = 'gray95'),
        strip.text = element_text(face = 'bold', size = 12),
        legend.position = 'bottom',
        strip.text.y = element_blank())


