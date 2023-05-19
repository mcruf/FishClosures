#####################################################################################
#                                                                                   #
#         Evaluate population abundance dynamics along simulation period            #
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Process simulation outputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~
# General inputs
#~~~~~~~~~~~~~~~~
scenarioname <- c("scelgnbcouplingnoclosure", # Baseline
                  "scelgnbcouplingwclosure", # Seasonal spawning closure
                  "scelgnbcouplingspwclosure", #Spawning area closure
                  "scelgnbcouplingoldspwclosure", #Old spawner area closure
                  "scelgnbcouplingnurseclosure", #Nursery area closure 
                  "scelgnbcouplingfeedclosure" #Feeding area closure 
                   ) 



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
  #main.path <-  file.path("/Users", "marie-christinerufener", "Desktop", "WBSsimu", "test", scenario)
  
  
  file <- grep("popstats_simu",list.files(main.path))
  file <- paste(main.path, list.files(main.path)[file], sep="/")
  
  
  # Remove simulations that went wrong
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Some simulations provided very unrealistic patterns, where the stock collapsed
  ## suddenly at a random time even when it showed an increased trend. This can be
  ## seen by plotting the SSB along time (see SSB_along_time.R script). We have to remove
  ## these simulations to ensure unbiased results:
  
  if(scenario == 'scelgnbcouplingnoclosure'){
    ## Simu no. 11 went wrong
    # idx <- grep("11", file)
    # file <- file[-idx]
  } else if(scenario == "scelgnbcouplingwclosure"){
    ## Simu no. 14 & 17
    idx <- grep("14", file)
    idx2 <- grep("17", file)
    file <- file[-c(idx, idx2)]
  } else if(scenario == "scelgnbcouplingspwclosure"){
    ## Simu no. 14 
    idx <- grep("14", file)
    file <- file[-idx]
  } else if(scenario == "scelgnbcouplingfeedclosure"){
    # Simu no. 7
    idx <- grep("7", file)
    file <- file[-idx]
  }
  
  
  ## Now read the result files
  pop_stats <- list()
  for(i in seq_along(file)){
    pop_stats[[i]] <- read.table(file[[i]])
  }

  
  
  ## Bind list file into single data frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for(nsim in seq_along(pop_stats)){
    pop_stats[[nsim]]$nsimu <- paste('nsimu', nsim, sep='')
  }
  
  
  allsim <- do.call('rbind', pop_stats) 
  colnames(allsim) <- c("tstep", "stk", paste0("N",0:13), paste0("W",0:13), paste0("SSB",0:13), 'nsimu')
  
  
  ## Filter data for desired species & time steps
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  library(dplyr)
  
  tsteps <- levels(factor(allsim$tstep))
  
  #cod <- filter(allsim, pop == 2 & tstep %in% tstep_months$V1); rownames(cod) <- NULL
  cod <- filter(allsim, stk == 2 & tstep %in% c(tsteps[1:120])); rownames(cod) <- NULL
  
  
  
  
  ## Make year column for better readability
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  csum <- cumsum(rep(12,10))
  
  tstep_months = unique(cod$tstep)
  
  Y1 <- tstep_months[1:csum[1]]
  Y2 <- tstep_months[(csum[1]+1):csum[2]]
  Y3 <- tstep_months[(csum[2]+1):csum[3]]
  Y4 <- tstep_months[(csum[3]+1):csum[4]]
  Y5 <- tstep_months[(csum[4]+1):csum[5]]
  Y6 <- tstep_months[(csum[5]+1):csum[6]]
  Y7 <- tstep_months[(csum[6]+1):csum[7]]
  Y8 <- tstep_months[(csum[7]+1):csum[8]]
  Y9 <- tstep_months[(csum[8]+1):csum[9]]
  Y10 <- tstep_months[(csum[9]+1):csum[10]]
  
  
  cod$Year <- ifelse(cod$tstep %in% Y1, "Y1",
                     ifelse(cod$tstep %in% Y2, "Y2",
                            ifelse(cod$tstep %in% Y3, "Y3",
                                   ifelse(cod$tstep %in% Y4, "Y4",
                                          ifelse(cod$tstep %in% Y5, "Y5",
                                                 ifelse(cod$tstep %in% Y6, "Y6",
                                                        ifelse(cod$tstep %in% Y7, "Y7",
                                                               ifelse(cod$tstep %in% Y8, "Y8",
                                                                      ifelse(cod$tstep %in% Y9, "Y9", "Y10")))))))))
  
  
  
  ## Transform data from wide to long-format
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  codw_N <- tidyr::gather(cod, SizeGroup, Nindividuals, N0:N13); codw_N[,3:30] <- NULL; codw_N$bio <- 'Numbers'
  codw_W <- tidyr::gather(cod, SizeGroup, Nindividuals, W0:W13); codw_W[,3:30] <- NULL; codw_W$bio <- 'Weight'
  codw_SSB <- tidyr::gather(cod, SizeGroup, Nindividuals, SSB0:SSB13); codw_SSB[,3:30] <- NULL; codw_SSB$bio <- 'SSB'
  
  codw <- rbind(codw_N,codw_W, codw_SSB)
  
  
  ## Calculate total abundance per year, size group & simulation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  codw[, c("nsimu", "Year", "SizeGroup", 'bio')] <- lapply(codw[, c("nsimu", "Year", "SizeGroup", 'bio')],factor) #Ser factor wherever applicable
  
  
  ## Numbers
  abundance <- codw %>%
    filter(bio == 'Numbers') %>%
    group_by(Year, nsimu, SizeGroup) %>% #Calculate total catches per year & length group
    dplyr::summarize(Total_abundance = sum(Nindividuals, na.rm=T)) %>%
    group_by(Year, SizeGroup) %>% #Now calculate the average total catch across simulations
    dplyr::summarize(Abundance_avg = mean(Total_abundance, na.rm=T),
                     Abundance_sd = sd(Total_abundance, na.rm=T)) 
  
  
  ## Weight
  weight <- codw %>%
    filter(bio == 'Weight') %>%
    group_by(Year, nsimu, SizeGroup) %>% #Calculate total catches per year & length group
    dplyr::summarize(Total_weight = sum(Nindividuals, na.rm=T)) %>%
    group_by(Year, SizeGroup) %>% #Now calculate the average total catch across simulations
    dplyr::summarize(Weigth_avg = mean(Total_weight, na.rm=T),
                     Weigth_sd = sd(Total_weight, na.rm=T)) 
  
  
  ## SSB
  ssb <- codw %>%
    filter(bio == 'SSB') %>%
    group_by(Year, nsimu, SizeGroup) %>% #Calculate total catches per year & length group
    dplyr::summarize(Total_ssb = sum(Nindividuals, na.rm=T)) %>%
    group_by(Year, SizeGroup) %>% #Now calculate the average total catch across simulations
    dplyr::summarize(SSB_avg = mean(Total_ssb, na.rm=T),
                     SSB_sd = sd(Total_ssb, na.rm=T)) 
  
  
  
  ## Rename size groups
  #~~~~~~~~~~~~~~~~~~~~~~
  
  ## Abundance
  abundance$SizeGroup <- dplyr::recode_factor(abundance$SizeGroup, 
                                                    N0   = "0-5",
                                                    N1   = "6-10",
                                                    N2   = "11-15",
                                                    N3   = "16-20",
                                                    N4   = "21-25",
                                                    N5   = "26-30",
                                                    N6   = "31-35",
                                                    N7   = "36-40",
                                                    N8   = "41-45",
                                                    N9   = "46-50",
                                                    N10  = "51-55",
                                                    N11  = "56-60",
                                                    N12  = "61-65",
                                                    N13  = ">66")
          
  
  ## Weight
  weight$SizeGroup <- dplyr::recode_factor(weight$SizeGroup, 
                                              W0   = "0-5",
                                              W1   = "6-10",
                                              W2   = "11-15",
                                              W3   = "16-20",
                                              W4   = "21-25",
                                              W5   = "26-30",
                                              W6   = "31-35",
                                              W7   = "36-40",
                                              W8   = "41-45",
                                              W9   = "46-50",
                                              W10  = "51-55",
                                              W11  = "56-60",
                                              W12  = "61-65",
                                              W13  = ">66")
  
  
  ## SSB
  ssb$SizeGroup <- dplyr::recode_factor(ssb$SizeGroup, 
                                           SSB0   = "0-5",
                                           SSB1   = "6-10",
                                           SSB2   = "11-15",
                                           SSB3   = "16-20",
                                           SSB4   = "21-25",
                                           SSB5   = "26-30",
                                           SSB6   = "31-35",
                                           SSB7   = "36-40",
                                           SSB8   = "41-45",
                                           SSB9   = "46-50",
                                           SSB10  = "51-55",
                                           SSB11  = "56-60",
                                           SSB12  = "61-65",
                                           SSB13  = ">66")
  
  
  ## merge all the data into one
  dat <- merge(abundance, weight, by = c('Year', 'SizeGroup'))
  dat <- merge(dat, ssb,  by = c('Year', 'SizeGroup'))
  
  
  ## Include scenario name
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat$Scenario <- scenario
  
  
  
  
  
  ## Put it into list
  #~~~~~~~~~~~~~~~~~~~~
  results[[sce]] <- dat
  
  rm(list=setdiff(ls(), c("results", 'scenarioname')))
  
  
}

allsim <- do.call('rbind', results) # Now bind list into single data frame



#~~~~~~~~~~~~
# 2) Plotting
#~~~~~~~~~~~~

## Reorder Year column to correct order
allsim$Year <- factor(allsim$Year, levels = paste('Y', 1:10, sep =''))

## Reorder Scenario level order
allsim$Scenario <- as.factor(allsim$Scenario)
allsim$Scenario <- factor(allsim$Scenario, levels = scenarioname )

## Rename scenario names
levels(allsim$Scenario)

allsim$Scenario <- recode_factor(allsim$Scenario, 
                                 scelgnbcouplingnoclosure = 'Baseline',
                                 scelgnbcouplingwclosure = 'Seasonal Spawning Closure',
                                 scelgnbcouplingspwclosure = 'Spawning Area Closure',
                                 scelgnbcouplingoldspwclosure = 'Old Spawner Area Closure',
                                  scelgnbcouplingnurseclosure = 'Nursery Area Closure',
                                 scelgnbcouplingfeedclosure = 'Feeding Area Closure'
                                 )


## Abundance per size group
allsim %>% 
  dplyr::select(-c('Weigth_avg','SSB_avg' )) %>% 
  filter(Year %in% c("Y1", "Y5","Y10") &
         SizeGroup != '0-5') %>%
  ggplot(aes(x=SizeGroup, y=Abundance_avg, fill=Scenario)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  # geom_errorbar(aes(ymin=Abundance_avg - Abundance_sd, ymax=Abundance_avg + Abundance_sd), width=.2,
  #               position=position_dodge(.9)) + #upper and lower error bars
  geom_errorbar(aes(ymin=Abundance_avg, ymax=Abundance_avg + Abundance_sd), width=.2, size=1,
                position=position_dodge(.9)) +
  #scale_fill_jco() +
  #scale_fill_uchicago() +
  #scale_fill_manual(values = c(pal_jco("default")(8)[c(2,3,5:6,4,7)])) +
  #scale_fill_manual(values = c(pal_jco("default")(8)[c(4,2,3,5:6,7)])) +
  scale_fill_manual(values = c(pal_jco("default")(8)[c(4,3, 5:6,2,7)])) +
  
  facet_grid(Scenario ~ Year) +
  #ggtitle(scenario) +
  theme_bw() +
  xlab("\nSize Group (cm)") + ylab("\nAbundance (N)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        #plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.text = element_text(size=16),
        axis.title.x = element_text(size =18, face ='bold'),
        axis.title.y = element_text(size =18, face ='bold',margin = margin(t = 0, r = 20, b = 0, l = 0)),
        strip.background = element_rect(fill = 'gray95'),
        strip.text = element_text(face = 'bold', size = 17),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold', size = 15),
        legend.text = element_text(size = 14),
        panel.spacing.y = unit(1, "lines"),
        strip.text.y = element_blank())


ggsave('/Users/marie-christinerufener/Desktop/Review_FR/R2/Figures/DISPLACE/N_at_size.png',
       dpi = 300, width = 35, height = 40, unit='cm')




## Abundance along time
# allsim %>% 
#   dplyr::select(-c('Weigth_avg','SSB_avg' )) %>% 
#   filter(Year %in% c("Y1", "Y5","Y10") &
#            SizeGroup != '0-5') %>%
#   ggplot(aes(y=Abundance_avg, x = Year, group = Scenario, col = Scenario)) +
#   geom_point() +
#   geom_line(lwd = 0.8) +
#   #scale_color_jco() +
#   scale_color_manual(values = c(pal_jco("default")(8)[c(2,4,5:8)])) +
#   facet_wrap(.~SizeGroup, scales = 'free_y') +
#   theme_bw() +
#   xlab("Size Group (cm)") + ylab("Abundance (N)") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11),
#         axis.text.y = element_text(size=11),
#         axis.title = element_text(size =13, face ='bold'),
#         plot.title = element_text(hjust = 0.5, face = 'bold'),
#         strip.background = element_rect(fill = 'gray95'),
#         strip.text = element_text(face = 'bold', size = 12),
#         legend.position = 'bottom')


