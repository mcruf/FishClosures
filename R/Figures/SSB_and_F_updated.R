##########################################################################################
#
#           plotting SSB and F between initial and final simulation step
#
###########################################################################################

## The first part plots SSB/SSBinit & F/Finit
## The second part plots the relative change of SSB (or F) relative to the baseline scenario


library(dplyr)
library(ggplot2)



#~~~~~~~~~~~~~~~~~~~~
## Part 1) SSB/SSBinit
#~~~~~~~~~~~~~~~~~~~~

general <- list()
general$case_study <- "BalticSea" #change name according to application (e.g., myfish)


## Specify directories to DISPLACE application & simulation outputs  
if(.Platform$OS.type == "unix") {
  
  # general$main.path         <- file.path("~","DISPLACE_outputs")                                                                                                   
  # general$main.path.param   <- file.path("~","ibm_vessels","WBSsimu",paste("DISPLACE_input_",general$case_study, sep=""))
  # general$main.path.ibm     <- file.path("~","ibm_vessels","WBSsimu",paste("DISPLACE_input_", general$case_study, sep='')) 
  
  # general$main.path         <- file.path("~", "DISPLACE_outputs")   
  # general$main.path.igraph  <- file.path("~","ibm_vessels","WBSsimu", paste("DISPLACE_input_",general$case_study, sep=""), "graphsspe")
  # general$main.path.param   <- file.path("~","ibm_vessels","WBSsimu", paste("DISPLACE_input_gis_",general$case_study, sep=""))
  # general$main.path.ibm     <- file.path("~","ibm_vessels","WBSsimu", paste("DISPLACE_input_", general$case_study, sep=''))
  # 
  general$main.path         <- file.path("~","Desktop","Review_FR", "R2","Results", "DISPLACE", "DISPLACE_outputs")   
  general$main.path.igraph  <- file.path("~","Desktop","WBSsimu", paste("DISPLACE_input_",general$case_study, sep=""), "graphsspe")
  general$main.path.param   <- file.path("~","Desktop","WBSsimu", paste("DISPLACE_input_gis_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("~","Desktop","WBSsimu", paste("DISPLACE_input_", general$case_study, sep=''))
  
  
  # do not forget to install the R packages on the qrsh interactive node linux platform, i.e. R > install.packages("data.table"), etc.
  # (and possibly kill the current jobs on HPC with the command qselect -u $USER | xargs qdel)
  # submit the shell to HPC with the command qsub ./IBM_processoutput_plots_for_loglike.sh
}



## Specify additional inputs to the Baltic Sea Application
if(general$case_study == "BalticSea"){
  general$igraph            <- 300
  general$a.year            <- "2016" #Starting year of the simulation
  general$a.country         <- c("DEU", "DNK", "EST", "FIN", "LTU", "LVA", "POL", "SWE") #Countries of interest
  general$nbpops            <- 37  #No. of simulated populations
  general$nbszgroup         <- 14 #No. of size groups
  general$namefolderinput   <- "BalticSea"
  general$use_sqlite        <- FALSE
  
  
  ### Specify here the folder names with the outputs of the simulations
  general$namefolderoutput  <- c(     "scelgnbcouplingnoclosure",
                                      #"scelgnbcouplingnoclosurenoitq",
                                      "scelgnbcouplingwclosure",
                                      #"scelgnbcouplingwclosurenoitq",
                                      "scelgnbcouplingspwclosure",
                                      "scelgnbcouplingoldspwclosure",
                                      "scelgnbcouplingnurseclosure",
                                      "scelgnbcouplingfeedclosure"
  ) 
  
  ### Specify the names of the simulaions & with the number of simulations that were conducted
  general$namesimu           <- list(
    "scelgnbcouplingnoclosure"  =   paste("simu", c(1:20), sep=''),
   # "scelgnbcouplingnoclosurenoitq"  =   paste("simu", c(1:20), sep=''),
    "scelgnbcouplingwclosure" =   paste("simu", c(1:20), sep=''),
    #"scelgnbcouplingwclosurenoitq"  =   paste("simu", c(1:20), sep=''),
    "scelgnbcouplingspwclosure"  =   paste("simu", c(1:20), sep=''),
    "scelgnbcouplingoldspwclosure" =   paste("simu", c(1:20), sep=''),
   "scelgnbcouplingnurseclosure"  =   paste("simu", c(1:20), sep=''),
    "scelgnbcouplingfeedclosure"  =   paste("simu", c(1:20), sep='')
  ) 
  
  
  ### Rename the scenarios with desired name - NOTE: Needs to be in the same order as above
  the_scenarios1 <-  c("Baseline",
                       #"Baseline + No ITQ",
                       "Seasonal Spawning Closure",
                       #"Seasonal Spawning Closure + No ITQ", 
                       "Spawning Area Closure",
                       "Old Spawner Area Closure",
                       "Nursery Area Closure",
                       "Feeding Area Closure"
  )
  
}






if(general$case_study=="BalticSea"){
  implicit_pops <- c(0, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 36)
  explicit_pops <- c(0:36)[-(implicit_pops+1)] 
}



selected_scenarios <- names(general$namesimu)

# look at annual indics such as the TACs...
res <- NULL

for(sce in selected_scenarios) {
  print(paste("sce ", sce))
  for(simu in general$namesimu[[sce]]) {
    print(paste("sim ", simu))
    # merge all infos
    annual_indics              <-  read.table (file=file.path(general$main.path, general$namefolderinput, sce, paste('popdyn_annual_indic_',  simu,".dat", sep='')))
    colnames(annual_indics)    <-  c("tstep", "stk", "multi", "multi2", "Fbar", "totland_kg", "totdisc_kg", "SSB_kg", "tac", paste0("N",0:10), paste0("F",0:10), paste0("W",0:10), paste0("M",0:10))
    
    annual_indics <- annual_indics [, 1:9]   # FOR NOW...
    res <- rbind (res, cbind(annual_indics, sce=sce, simu=paste(simu, sep="_")))
  }
}



outcome_firsty <- res[res$tstep==8761,]  
#outcome_lasty <- res[res$tstep==35065,]  
outcome_lasty <- res[res$tstep==96433,]  
outcome <- merge(outcome_firsty, outcome_lasty, 
                 by.x=c('stk', 'sce', 'simu'), 
                 by.y=c('stk', 'sce', 'simu'))

outcome$"F/Finit" <- outcome$Fbar.y/outcome$Fbar.x
outcome$"SSB/SSBinit" <- outcome$SSB_kg.y/outcome$SSB_kg.x



outcome$sce <- factor(outcome$sce)
outcome$sce <- factor(outcome$sce, levels=selected_scenarios, labels=  the_scenarios1)

# put in long format
df1 <- cbind.data.frame(outcome[,c('stk','sce','simu','F/Finit')], var="F/Finit")
df2 <- cbind.data.frame(outcome[,c('stk','sce','simu','SSB/SSBinit')] , var="SSB/SSBinit")
colnames(df1) <- colnames(df2) <- c('stk','sce','simu','value','var')
out <- rbind.data.frame(df1,df2)



# SSB, F and whatever 
the_dim        <- c(2400, 2400)
# namefile       <- paste0("responsecurves_bio_laty_",selected)
namefile       <- paste0("responsecurves_bio_laty_",selected_scenarios)
output.folder  <- file.path(general$main.path, general$namefolderinput)
tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
     width = the_dim[1], height = the_dim[2],
     units = "px", pointsize = 12,  res=450, compression=c("lzw"))


library(ggplot2)

## Remove some odd outliers
out2 <- out %>% filter(stk == 2 & var == 'SSB/SSBinit') %>% filter(value < 5)
out2b <- out %>% filter(stk == 2 & var == 'F/Finit') %>% filter(value < 1.5)

out3 <- rbind(out2, out2b)


p <- 
  #out3 %>% filter(stk == 2) %>%
  out3 %>% filter(stk == 2 & !(sce %in% "Baseline")) %>%
  ggplot(aes(x=sce, y=value, fill=var,alpha=var))  + 
  geom_boxplot(outlier.shape=NA)  +
  labs(x = "Scenario", y = "Value")  + 
  facet_wrap( ~ var, scales="free") + 
  #ylim(0, 1) + #For all other scenarios
  #ylim(0, 0.45) + # For DISPLACE vs. DISPLACE-LGNB scenario
  scale_fill_manual(values=c("#69b3a2", "grey")) +
  scale_alpha_manual(values=c(0.8,0.8)) +
#scale_fill_discrete(name = "", labels = c("F/Finit", "SSB/SSBinit"))
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=18), 
          axis.text.y = element_text(size=18),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=18),
          strip.text.x = element_blank(),  
          panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
          strip.background = element_blank(),
          
          #legend.position="bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=15),
          
          
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6),
          
          panel.border = element_rect(colour = "black")) +
    geom_abline(intercept=0, slope=0, color="grey", lty=2)  + 
    geom_boxplot(outlier.shape=NA) 



dev.off()

################################################################################

# 
# #~~~~~~~~~~~~~~~~~~~~
# ## Part 2) Plot for relative change
# #~~~~~~~~~~~~~~~~~~~~
# 
# 
# ### Plot for relative change
# 
# general <- list()
# general$case_study <- "BalticSea" #change name according to application (e.g., myfish)
# 
# 
# ## Specify directories to DISPLACE application & simulation outputs
# if(.Platform$OS.type == "unix") {
# 
#   # general$main.path         <- file.path("~","DISPLACE_outputs")
#   # general$main.path.param   <- file.path("~","ibm_vessels","WBSsimu",paste("DISPLACE_input_",general$case_study, sep=""))
#   # general$main.path.ibm     <- file.path("~","ibm_vessels","WBSsimu",paste("DISPLACE_input_", general$case_study, sep=''))
# 
#   # general$main.path         <- file.path("~", "DISPLACE_outputs")
#   # general$main.path.igraph  <- file.path("~","ibm_vessels","WBSsimu", paste("DISPLACE_input_",general$case_study, sep=""), "graphsspe")
#   # general$main.path.param   <- file.path("~","ibm_vessels","WBSsimu", paste("DISPLACE_input_gis_",general$case_study, sep=""))
#   # general$main.path.ibm     <- file.path("~","ibm_vessels","WBSsimu", paste("DISPLACE_input_", general$case_study, sep=''))
#   #
#   general$main.path         <- file.path("~","Desktop","Review_FR", "R2","Results", "DISPLACE", "DISPLACE_outputs")
#   general$main.path.igraph  <- file.path("~","Desktop","WBSsimu", paste("DISPLACE_input_",general$case_study, sep=""), "graphsspe")
#   general$main.path.param   <- file.path("~","Desktop","WBSsimu", paste("DISPLACE_input_gis_",general$case_study, sep=""))
#   general$main.path.ibm     <- file.path("~","Desktop","WBSsimu", paste("DISPLACE_input_", general$case_study, sep=''))
# 
# 
#   # do not forget to install the R packages on the qrsh interactive node linux platform, i.e. R > install.packages("data.table"), etc.
#   # (and possibly kill the current jobs on HPC with the command qselect -u $USER | xargs qdel)
#   # submit the shell to HPC with the command qsub ./IBM_processoutput_plots_for_loglike.sh
# }
# 
# 
# 
# ## Specify additional inputs to the Baltic Sea Application
# if(general$case_study == "BalticSea"){
#   general$igraph            <- 300
#   general$a.year            <- "2016" #Starting year of the simulation
#   general$a.country         <- c("DEU", "DNK", "EST", "FIN", "LTU", "LVA", "POL", "SWE") #Countries of interest
#   general$nbpops            <- 37  #No. of simulated populations
#   general$nbszgroup         <- 14 #No. of size groups
#   general$namefolderinput   <- "BalticSea"
#   general$use_sqlite        <- FALSE
# 
# 
#   ### Specify here the folder names with the outputs of the simulations
#   general$namefolderoutput  <- c(     "scelgnbcouplingnoclosure",
#                                       #"scelgnbcouplingnoclosurenoitq",
#                                       "scelgnbcouplingwclosure",
#                                       #"scelgnbcouplingwclosurenoitq",
#                                       "scelgnbcouplingspwclosure",
#                                       "scelgnbcouplingnurseclosure",
#                                       "scelgnbcouplingoldspwclosure",
#                                       "scelgnbcouplingfeedclosure"
#   )
# 
#   ### Specify the names of the simulaions & with the number of simulations that were conducted
#   general$namesimu           <- list(
#     "scelgnbcouplingnoclosure"  =   paste("simu", c(1:20), sep=''),
#     #"scelgnbcouplingnoclosurenoitq"  =   paste("simu", c(1:5), sep=''),
#     "scelgnbcouplingwclosure" =   paste("simu", c(1:20), sep=''),
#     #"scelgnbcouplingwclosurenoitq"  =   paste("simu", c(1:5), sep=''),
#     "scelgnbcouplingspwclosure"  =   paste("simu", c(1:20), sep=''),
#     "scelgnbcouplingnurseclosure"  =   paste("simu", c(1:20), sep=''),
#     "scelgnbcouplingoldspwclosure" =   paste("simu", c(1:20), sep=''),
#     "scelgnbcouplingfeedclosure"  =   paste("simu", c(1:20), sep='')
#   )
# 
# 
#   ### Rename the scenarios with desired name - NOTE: Needs to be in the same order as above
#   the_scenarios1 <-  c("Baseline",
#                        #"Baseline + No ITQ",
#                        "Seasonal Spawning Closure",
#                        #"Seasonal Spawning Closure + No ITQ",
#                        "Spawning Area Closure",
#                        "Nursery Area Closure",
#                        "Old Spawner Area Closure",
#                        "Feeding Area Closure"
#   )
# 
# }
# 
# 
# 
# 
# 
# 
# if(general$case_study=="BalticSea"){
#   implicit_pops <- c(0, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 36)
#   explicit_pops <- c(0:36)[-(implicit_pops+1)]
# }
# 
# 
# 
# selected_scenarios <- names(general$namesimu)
# 
# 
# 
# 
# # look at annual indics such as the TACs...
# res <- NULL
# for(sce in selected_scenarios) {
#   print(paste("sce ", sce))
#   for(simu in general$namesimu[[sce]]) {
#     print(paste("sim ", simu))
#     # merge all infos
#     annual_indics              <-  read.table (file=file.path(general$main.path, general$namefolderinput, sce, paste('popdyn_annual_indic_',  simu,".dat", sep='')))
#     colnames(annual_indics)    <-  c("tstep", "stk", "multi", "multi2", "Fbar", "totland_kg", "totdisc_kg", "SSB_kg", "tac", paste0("N",0:10), paste0("F",0:10), paste0("W",0:10), paste0("M",0:10))
# 
#     annual_indics <- annual_indics [, 1:9]   # FOR NOW...
#     res <- rbind (res, cbind(annual_indics, sce=sce, simu=paste(simu, sep="_")))
#   }
# }
# 
# 
# 
# outcome_firsty <- res[res$tstep==8761,]
# #outcome_lasty <- res[res$tstep==35065,]
# outcome_lasty <- res[res$tstep==96433,]
# 
# 
# outcome <- merge(outcome_firsty, outcome_lasty, by.x=c('stk', 'sce', 'simu'), by.y=c('stk', 'sce', 'simu'))
# outcome$"F/Finit" <- outcome$Fbar.y/outcome$Fbar.x
# outcome$"SSB/SSBinit" <- outcome$SSB_kg.y/outcome$SSB_kg.x
# 
# 
# 
# outcome$sce <- factor(outcome$sce)
# outcome$sce <- factor(outcome$sce, levels=selected_scenarios, labels=  the_scenarios1)
# 
# 
# # put in long format
# df1 <- cbind.data.frame(outcome[,c('stk','sce','simu','F/Finit')], var="F/Finit")
# df2 <- cbind.data.frame(outcome[,c('stk','sce','simu','SSB/SSBinit')] , var="SSB/SSBinit")
# colnames(df1) <- colnames(df2) <- c('stk','sce','simu','value','var')
# out <- rbind.data.frame(df1,df2)
# 
# 
# 
# ## Go for the plotting
# #~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# ## Plotting SSB/F against baseline scenario (no closure)
# res2 <- subset(res, stk =="2")
# 
# res2$tstep <- as.factor(res2$tstep)
# res2$simu <- as.factor(res2$simu)
# 
# 
# ordlvls <- paste("simu", 1:20, sep="")
# res2$simu <- factor(res2$simu, levels = ordlvls )
# 
# res2$sce <- as.factor(res2$sce)
# res2$sce <- factor(res2$sce, levels = selected_scenarios) #Ensure that the Baseline is the first level
# 
# 
# library(dplyr)
# res2_chg <- res2 %>%
#   filter(tstep %in% c("8761", "96433")) %>%
#   arrange(simu, tstep, sce) %>%
#   group_by(tstep,simu) %>%
#   mutate(CHG_SSB = ((SSB_kg  - SSB_kg [1L])/SSB_kg [1L])*100,
#          CHG_F = ((Fbar - Fbar [1L])/Fbar [1L])*100) %>%
#   ungroup() %>%
#   data.frame()
# 
# head(res2_chg)
# 
# 
# res2_chg$sce <- factor(res2_chg$sce)
# #res2_chg$sce <- factor(res2_chg$sce, levels=selected_scenarios, labels=  labels_selected_scenarios)
# res2_chg$sce <- factor(res2_chg$sce, levels=selected_scenarios, labels=  the_scenarios1)
# 
# 
# library(ggplot2)
# #96433
# p_ssb <- res2_chg %>%
#   # subset( sce %in% c("SESC", "SPSC", "SPNC") &
#   #           tstep %in% c("8761", "35065")) %>%
#   filter(tstep %in% c("8761", "96433")) %>%
# 
#   ggplot(aes(x=sce, y=CHG_SSB, fill=tstep,  alpha=tstep))  +
#   #geom_boxplot(outlier.shape=NA)  +
#   labs(x = "Scenario", y = "Relative change - SSB")  +
#   #facet_wrap( ~ tstep, ncol=1, scales="free_y") +
#   #facet_wrap( tst ~ stk, ncol=2, scales="free_y") +
# 
#   #ylim(0, 1) + #For all other scenarios
#   ylim(-100, 0) + # For DISPLACE vs. DISPLACE-LGNB scenario
#   scale_fill_manual(values=c("grey", "#69b3a2"),
#                     labels = c("Tinit", "Tfinal")) +
#   scale_alpha_manual(values=c(0.8,0.8), labels = c("Tinit", "Tfinal")) +
#   geom_abline(intercept=0, slope=0, color="darkorange", lty=2, size=1)  +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=18),
#         axis.text.y = element_text(size=18),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size=18),
#         strip.text.x = element_blank(),
#         panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
#         strip.background = element_blank(),
# 
#         #legend.position="bottom",
#         legend.title = element_blank(),
#         legend.text = element_text(size=15),
# 
# 
#         #legend.position = "bottom",
#         #legend.justification = c("right", "top"),
#         legend.box.just = "right",
#         legend.margin = margin(6, 6, 6, 6),
# 
#         panel.border = element_rect(colour = "black")) +
#   geom_boxplot(outlier.shape=NA)
# 
# 
# 
# p_f <- res2_chg %>%
#   # subset( sce %in% c("SESC", "SPSC", "SPNC") &
#   #           tstep %in% c("8761", "35065")) %>%
#   filter(tstep %in% c("8761", "96433")) %>%
#   ggplot(aes(x=sce, y=CHG_F, fill=tstep,  alpha=tstep))  +
#   #geom_boxplot(outlier.shape=NA)  +
#   labs(x = "Scenario", y = "Relative change (%) - F")  +
#   #facet_wrap( ~ tstep, ncol=1, scales="free_y") +
#   #facet_wrap( tst ~ stk, ncol=2, scales="free_y") +
# 
#   #ylim(0, 1) + #For all other scenarios
#   ylim(-100, 0) + # For DISPLACE vs. DISPLACE-LGNB scenario
#   scale_fill_manual(values=c("grey", "#69b3a2"),
#                     labels = c("Tinit", "Tfinal")) +
#   scale_alpha_manual(values=c(0.8,0.8), labels = c("Tinit", "Tfinal")) +
#   geom_abline(intercept=0, slope=0, color="darkorange", lty=2, size=1)  +
#   theme_bw()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1,size=18),
#         axis.text.y = element_text(size=18),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size=18),
#         strip.text.x = element_blank(),
#         panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
#         strip.background = element_blank(),
# 
#         #legend.position="bottom",
#         legend.title = element_blank(),
#         legend.text = element_text(size=15),
# 
# 
#         #legend.position = "bottom",
#         #legend.justification = c("right", "top"),
#         legend.box.just = "right",
#         legend.margin = margin(6, 6, 6, 6),
# 
#         panel.border = element_rect(colour = "black")) +
#   geom_boxplot(outlier.shape=NA)
# 
# # library(gridExtra)
# # grid.arrange(p_ssb, p_f,ncol=2)
# 
# library(ggpubr)
# 
# allplot <- ggarrange(p_ssb,p_f, common.legend = T, legend="right")
# 
# setwd("/Users/marie-christinerufener/Desktop/Review_r1/Figures")
# ggsave(plot=allplot, "Figure_6_updated.png",dpi=450, width = 21, height = 10, units = "cm", bg="white")
# 
# 
# 
# 
# 
# 
# ## Plotting as in first paper draft (= phd thesis) - plotting SSB/F in relation of initial and final state for all scenarios
# 
# 
# # SSB, F and whatever
# the_dim        <- c(2400, 2400)
# # namefile       <- paste0("responsecurves_bio_laty_",selected)
# namefile       <- paste0("responsecurves_bio_laty_",selected_scenarios)
# output.folder  <- file.path(general$main.path, general$namefolderinput)
# tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
#      width = the_dim[1], height = the_dim[2],
#      units = "px", pointsize = 12,  res=450, compression=c("lzw"))
# 
# 
# library(ggplot2)
# p <- ggplot(out[out$stk==2,], aes(x=sce, y=value, fill=var,alpha=var))  +
#   geom_boxplot(outlier.shape=NA)  +
#   labs(x = "Scenario", y = "Value")  +
#   #facet_wrap( ~ stk, ncol=1, scales="fixed") +
#   facet_wrap( var ~ stk, ncol=2, scales="free_y") +
# 
#   #ylim(0, 1) + #For all other scenarios
#   #ylim(0, 0.45) + # For DISPLACE vs. DISPLACE-LGNB scenario
#   scale_fill_manual(values=c("#69b3a2", "grey")) +
#   scale_alpha_manual(values=c(0.6,1))
# #scale_fill_discrete(name = "", labels = c("F/Finit", "SSB/SSBinit"))
# print(
#   p   +
#     theme_bw()+
#     theme(axis.text.x = element_text(angle = 45, hjust = 1,size=18),
#           axis.text.y = element_text(size=18),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size=18),
#           strip.text.x = element_blank(),
#           panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
#           strip.background = element_blank(),
# 
#           #legend.position="bottom",
#           legend.title = element_blank(),
#           legend.text = element_text(size=15),
# 
# 
#           legend.position = "bottom",
#           #legend.justification = c("right", "top"),
#           legend.box.just = "right",
#           legend.margin = margin(6, 6, 6, 6),
# 
#           panel.border = element_rect(colour = "black")) +
#     geom_abline(intercept=0, slope=0, color="grey", lty=2)  +
#     geom_boxplot(outlier.shape=NA)
# )
# 
# 
# dev.off()
