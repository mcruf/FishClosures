if(TRUE){
  # GENERAL SETTINGS
  general <- list()
  
  general$case_study <- "BalticSea" #change name according to application (e.g., myfish)
  
  if(.Platform$OS.type == "unix") {}
  
  general$main.path         <- file.path("~","DISPLACE_outputs")                                                                                                   
  general$main.path.igraph  <- file.path("~","DISPLACE_input_raw", "igraph")
  general$main.path.param   <- file.path("~",paste("DISPLACE_input_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("~",paste("DISPLACE_input_", general$case_study, sep='')) 
  # do not forget to install the R packages on the qrsh interactive node linux platform, i.e. R > install.packages("data.table"), etc.
  # (and possibly kill the current jobs on HPC with the command qselect -u $USER | xargs qdel)
  # submit the shell to HPC with the command qsub ./IBM_processoutput_plots_for_loglike.sh
  
  if(.Platform$OS.type == "windows") {
    #general$main.path         <- file.path("C:","Users","mruf","Documents","PHD_projects","Proj_3","DISPLACE_outputs")  
    general$main.path         <- file.path("D:","PhD","Project III","Results","DISPLACE","DISPLACE_outputs")     
    general$main.path.igraph  <- file.path("C:","Users","mruf","Documents","PHD_projects","Proj_3",paste("DISPLACE_input_",general$case_study, sep=""), "graphsspe")
    general$main.path.param   <- file.path("C:","Users","mruf","Documents","PHD_projects","Proj_3",paste("DISPLACE_input_gis_",general$case_study, sep=""))
    general$main.path.ibm     <- file.path("C:","Users","mruf","Documents","PHD_projects","Proj_3", paste("DISPLACE_input_", general$case_study, sep='')) 
  }
  
  if(general$case_study=="BalticSea"){
    general$igraph            <- 100
    general$a.year            <- "2016"
    general$a.country         <- c("DEU", "DNK", "EST", "FIN", "LTU", "LVA", "POL", "SWE")
    general$nbpops            <- 37  
    general$nbszgroup         <- 14
    general$namefolderinput   <- "BalticSea"
    general$use_sqlite        <- FALSE
    
    
    general$namefolderoutput  <- c( "scebaselinenoclosure",
                                    "scebaseline",
                                    "scenbcpcouplingnoclosure",
                                    "scenbcpcoupling",
                                    "scenbcpcouplingspw",
                                    "scenbcpcouplingrec"
    ) 
    
    
    general$namesimu           <- list( "scebaselinenoclosure"          =   paste("simu", c(1:50), sep=''), 
                                        "scebaseline"       	          =   paste("simu", c(1:50), sep=''),
                                        "scenbcpcouplingnoclosure"   	  =   paste("simu", c(1:50), sep=''), 
                                        "scenbcpcoupling"   	          =   paste("simu", c(1:50), sep=''), 
                                        "scenbcpcouplingspw"	          =   paste("simu", c(1:50), sep=''), 
                                        "scenbcpcouplingrec"	          =   paste("simu", c(1:50), sep='')
                                        
    ) 
    
    
    the_scenarios1 <-  c("Baseline without closure", 
                         "Baseline", 
                         "DISPLACE-LGNB without closure", 
                         "DISPLACE-LGNB with closure",
                         "Spawning closure", 
                         "Nursery closures"
    )
    
  }
  
  
} # end FALSE






if(general$case_study=="BalticSea"){
        implicit_pops <- c(0, 3, 4, 5, 6, 7, 8, 9, 10, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 36)
        explicit_pops <- c(0:36)[-(implicit_pops+1)] 
    }


 selected_scenarios <-  c(
   #"scebaselinenoclosure",
   "scebaseline",
   #"scenbcpcouplingnoclosure",
   "scenbcpcoupling"
   #"scenbcpcouplingspw",
   #"scenbcpcouplingrec"
 )
                                 
labels_selected_scenarios <- c(
  "DISPLACE", 
  "DISPLACE-LGNB" 
  #"Spawning closure", 
  #"Nursery closure"
)                    
                                 


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
   outcome_lasty <- res[res$tstep==35065,]  
   outcome <- merge(outcome_firsty, outcome_lasty, by.x=c('stk', 'sce', 'simu'), by.y=c('stk', 'sce', 'simu'))
   outcome$"F/Finit" <- outcome$Fbar.y/outcome$Fbar.x
   outcome$"SSB/SSBinit" <- outcome$SSB_kg.y/outcome$SSB_kg.x
   
   

   outcome$sce <- factor(outcome$sce)
   outcome$sce <- factor(outcome$sce, levels=selected_scenarios, labels=  labels_selected_scenarios)


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
   p <- ggplot(out[out$stk==2,], aes(x=sce, y=value, fill=var,alpha=var))  + 
     geom_boxplot(outlier.shape=NA)  +
             labs(x = "Scenario", y = "Value")  + facet_wrap( ~ stk, ncol=1, scales="fixed") + 
     #ylim(0, 1) + #For all other scenarios
     ylim(0, 0.45) + # For DISPLACE vs. DISPLACE-LGNB scenario
     scale_fill_manual(values=c("#69b3a2", "grey")) +
     scale_alpha_manual(values=c(0.6,1)) 
     #scale_fill_discrete(name = "", labels = c("F/Finit", "SSB/SSBinit"))
 print(
       p   + 
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
       )

 
dev.off()

# if necessary to add a second y-axis, play with the below:

# adding the relative humidity data, transformed to match roughly the range of the temperature
#  p <- p + geom_line(aes(y = rel_hum/5, colour = "Humidity"))
  
#  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
#  # and, very important, reverting the above transformation
#  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))

 
 
 
 
 
 

  # for a traffic lights table
 #ggplot_build(p)$dat
 
 head(dat)
 dd<- aggregate(outcome_lasty[outcome_lasty$var=="gradva","value"], 
               by=list(
                        #area=outcome_lasty[outcome_lasty$var=="gradva","ICES_area"], 
                       sce=outcome_lasty[outcome_lasty$var=="gradva","sce"]),
                  mean, na.rm=TRUE)
 head(dd)
