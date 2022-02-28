
#' Produce polygon plots of time series for annual indicators
#'
#' This function produces all accumulated time series over month to compare scenario outcomes
#' All the plots are being stored in a polygons_plots folder that can further be found in the output folder.
#' (compare up to 5 scenarios simultaneously)
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' \dontrun{
#' general <- setGeneralOverallVariable(main_path_outputs =file.path("C:","DISPLACE_outputs"),
#'                                       case_study="DanishFleet",
#'                                       igraph=41,
#'                                       a.year="2015",
#'                                       a.country="DEN",
#'                                       nbpops=39,
#'                                       nbszgroup=14,
#'                                       namefolderinput="DanishFleet",
#'                                       the_scenarios= c("svana_baseline",
#'                                                       "svana_sub1mx20",
#'                                                       "svana_sub4mx20",
#'                                                       "svana_sub4mx5ns20bt",
#'                                                       "svana_sub4mx20ns5bt",
#'                                                       "svana_sub4mx5ns5bt" ),
#'                                       nbsimus=20
#'                                       )
#'
#'
#' loadLoglikeFiles(general=general, use_port_info=FALSE)
#' 
#' 
#' 
#' polygonPlotsFromAnnualIndicFiles (general=general,
#'                                          a_variable="Fbar",  # could be "Fbar" or "totland" or "tac"
#'                                            the_baseline="svana_baseline",
#'                                            a_width=3500,
#'                                            a_height=1000,
#'                                            selected_scenarios=general$namefolderoutput[1:3],
#'                                            explicit_pops=c(0, 1, 2, 3, 11, 23, 24, 26, 30, 31, 32),
#'                                            is_individual_export=TRUE
#'                                            )
#'
#'   }



 
polygonPlotsFromPopStatsFiles <- function(general=general,
                                            a_variable="Fbar",
                                             nby=5,
                                            the_baseline="svana_baseline",
                                            a_width=3500,
                                            a_height=1000,
                                            selected_scenarios=general$namefolderoutput[1:3],
                                            the_scenario_names=general$namefolderoutput[1:3], 
                                            explicit_pops=c(0, 1, 2, 3, 11, 23, 24, 26, 30, 31, 32),
                                            is_individual_export=TRUE,
                                            add_legend=FALSE
                                            )
{

                                                
    do_polygon_plot_pop_stats <- function(
                  a_variable="Fbar",
                  nby=5,
                  a_stock=0,
                  a_stockname="0",
                  selected_scenarios=general$namefolderoutput[1:3],
                  the_scenario_names=general$namefolderoutput[1:3], 
                  name_set_of_sces= "setA",
                  selected=selected,
                  export=TRUE,
                  a_xlab="# Month",
                  a_ylab="F",               
                  add_legend=FALSE,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4),
                              rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
                  the_sims=1:20,
                   width = 3500, height = 1000
                  ) {
 

  
    # look at annual indics such as the TACs...
    res <- NULL
    for(sce in selected_scenarios) {
            print(paste("sce ", sce))
     

         for(i in the_sims) {
            print(paste("sim ", i))
         # merge all infos
               pop_stats              <-  read.table (file=file.path(general$main.path, general$namefolderinput, sce, paste('popstats_simu',  i,".dat", sep='')))
               colnames(pop_stats)    <-  c("tstep", "stk", paste0("N",0:13), paste0("W",0:13), paste0("SSB",0:13))

               agg_pop_stats <- cbind.data.frame(pop_stats[, 1:2],
                                              totN=apply(pop_stats[,grep("N", colnames(pop_stats))], 1, sum),
                                              meanW=apply(pop_stats[,grep("SSB", colnames(pop_stats))], 1, mean),
                                              totSSB=apply(pop_stats[,grep("SSB", colnames(pop_stats))], 1, sum)
                                              )
            res <- rbind (res, cbind(agg_pop_stats, sce=sce, simu=paste("simu", i, sep="_")))
             }
      }
      res <- res[res$stk==a_stock,]
       


    sce1   <- selected_scenarios[1]
    sce2   <- selected_scenarios[2]
    sce3   <- selected_scenarios[3]
    if(length(selected_scenarios)>=4) sce4   <- selected_scenarios[4]
    if(length(selected_scenarios)>=5) sce5   <- selected_scenarios[5]
   
    obj1        <- res[res$sce==sce1,]
    obj2        <- res[res$sce==sce2,]
    obj3        <- res[res$sce==sce3,]
    if(length(selected_scenarios)>=4) obj4        <- res[res$sce==sce4,]
    if(length(selected_scenarios)>=5) obj5        <- res[res$sce==sce5,]
   
 
    simu_names <-  the_sims
     
    ## a nice quantile plot 
    mat_sce_base <- matrix(NA, nrow=max(the_sims), ncol=length( unique(obj1[,"tstep"]) ) ) # assuming max 11000 records
    rownames(mat_sce_base) <- 1:max(the_sims)
    mat_sce1 <- matrix(NA, nrow=max(the_sims),  ncol=length( unique(obj1[,"tstep"]) ))
    rownames(mat_sce1) <- 1:max(the_sims)
    mat_sce2 <- matrix(NA, nrow=max(the_sims),  ncol=length( unique(obj1[,"tstep"]) ))
    rownames(mat_sce2) <- 1:max(the_sims)
    if(length(selected_scenarios)>=4) mat_sce3 <- matrix(NA, nrow=max(the_sims),  ncol=length( unique(obj1[,"tstep"]) ))
    if(length(selected_scenarios)>=4) rownames(mat_sce3) <- 1:max(the_sims)
    if(length(selected_scenarios)>=5) mat_sce4 <- matrix(NA, nrow=max(the_sims),  ncol=length( unique(obj1[,"tstep"]) ))
    if(length(selected_scenarios)>=5) rownames(mat_sce4) <- 1:max(the_sims)
    for (sim in simu_names){

      dat <- as.numeric(obj1[obj1$simu==paste("simu_", sim, sep=''),a_variable])
      mat_sce_base[sim, ] <-      dat
                        
      dat <-  as.numeric(obj2[obj2$simu==paste("simu_", sim, sep=''),a_variable]) 
      mat_sce1[sim, ] <-      dat
                          
      dat <-  as.numeric(obj3[obj3$simu==paste("simu_", sim, sep=''),a_variable])
      mat_sce2[sim, ] <-        dat
                            
    
      if(length(selected_scenarios)>=4) {
      dat <-  as.numeric(obj4[obj4$simu==paste("simu_", sim, sep=''),a_variable]) 
      mat_sce3[sim, ] <-        dat
      }
                          
      if(length(selected_scenarios)>=5){
      dat <-   as.numeric(obj5[obj5$simu==paste("simu_", sim, sep=''),a_variable]) 
       mat_sce4[sim, ] <-      dat
      }
    }


    if(export) tiff(file=file.path(general$main.path, general$namefolderinput, "polygon_plots",
                    paste("", a_variable,"perScenario_",a_stockname, "_", name_set_of_sces, ".tiff", sep="")), 
                     width = a_width, height = a_height, compression="lzw")
   
    sim_ref <- names(which.max (apply(mat_sce_base, 1, function(x) sum(as.numeric(x), na.rm=TRUE))) )
   
   er <- try(   {

   par(mfrow=c(1,1))
   par(mar=c(4,4.4,2,2))
   par(oma=c(3,5,1,1))
         
   plot(mat_sce1[sim_ref, ],  
           ylim=c(0,   max(as.numeric(obj1[obj1$simu==paste("simu_",sim_ref, sep=''),a_variable]))),    
        #   ylim=c(0,   1.2), 
       col=2, type="n", xlab=a_xlab, ylab="", cex.lab=1.6, axes=FALSE)
      
   
      polygon(x=c((1:ncol(mat_sce_base))[1:(nby*12)], rev((1:ncol(mat_sce_base))[1:(nby*12)])),
       y=c(apply(mat_sce_base[,1:(nby*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce_base[,1:(nby*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])),
         col= color_legend[1], border=NA)   # blue

       polygon(x=c((1:ncol(mat_sce1))[1:(nby*12)], rev((1:ncol(mat_sce1))[1:(nby*12)])),
       y=c(apply(mat_sce1[,1:(nby*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["95%",], rev(apply(mat_sce1[,1:(nby*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["5%",])),
         col=  color_legend[2], border=NA)  # green

       polygon(x=c((1:ncol(mat_sce2))[1:(nby*12)], rev((1:ncol(mat_sce2))[1:(nby*12)])),
       y=c(apply(mat_sce2[,1:(nby*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce2[,1:(nby*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])),
         col=   color_legend[3], border=NA) # red
         
       if(length(selected_scenarios)>=4) polygon(x=c((1:ncol(mat_sce3))[1:(nby*12)], rev((1:ncol(mat_sce3))[1:(nby*12)])),
       y=c(apply(mat_sce3[,1:(nby*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce3[,1:(nby*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])) ,
         col= color_legend[4], border=NA) # grey
 
       if(length(selected_scenarios)>=5) polygon(x=c((1:ncol(mat_sce4))[1:(nby*12)], rev((1:ncol(mat_sce4))[1:(nby*12)])),
       y=c(apply(mat_sce4[,1:(nby*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce4[,1:(nby*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])) ,
         col=  color_legend[5], border=NA) # grey
    

    axis(2, las=2, cex.axis=1.5)
    axis(1, at=1:(nby*12), labels=seq(1, 12*nby, by=1), cex.axis=1.5)
    if(add_legend) legend("topleft", fill=color_legend, border =color_legend, legend=selected_scenarios, cex=1.3, bty="n")
    box()
  
    if(a_ylab=="F"){
     mtext(side = 2, text = substitute( paste(italic('F'), a_stockname)), line = 3.6, cex=1.2)
    } else{
     mtext(side = 2, text = paste(a_ylab, a_stockname), line = 5.6, cex=1.5)
    }
    
    abline(h=0, lty=2, col=grey(0.9))
 
   }, silent=TRUE)

   if(class(er)=="try-error"){
           print(paste("no data."))

   }
   
   if(export) dev.off()
  
 
   return()
   }

   


   #---------------------
   dir.create(file.path(general$main.path, general$namefolderinput,"polygon_plots"))
   
   
   graphics.off()
    tiff(file=file.path(general$main.path, general$namefolderinput, "polygon_plots", paste("accumulated_per_scenario_polygon_", a_variable, ".tiff", sep="")), 
                                  width = a_width, height = a_height,   compression="lzw",
                                   units = "px", pointsize = 12,  res=300)
   par(mfrow=c(1,1))
   par(mar=c(2,5.4,2,2))
   par(oma=c(4,5,1,1))

   for (a_stock in explicit_pops){
  
   do_polygon_plot_pop_stats (
                  a_variable=a_variable,
                   nby=nby,
                  a_stock=a_stock,
                  a_stockname=a_stock,
                  selected_scenarios=selected_scenarios,
                  the_scenario_names=the_scenario_names, 
                  name_set_of_sces= "setA",
                  selected=selected,
                  export=is_individual_export,
                  a_xlab="# Month",
                  if(a_variable=="totN") {a_ylab="Total N" } else{  if(a_variable=="meanW"){ a_ylab= "Mean Weight (kg)"} else{  if(a_variable=="totSSB"){ a_ylab= "SSB (kg)"} else a_ylab=a_variable}},
                  add_legend=add_legend,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4),
                              rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2)),
                  the_sims=1:length(general$namesimu[[1]]),
                  width = a_width,
                  height = a_height
                  ) 
   
 
  if(!is_individual_export) mtext("# months", 1, line=2, cex=1.5, outer=TRUE)
  if(!is_individual_export) mtext(side=2,"Indicators",line=2, cex=1.5, outer=TRUE)

  
  
  if(!is_individual_export) dev.off()

  } 
 
  
return()
}  
 
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!CALLS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

                                                                                     
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
                                        "scebaseline"       	           =   paste("simu", c(1:50), sep=''),
                                        "scenbcpcouplingnoclosure"   	 =   paste("simu", c(1:50), sep=''), 
                                        "scenbcpcoupling"   	           =   paste("simu", c(1:50), sep=''), 
                                        "scenbcpcouplingspw"	           =   paste("simu", c(1:50), sep=''), 
                                        "scenbcpcouplingrec"	           =   paste("simu", c(1:50), sep='')
                                        
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

#source(file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis_BalticSea","DISPLACE_R_outputs_ForBalticSea","loadAggLoglikeFiles.R"))
source(file.path("C:","Users","mruf","Documents","PHD_projects","Proj_3","DISPLACE_input_gis_BalticSea","DISPLACE_R_outputs_ForBalticSea","loadAggLoglikeFiles.R"))

loadLoglikeFiles(general=general, use_port_info=FALSE) 

implicit_pops <- c(0, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 36)
explicit_pops <- c(0:36)[-(implicit_pops+1)] 
 

#!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#
the_baseline <- "scebaselinenoclosure"
#!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#
#!#!#!#!#!#!#!#
#explicit_pops <- c(1,2,11,12,22,23,31,35) ##original one
explicit_pops <- 2 #for WBS


polygonPlotsFromPopStatsFiles (general=general,
                                          a_variable="totN",  
                                            nby=5,
                                            the_baseline=the_baseline,
                                            a_width=3500,
                                            a_height=1000,
                                             selected_scenarios= c(
                                               "scebaselinenoclosure",
                                               "scebaseline",
                                               "scenbcpcouplingnoclosure",
                                               "scenbcpcoupling",
                                               "scenbcpcouplingspw",
                                               "scenbcpcouplingrec"
                                             ),
                                            the_scenario_names=c(
                                              "Baseline without closure", 
                                              "Baseline", 
                                              "DISPLACE-LGNB without closure", 
                                              "DISPLACE-LGNB with closure",
                                              "Spawning closure", 
                                              "Nursery closures"
                                            ),
                                           explicit_pops=explicit_pops,
                                            is_individual_export=TRUE,
                                            add_legend=TRUE
                                            )
                                            
                                            
polygonPlotsFromPopStatsFiles (general=general,
                                          a_variable="meanW", 
                                          nby=5,
                                            the_baseline=the_baseline,
                                            a_width=3500,
                                            a_height=1000,
                               selected_scenarios= c(
                                 "scebaselinenoclosure",
                                 "scebaseline",
                                 "scenbcpcouplingnoclosure",
                                 "scenbcpcoupling",
                                 "scenbcpcouplingspw",
                                 "scenbcpcouplingrec"
                               ),
                               the_scenario_names=c(
                                 "Baseline without closure", 
                                 "Baseline", 
                                 "DISPLACE-LGNB without closure", 
                                 "DISPLACE-LGNB with closure",
                                 "Spawning closure", 
                                 "Nursery closures"
                               ),
                                           explicit_pops=explicit_pops,
                                            is_individual_export=TRUE,
                                            add_legend=TRUE
                                            )

polygonPlotsFromPopStatsFiles (general=general,
                                          a_variable="totSSB", 
                                          nby=5,
                                            the_baseline=the_baseline,
                                            a_width=3500,
                                            a_height=1000,
                               selected_scenarios= c(
                                 "scebaselinenoclosure",
                                 "scebaseline",
                                 "scenbcpcouplingnoclosure",
                                 "scenbcpcoupling",
                                 "scenbcpcouplingspw",
                                 "scenbcpcouplingrec"
                               ),
                               the_scenario_names=c(
                                 "Baseline without closure", 
                                 "Baseline", 
                                 "DISPLACE-LGNB without closure", 
                                 "DISPLACE-LGNB with closure",
                                 "Spawning closure", 
                                 "Nursery closures"
                               ),
                                           explicit_pops=explicit_pops,
                                            is_individual_export=TRUE,
                                            add_legend=TRUE
                                            )

  