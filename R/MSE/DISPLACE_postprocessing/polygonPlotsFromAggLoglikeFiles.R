
#' Produce polygon plots of time series for aggregated loglike indicators
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
#' polygonPlotsFromAggLoglikeFiles (general=general,
#'                                            the_baseline="svana_baseline",
#'                                            a_width=3500,
#'                                            a_height=1000,
#'                                            selected_scenarios=general$namefolderoutput[1:3]
#'                                            )
#'
#'   }




 
polygonPlotsFromAggLoglikeFiles <- function(general=general,
                                            the_baseline="svana_baseline",
                                            a_width=3500,
                                            a_height=1000,
                                            selected_scenarios=general$namefolderoutput[1:3],
                                            the_scenario_names= general$namefolderoutput[1:3]
                                            )
{

   sce1 <- selected_scenarios[1] # init
   
   do_polygon_plot <- function(
                  a_variable="gradva",
                  a_set_of_scenarios=selected_scenarios,
                  the_scenario_names= the_scenario_names, 
                  name_set_of_sces= "setA",
                  selected=selected,
                  export=TRUE,
                  a_xlab="# months",
                  a_ylab="Accumulated Gross Added Value (millions Euro)",
                  add_legend=FALSE,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5),
                              rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2))
                  ) {
 
   sce1   <- a_set_of_scenarios[1]
   sce2   <- a_set_of_scenarios[2]
   sce3   <- a_set_of_scenarios[3]
   if(length(a_set_of_scenarios)>=4) sce4   <- a_set_of_scenarios[4]
   if(length(a_set_of_scenarios)>=5) sce5   <- a_set_of_scenarios[5]
   
   obj1        <- get(paste("lst_loglike_agg_weight", selected, sce1, sep=''), env=.GlobalEnv) 
   obj2        <- get(paste("lst_loglike_agg_weight", selected, sce2, sep=''), env=.GlobalEnv) 
   obj3        <- get(paste("lst_loglike_agg_weight", selected, sce3, sep=''), env=.GlobalEnv) 
   if(length(a_set_of_scenarios)>=4) obj4        <- get(paste("lst_loglike_agg_weight", selected, sce4, sep=''), env=.GlobalEnv) 
   if(length(a_set_of_scenarios)>=5) obj5        <- get(paste("lst_loglike_agg_weight", selected, sce5, sep=''), env=.GlobalEnv) 

  # caution: complete missing records if 0s in given year.month
  complete_all_year_month <- function (x, years_span=2016:2020){ 
      allcombi              <- expand.grid(month=sprintf("%02d", 1:12), year=years_span)
      allcombi$year.month   <- paste0(allcombi$year,".",allcombi$month)
      allcombi              <- cbind.data.frame(year.month=allcombi$year.month,  matrix(0, ncol=ncol(x)-1))
      colnames(allcombi)    <- colnames(x)
      allmissingcombi       <- allcombi[!allcombi$year.month %in% x$year.month,]
      dd <- rbind.data.frame(x, allmissingcombi)
      rownames(dd) <- dd$year.month
      dd <- dd[as.character(allcombi$year.month),] # get the right order...
   return(dd)
  } 
  obj1 <- lapply(obj1, complete_all_year_month)
  obj2 <- lapply(obj2, complete_all_year_month)
  obj3 <- lapply(obj3, complete_all_year_month)
  if(length(a_set_of_scenarios)>=4) obj4        <- lapply(obj4, complete_all_year_month)
  if(length(a_set_of_scenarios)>=5) obj5        <- lapply(obj5, complete_all_year_month)
  
   simu_names <-  names(obj1)
   simu_names <- simu_names[simu_names %in% names(obj2)]
   simu_names <- simu_names[simu_names %in% names(obj3)]
   if(length(a_set_of_scenarios)==4) simu_names <- simu_names[simu_names %in% names(obj4)]
   if(length(a_set_of_scenarios)>=5) simu_names <- simu_names[simu_names %in% names(obj5)]
      
    
    ## a nice quantile plot for profit
    #plot(cumsum(loglike_Scenario1_save[loglike_Scenario1_save$simu=="simu2",]$gradva), type="l", col=2)
    mat_sce_base <- matrix(NA, nrow=length(simu_names), ncol=60) # assuming max 11000 records
    rownames(mat_sce_base) <- simu_names
    mat_sce1 <- matrix(NA, nrow=length(simu_names),  ncol=60)
    rownames(mat_sce1) <- simu_names
    mat_sce2 <- matrix(NA, nrow=length(simu_names),  ncol=60)
    rownames(mat_sce2) <- simu_names
    if(length(a_set_of_scenarios)>=4) mat_sce3 <- matrix(NA, nrow=length(simu_names),  ncol=60)
    if(length(a_set_of_scenarios)>=4) rownames(mat_sce3) <- simu_names
    if(length(a_set_of_scenarios)>=5) mat_sce4 <- matrix(NA, nrow=length(simu_names),  ncol=60)
    if(length(a_set_of_scenarios)>=5) rownames(mat_sce4) <- simu_names
    for (sim in simu_names){


      mat_sce_base[sim, ] <-
                           cumsum(as.numeric(obj1[[sim]][,a_variable]) )[1:dim(mat_sce_base)[2]]
      mat_sce1[sim, ] <-
                           cumsum(as.numeric(obj2[[sim]][,a_variable]) )[1:dim(mat_sce1)[2]]
      mat_sce2[sim, ] <-
                           cumsum(as.numeric(obj3[[sim]][,a_variable]) )[1:dim(mat_sce2)[2]]
      if(length(a_set_of_scenarios)>=4) mat_sce3[sim, ] <-
                           cumsum(as.numeric(obj4[[sim]][,a_variable]) )[1:dim(mat_sce3)[2]]
      if(length(a_set_of_scenarios)>=5) mat_sce4[sim, ] <-
                           cumsum(as.numeric(obj5[[sim]][,a_variable]) )[1:dim(mat_sce4)[2]]
      }


    if(export) tiff(file=file.path(general$main.path, general$case_study, paste("accumulated",a_variable,"perScenario", selected,"_",name_set_of_sces, ".tiff", sep="")), compression="lzw")

    
    sim_ref <- names(which.max (apply(mat_sce_base, 1, function(x) sum(as.numeric(x), na.rm=TRUE))) )
  

    
    er <- try(   {
         
                
    plot(cumsum(as.numeric(obj1[[sim_ref]][,a_variable])) /1e6,  ylim=c(min(cumsum(as.numeric(obj1[[sim_ref]][,a_variable])) /1e6),   max(cumsum(as.numeric(obj1[[sim_ref]][,a_variable])) /1e6)), 
       col=2, type="n", xlab="Nb months", ylab="", cex.lab=1.6, axes=FALSE)
    
 
    polygon(x=c((1:ncol(mat_sce_base))[1:(5*12)], rev((1:ncol(mat_sce_base))[1:(5*12)])),
       y=c(apply(mat_sce_base[,1:(5*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce_base[,1:(5*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])) /1e6,
         col=  color_legend[1], border=NA)   # blue
  
    polygon(x=c((1:ncol(mat_sce1))[1:(5*12)], rev((1:ncol(mat_sce1))[1:(5*12)])),
       y=c(apply(mat_sce1[,1:(5*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce1[,1:(5*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])) /1e6,
         col=  color_legend[2], border=NA)  # green
  
    polygon(x=c((1:ncol(mat_sce2))[1:(5*12)], rev((1:ncol(mat_sce2))[1:(5*12)])),
       y=c(apply(mat_sce2[,1:(5*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce2[,1:(5*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])) /1e6,
         col=  color_legend[3] , border=NA) # red
  
    if(length(a_set_of_scenarios)>=4) polygon(x=c((1:ncol(mat_sce3))[1:(5*12)], rev((1:ncol(mat_sce3))[1:(5*12)])),
       y=c(apply(mat_sce3[,1:(5*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce3[,1:(5*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])) /1e6,
         col=  color_legend[4], border=NA) # grey
 
    if(length(a_set_of_scenarios)>=5) polygon(x=c((1:ncol(mat_sce4))[1:(5*12)], rev((1:ncol(mat_sce4))[1:(5*12)])),
       y=c(apply(mat_sce4[,1:(5*12)], 2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)["5%",], rev(apply(mat_sce4[,1:(5*12)], 2, quantile,  probs=c(0.05, 0.95), na.rm=TRUE)["95%",])) /1e6,
         col=  color_legend[5], border=NA) # grey

    axis(2, las=2, cex.axis=1.5)
    axis(1, cex.axis=1.5)
    if(add_legend) legend("topleft", fill=color_legend, border =color_legend, legend=the_scenario_names, cex=1.3, bty="n")
    box()

    abline(h=0, lty=2, col=grey(0.9))
   
    mtext(side = 2, text = a_ylab, line = 3.6, cex=1.2)
   
    }, silent=TRUE)

    if(class(er)=="try-error"){
           print(paste("no data."))

    }

      
 
  
    if(export) dev.off()
   

 
    return()
    }

   
  
   var_names <- colnames(get(paste("lst_loglike_agg_weight_","selected_set1_",sce1, sep=''), env=.GlobalEnv)[[1]])
   dir.create(file.path(general$main.path, general$namefolderinput,"polygon_plots"))
   
   for (a_var in var_names[-1]){
   
   graphics.off()
    tiff(file=file.path(general$main.path, general$namefolderinput, "polygon_plots", paste("accumulated_per_scenario_polygon_", a_var, ".tiff", sep="")), 
                                  width = a_width, height = a_height,   compression="lzw",
                                   units = "px", pointsize = 12,  res=300)
   par(mfrow=c(1,3))
   par(mar=c(2,4.4,2,2))
   par(oma=c(4,4,1,1))


   cat (paste0("plot for ", a_var,"\n"))
   
  do_polygon_plot(
                  a_variable=a_var,                 
                  a_set_of_scenarios=selected_scenarios,
                  the_scenario_names= the_scenario_names, 
                  name_set_of_sces= "setA",
                  selected="_selected_set1_",
                  export=FALSE,
                  a_xlab="# months",
                  if(a_var=="gradva") a_ylab="Acc. GVA (mio Euro)" else a_ylab=a_var,
                  add_legend=TRUE,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2))              
                  )
   cat (paste0("plot for ", a_var," for set1...ok\n"))
   
 do_polygon_plot(
                  a_variable=a_var,                 
                  a_set_of_scenarios=selected_scenarios,
                  the_scenario_names= the_scenario_names, 
                  name_set_of_sces= "setA",
                  selected="_selected_set2_",
                  export=FALSE,
                  a_xlab="# months",
                  if(a_var=="gradva") a_ylab="Acc. GVA (mio Euro)" else a_ylab=a_var,
                  add_legend=TRUE,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2))              
                  )
   cat (paste0("plot for ", a_var," for set2...ok\n"))
   
 do_polygon_plot(
                  a_variable=a_var,                 
                  a_set_of_scenarios=selected_scenarios,
                  the_scenario_names= the_scenario_names, 
                  name_set_of_sces= "setA",
                  selected="_selected_set3_",
                  export=FALSE,
                  a_xlab="# months",
                  if(a_var=="gradva") a_ylab="Acc. GVA (mio Euro)" else a_ylab=a_var,
                  add_legend=TRUE,
                  color_legend= c(rgb(94/255,79/255,162/255,0.5), rgb (158/255,1/255,66/255,0.5), rgb(140/255,81/255,10/255,0.4), rgb(1,0,0,0.5), rgb(0,0.5,1,0.5), rgb(0,1,0.5,0.5), rgb(1,0,0.5,0.5), rgb(0,0,0,0.2))              
                  )
    cat (paste0("plot for ", a_var," for set3...ok\n"))
  
 
  mtext("# months", 1, line=2, cex=1.5, outer=TRUE)
  mtext(side=2,"Indicators",line=2, cex=1.5, outer=TRUE)

  
  
  dev.off()

  } # end a_var
  
  
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



the_baseline <- "scebaselinenoclosure"

polygonPlotsFromAggLoglikeFiles (general=general,
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
                                 )
                                            )

