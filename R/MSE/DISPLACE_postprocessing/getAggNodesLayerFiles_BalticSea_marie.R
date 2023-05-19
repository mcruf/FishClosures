


#' Produce an average spatial layer as first step to do a map 
#'
#' This function generates an average layer as a first step to generate maps from popnodes files 
#'
#' @param fname First name
#' @param lname Last name
#' @export
#' @examples
#' 
#' \dontrun{
#' general <- setGeneralOverallVariable (main_path_outputs =file.path("C:","DISPLACE_outputs"),
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
#' getAggNodeLayerFiles (general, a_type="cumcatches", a_tstep="34321")
#'  #=> produce files in output folders....
#'  }








getAggNodeLayerFiles <- function(general, a_type="cumcatches", a_tstep="34321", a_pop="", a_comment=""){
  
  
  for (sce in general$namefolderoutput){
    
    
    print(sce)
    
    
    
    ## catches-------------------------------
    alllayers <- NULL
    
    
    for (sim in general$namesimu[[sce]]){
      
      er <- try(   {
        obj <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                         paste("popnodes_",a_type,"_", sim, ".dat", sep='')))
        if(a_type=="cumulcatches_per_pop" || a_type=="cumuldiscards_per_pop"|| a_type=="cumftime_per_pop"){
          colnames (obj) <- c('pop', 'tstep', 'idx_node', 'long', 'lat', a_type)
          if(!a_pop %in% unique(obj$pop)){
            print(paste("no catch field for ", a_pop))          
            return()
          }                                   
          obj    <- obj[obj$pop== a_pop & obj$tstep==a_tstep,] 
          obj    <- obj[c("idx_node",'lat','long', a_type)]
          a_popname <- paste0("_pop", a_pop)
        } else{
          colnames (obj) <- c('tstep', 'idx_node', 'long', 'lat', a_type)
          obj    <- obj[obj$tstep==a_tstep,] # e.g. if "34321" then cumul at 1st of Dec 4th year
          obj    <- obj[c("idx_node",'lat','long', a_type)]
          a_popname <- ""
        }
        obj    <- cbind(obj, simu=sim)
        print(head(obj))
        alllayers <- rbind(alllayers, obj)
      }, silent=TRUE)
      
      if(class(er)=="try-error"){
        print(paste("no simu", sim))
        
      }
      
    }
    
    # CAUTION:
    # read graph coord and complete DISPLACE output files with all coords for image() to work properly
    coord <- read.table(file=file.path(paste(general$main.path.ibm, sep=""),
                                       "graphsspe", paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
    coord <- as.matrix(as.vector(coord))
    coord <- matrix(coord, ncol=3)
    colnames(coord) <- c('SI_LONG', 'SI_LATI', 'idx.port')
    # hereafter:
    allgraphpts <-  data.frame(idx_node=0, lat=coord[,2], long=coord[,1], a_type=NA, simu="simu2") 
    colnames(allgraphpts) [colnames(allgraphpts)%in%  "a_type"] <- a_type
    alllayers <- rbind(alllayers, allgraphpts)
    
    
    alllayersav <- tapply(as.numeric(as.character(alllayers[,a_type])), list(paste(alllayers$idx_node, alllayers$lat, alllayers$long))
                          , mean, na.rm=TRUE) # average over simus
    alllayersav <- cbind.data.frame(node=names(alllayersav), avcum=alllayersav)
    
    write.table(alllayersav, file=file.path(general$main.path, general$namefolderinput, sce,
                                            paste("average_",a_type,"_layer",a_popname,a_comment,".txt", sep='')), row.names=FALSE, quote=FALSE)
    
    
    
  }
  
  
  return()
}


##-------------------------------------------------------------------##
##-------------------CALLS-------------------------------------------##



if(TRUE){
  # GENERAL SETTINGS
  general <- list()
  
  general$case_study <- "BalticSea" #change name according to application (e.g., myfish)
  
  
  general$main.path         <- file.path("~","Desktop","Review_FR", "R2","Results", "DISPLACE", "DISPLACE_outputs")   
  general$main.path.igraph  <- file.path("~","Desktop","WBSsimu", paste("DISPLACE_input_",general$case_study, sep=""), "graphsspe")
  general$main.path.param   <- file.path("~","Desktop","WBSsimu", paste("DISPLACE_input_gis_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("~","Desktop","WBSsimu", paste("DISPLACE_input_", general$case_study, sep=''))
  
  # do not forget to install the R packages on the qrsh interactive node linux platform, i.e. R > install.packages("data.table"), etc.
  # (and possibly kill the current jobs on HPC with the command qselect -u $USER | xargs qdel)
  # submit the shell to HPC with the command qsub ./IBM_processoutput_plots_for_loglike.sh
  
  
  if(general$case_study=="BalticSea"){
    general$igraph            <- 100
    general$a.year            <- "2016"
    general$a.country         <- c("DEU", "DNK", "EST", "FIN", "LTU", "LVA", "POL", "SWE")
    general$nbpops            <- 37  
    general$nbszgroup         <- 14
    general$namefolderinput   <- "BalticSea"
    general$use_sqlite        <- FALSE
    
    
    general$namefolderoutput  <- c("scelgnbcouplingnoclosure",
                                   "scelgnbcouplingwclosure",
                                   "scelgnbcouplingspwclosure",
                                   "scelgnbcouplingoldspwclosure",
                                   "scelgnbcouplingnurseclosure",
                                   "scelgnbcouplingfeedclosure"
    ) 
    
    
    general$namesimu           <- list("scelgnbcouplingnoclosure"  =   paste("simu", c(1:20), sep=''),
                                       "scelgnbcouplingwclosure" =   paste("simu", c(1:20), sep=''),
                                       "scelgnbcouplingspwclosure"  =   paste("simu", c(1:20), sep=''),
                                       "scelgnbcouplingoldspwclosure" =   paste("simu", c(1:20), sep=''),
                                       "scelgnbcouplingnurseclosure"  =   paste("simu", c(1:20), sep=''),
                                       "scelgnbcouplingfeedclosure"  =   paste("simu", c(1:20), sep='')
                                       
    ) 
    
    
    the_scenarios1 <-  c("Baseline",
                         "Seasonal Spawning Closure",
                         "Spawning Area Closure",
                         "Old Spawner Area Closure",
                         "Nursery Area Closure",
                         "Feeding Area Closure"
    )
    
  }
  
  
} # end FALSE



#implicit_pops <- c(0, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 36)
# explicit_pops <- c(0:36)[-(implicit_pops+1)] 

#----------
for(pop in c(0:( general$nbpops -1 ))){
  getAggNodeLayerFiles (general, a_type="cumulcatches_per_pop", a_tstep="34321", a_pop = pop)
  cat(paste("cumcatches for pop",pop,"..ok/n"))
}



for(pop in c(0:( general$nbpops -1 ))){
  getAggNodeLayerFiles (general, a_type="cumcatches_per_pop", a_tstep="34321", a_pop = pop)
  cat(paste("cumcatches for pop",pop,"..ok/n"))
}



for(pop in c(0:( general$nbpops -1 ))){
  getAggNodeLayerFiles (general, a_type="cumftime", a_tstep="34321", a_pop = pop)
  cat(paste("cumftime for pop",pop,"..ok/n"))
}


for(pop in c(0:( general$nbpops -1 ))){
  getAggNodeLayerFiles (general, a_type="cumdiscards_per_pop", a_tstep="34321", a_pop = pop)
  cat(paste("cumdiscards for pop",pop,"..ok/n"))
}






getAggNodeLayerFiles (general, a_type="cumcatches", a_tstep="34321")
cat("cumcatches...ok/n")
#getAggNodeLayerFiles (general, a_type="cumsweptarea", a_tstep="8761", a_comment="_firsty")
#cat("cumsweptarea...ok/n")
#getAggNodeLayerFiles (general, a_type="cumsweptarea", a_tstep="34321")
#cat("cumsweptarea...ok/n")
getAggNodeLayerFiles (general, a_type="cumftime", a_tstep="34321")
cat("cumftime...ok/n")
getAggNodeLayerFiles (general, a_type="cumdiscards", a_tstep="34321")
cat("cumdiscards...ok/n")
