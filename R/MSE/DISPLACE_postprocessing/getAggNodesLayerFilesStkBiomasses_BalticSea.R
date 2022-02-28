

            
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








getAggNodeLayerFilesStkBiomasses <- function(general, a_type="inc", a_tstep="34321", a_pop=1, nbstks=37){


  for (sce in general$namefolderoutput){


   print(sce)
 

 
    ## total biomasses on node per stock-------------------------------
    alllayers <- NULL


     for (sim in general$namesimu[[sce]]){

         er <- try(   {
            obj <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                                paste("popnodes_",a_type,"_", sim, ".dat", sep='')))
            if(a_type=="inc"){  # stkbiomasses_per_pop
               colnames (obj) <- c('tstep', 'idx_node', 'long', 'lat',
                   paste(rep((0:(nbstks-1)), each=1*2), rep(c(paste0('N', 0), paste0('B', 0)), nbstks), sep="_"))
               if(!a_pop %in% unique(1:nbstks)){
                print(paste("no field for ", a_pop))          
                return()
                }                                   
               obj    <- obj[obj$tstep==a_tstep, ] 
               obj    <- obj[c("idx_node",'lat','long',  paste(a_pop, paste0('B', 0), sep="_"))]
               colnames(obj)[ncol(obj)] <- a_type # rename
               a_popname <- paste0("_pop", a_pop)
             } else{
               stop("inc not found")
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
     print(paste(paste(general$main.path.ibm, sep=""),"graphsspe", paste("coord", general$igraph, ".dat", sep="")))
                          
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
                              paste("average_",a_type,"_layer",a_popname,".txt", sep='')), row.names=FALSE, quote=FALSE)

   
 
    }


return()
}


##-------------------------------------------------------------------##
##-------------------CALLS-------------------------------------------##

                                                                                     
if(TRUE){
 # GENERAL SETTINGS
  general <- list()

  general$case_study <- "BalticSea"

 if(.Platform$OS.type == "unix") {}
  #   general$main.path         <- file.path("~","ibm_vessels","DISPLACE_outputs")                                                                                                   
  #   general$main.path.igraph  <- file.path("~","ibm_vessels","DISPLACE_input_raw", "igraph")
  #   general$main.path.param   <- file.path("~","ibm_vessels", paste("DISPLACE_input_",general$case_study, sep=""))
  #   general$main.path.ibm     <- file.path("~","ibm_vessels", paste("DISPLACE_input_", general$case_study, sep='')) 
  general$main.path         <- file.path("~","DISPLACE_outputs")                                                                                                   
  general$main.path.igraph  <- file.path("~","DISPLACE_input_raw", "igraph")
  general$main.path.param   <- file.path("~",paste("DISPLACE_input_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("~",paste("DISPLACE_input_", general$case_study, sep='')) 
  # do not forget to install the R packages on the qrsh interactive node linux platform, i.e. R > install.packages("data.table"), etc.
  # (and possibly kill the current jobs on HPC with the command qselect -u $USER | xargs qdel)
  # submit the shell to HPC with the command qsub ./IBM_processoutput_plots_for_loglike.sh
  
 if(.Platform$OS.type == "windows") {
  general$main.path         <- file.path("C:","DISPLACE_outputs")                                                                                                   
  general$main.path.igraph  <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_",general$case_study, sep=""), "graphsspe")
  general$main.path.param   <- file.path("C:","Users","fbas","Documents","GitHub",paste("DISPLACE_input_gis_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("C:","Users","fbas","Documents","GitHub", paste("DISPLACE_input_", general$case_study, sep='')) 
 }

 if(general$case_study=="BalticSea"){
    general$igraph            <- 100
    general$a.year            <- "2016"
    general$a.country         <- c("DEU", "DNK", "EST", "FIN", "LTU", "LVA", "POL", "SWE")
    general$nbpops            <- 37  
    general$nbszgroup         <- 14
    general$namefolderinput   <- "BalticSea"
    general$use_sqlite        <- FALSE
  
    general$namefolderoutput   <- c(
                                 "scebaseline",
				 "scenbcpcoupling",
				 "scenbcpcouplingrec",
				 "scenbcpcouplingspw"
				 #"scenbcpcouplingraw" #This is the baseline without any fishing closures    
                                 )

     general$namesimu           <- list(
                                 "scebaseline"      	 =   paste("simu", c(1:50), sep=''), 
				 "scenbcpcoupling"   	 =   paste("simu", c(1:50), sep=''), 
				 "scenbcpcouplingrec"	 =   paste("simu", c(1:50), sep=''), 
				 "scenbcpcouplingspw"	 =   paste("simu", c(1:50), sep='')
                           	 #"scenbcpcouplingraw"    =   paste("simu", c(1:50), sep='')       
                                 )


     the_scenarios1 <-  c("Baseline DISPLACE", 
                       	"Baseline LGNB-DISPLACE", 
			"Spawning closure", 
			"Nursery closures"
			#"No closures"
                          )

# FANCOIS original input
    #general$igraphs   <- c(
			    #"scebaseline"=100,
                                 #"scerestrictionsonnets"=101,
                                 #"scerestrictionontrawling5eez"=102,
                                 #"scerestrictionontrawling10eez"=103,
                                 #"scerestrictionontrawling15eez"=104,
                                 #"scerestrictionontrawling20eez"=105,
                                 #"scerestrictionontrawling5hab"=106,
                                 #"scerestrictionontrawling10hab"=107,
                                 #"scerestrictionontrawling15hab"=108,
                                 #"scerestrictionontrawling20hab"=109,
                                 ##"scerestrictionsonnetsandtrawl15hab"=112,
                                 #"scerestrictionsonnetsandtrawl15eez"=113,
                                 #"scerestrictionsonnetsandtrawl20hab"=110,
                                 #"scerestrictionsonnetsandtrawl20eez"=111,
                                 #"scerestrictionontrawling25eez"=114,
                                 #"scerestrictionontrawling25hab"=115,
                                 #"scerestrictionontrawling30eez"=116,
                                 #"scerestrictionontrawling30hab"=117,
                                 #"scerestrictionsonnetsandtrawl25eez"=118,
                                 #"scerestrictionsonnetsandtrawl25hab"=119,
                                 #"scerestrictionsonnetsandtrawl30eez"=120,
                                 #"scerestrictionsonnetsandtrawl30hab"=121,
                                 #"scerestrictionontrawling50eez"=122,
                                 #"scerestrictionontrawling50hab"=123,
                                 #"scerestrictionontrawling1eez"=124,
                                 #"scerestrictionontrawling1hab"=125,
                                 #"scerestrictionontrawling30eezH"=126,
                                 #"scerestrictionontrawling50eezH"=127,
                                 #"scerestrictionontrawling10eez10lesstrip"=103,                                
                                 #"scerestrictionontrawling20eez20lesstrip"=105,                                
                                 #"scerestrictionontrawling30eez30lesstrip"=116
                                 #)
 


# My input - indicate the right graphs here!!!
    general$igraphs   <- c(
			   "scebaseline"        = 100, 
		           "scenbcpcoupling"    = 100,
			   "scenbcpcouplingrec" = 202,
			   "scenbcpcouplingspw" = 201
 			   #"scenbcpcouplingraw" = 203
                                 )
 


   }


} # end FALSE
 
 
 
 
 implicit_pops <- c(0, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 36)
 explicit_pops <- c(0:36)[-(implicit_pops+1)] 
   
#----------
for(pop in c("35")){ # in explicit_pops){
   getAggNodeLayerFilesStkBiomasses (general, a_type="inc", a_tstep="34321", a_pop = pop, nbstks=37)
   cat(paste("stock biomasses for pop",pop,"..ok/n"))
}

