
#####################################################################################
#                                                                                   #
#        Process the simulation outputs to generate the loglike files               #
#                                                                                   #
#####################################################################################

# This is the very first script that needs to be run after running the DISPLACE scenario-specific simulations.
# This script basically processes the simulation output such that the "loglike" files are retrieved.
# These files are then used as input in other scripts (e.g., mapping script, biomass dynamics over time, indicators)

# Note: The following script also generates the boxplots figures in which the different indicators are dispalyed relative to the baseline scenario.


# The loglike file stores all results related to the biological, behavioral and economic indicators.
# These files mimick the logbook files fromat - hence, 'loglike'


# The script is divided into four parts:

# 1) Specify general settings
# 2) Functions to process DISPLACE simulations outputs
# 3)...
# 4) Plots


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# These are used in all other post-processing scripts, and therefore extra 
# care needs to be set with the ordering of the scenarios.

# NOTE;
## Some simulations provided very unrealistic patterns, where the stock collapsed
## suddenly at a random time even when it showed an increased trend. This can be
## seen by plotting the SSB along time (see SSB_along_time.R script). We have to remove
## these simulations to ensure unbiased results.
## Simulations to remove:
## - Seasonal spawning closure - simu 14 and 17
## - Spawning area closure - simu 14
## - Feeding closure - simu 7




#~~~~~~~~~~~~~~~~~~~~
# 1) GENERAL SETTINGS
#~~~~~~~~~~~~~~~~~~~~

general <- list()
general$case_study <- "BalticSea" #change name according to application (e.g., myfish)


## Specify directories to DISPLACE application & simulation outputs  
if(.Platform$OS.type == "unix") {
  
  
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
  general$namefolderoutput  <- c("scelgnbcouplingnoclosure",
                                 "scelgnbcouplingwclosure",
                                 "scelgnbcouplingspwclosure",
                                 "scelgnbcouplingoldspwclosure",
                                 "scelgnbcouplingnurseclosure",
                                 "scelgnbcouplingfeedclosure"
  ) 
  
  ### Specify the names of the simulations & with the number of simulations that were conducted
  general$namesimu  <- list("scelgnbcouplingnoclosure"     =   paste("simu", c(1:20), sep=''), # Baseline
                            #"scelgnbcouplingwclosure"      =   paste("simu", c(1:20), sep=''), # Seasonal spawning closure
                            "scelgnbcouplingwclosure"      =   paste("simu", c(1:13,15:16,18:20), sep=''), # Seasonal spawning closure -- remove odd simulation (n.14 & 17)
                            "scelgnbcouplingspwclosure"    =   paste("simu", c(1:13,15:20), sep=''), # Spawning area closure -- remove odd simulation (no. 14)
                            "scelgnbcouplingoldspwclosure" =   paste("simu", c(1:20), sep=''), # Old spawner closure
                            "scelgnbcouplingnurseclosure"  =   paste("simu", c(1:20), sep=''), # Nursery closure
                            "scelgnbcouplingfeedclosure"   =   paste("simu", c(1:6,8:20), sep='')  # Feeding closure -- remove odd simulation (no. 7)
  ) 
  
  
  ### Rename the scenarios with desired name - NOTE: Needs to be in the same order as above
  the_scenarios1 <-  c("Baseline",
                       "Seasonal Spawning Closure",
                       "Spawning Area Closure",
                       "Old Spawner Area Closure",
                       "Nursery Area Closure",
                       "Feeding Area Closure"
  )
  
}



#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2) SIMU PROCESSING FUNCTION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## NOTE: Remind to UPDATE line containing 
# t.seq



#### Start Function ####

getAggLoglikeFiles <- function(general=general, 
                               what="weight", 
                               explicit_pops=c(10,11),
                               implicit_pops=c(0,1,2,4,5,6,8,9,12:38),
                               selected_vessels_set1=selected_vessels_set1, # Considers all vessels
                               selected_vessels_set2=selected_vessels_set2, # Considers only the gillnetters
                               selected_vessels_set3=selected_vessels_set3  # Considers only the trawlers
){
  
  
  
  c.listquote <- function (...){
    args <- as.list(match.call()[-1])
    lstquote <- list(as.symbol("list"))
    for (i in args) {
      if (class(i) == "name" || (class(i) == "call" && i[[1]] !=
                                 "list")) {
        i <- eval(substitute(i), sys.frame(sys.parent()))
      }
      if (class(i) == "call" && i[[1]] == "list") {
        lstquote <- c(lstquote, as.list(i)[-1])
      }
      else if (class(i) == "character") {
        for (chr in i) {
          lstquote <- c(lstquote, list(parse(text = chr)[[1]]))
        }
      }
      else stop(paste("[", deparse(substitute(i)), "] Unknown class [",
                      class(i), "] or is not a list()", sep = ""))
    }
    return(as.call(lstquote))
  }
  
  ICESarea2 <- function (tacsat, string = TRUE){
    library(sp)
    ICES.area <- rep(NA, dim(tacsat)[1])
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(8.648336, 7.034822, 7.357525, 9.083985, 9.608377,
                                         10.22958, 10.689431, 11.084742, 11.617201, 12.068985,
                                         11.972174, 10.59262, 9.971417, 9.39862, 8.648336),
                               pol.y = c(57.08073, 57.99182, 58.20964, 58.87187, 59.32015,
                                         59.86417, 59.99375, 59.8804, 58.96783, 58.0774, 57.4653,
                                         57.74247, 57.50441, 57.10708, 57.08073)) > 0] <- ifelse(string, 'kask', '0')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(10.59262, 11.97217, 12.15883, 12.70796, 13.12992,
                                         12.80622, 12.95073, 12.72185, 12.45127, 12.29556,
                                         12.13384, 11.99063, 11.58487, 11.58487, 11.63281,
                                         11.49492, 11.3094, 11.27652, 10.71374, 10.70218,
                                         10.24553, 10.19351, 10.42472, 10.59262), pol.y = c(57.74247,
                                                                                            57.4653, 57.48032, 56.94085, 56.46389, 56.36135,
                                                                                            56.19091, 56.16918, 56.29535, 56.12728, 55.49119,
                                                                                            55.28764, 55.63113, 55.91101, 55.90623, 55.94866,
                                                                                            55.97965, 56.00988, 56.14253, 56.25853, 56.49587,
                                                                                            57.11107, 57.63566, 57.74247)) > 0] <-ifelse(string,'kask', '0')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(-4, 7, 8, 7, 7, 7.906163, -4, -4.6, -4.6, -4),
                               pol.y = c(62, 62, 61.5, 60, 58, 57.5, 57.5, 57.3, 58.2,
                                         58.4)) > 0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(7.906163, 8.6488368, 8.5, 10.3, 10.3, 9.2,
                                         9.2, 7.11, -1, -4, -1.78), pol.y = c(57.5, 57.08073,
                                                                              57, 57.3, 57, 56.2, 52.7, 53.5, 53.5, 56.1, 57.5)) >
                0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(0, 0.5, 7.5, 7.5), pol.y = c(53.5, 51, 51,
                                                                      53.5)) > 0] <- ifelse(string,'nsea', '1')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(12, 12, 11.94, 11.97, 12, 12, 15, 15, 14.78,
                                         14.2, 13.7, 12.81, 12.44), pol.y = c(55.3, 54.75,
                                                                              54.67, 54.56, 54.56, 53.5, 53.5, 55, 55.3, 55.4,
                                                                              55.5, 55.38, 55.33)) > 0] <- ifelse(string,'2224', '2')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(14.2, 14.78, 15, 15, 18, 18, 14.2), pol.y = c(55.4,
                                                                                       55.3, 55, 53, 53, 56.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(18, 18, 22, 22), pol.y = c(56.5, 53.5, 53.5,
                                                                    56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(18, 18, 18.32, 18.32, 19, 19, 16, 16), pol.y = c(56.5,
                                                                                          57, 57, 57.5, 57.925, 59.762, 59.762, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(19, 19, 18.45, 18.3, 18, 18, 21.5, 21.72, 21.98,
                                         22.17, 22.24, 21.93), pol.y = c(58.5, 57.9, 57.58,
                                                                         57, 57, 56.5, 56.5, 57.57, 57.97, 58.04, 58.15,
                                                                         58.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(21.5, 21.72, 21.98, 22.17, 22.24, 22.24, 23,
                                         25, 25), pol.y = c(56.5, 57.57, 57.97, 58.04, 58.15,
                                                            58.35, 58.5, 58.5, 56.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(19, 17.975, 21.6, 21.8, 23.325, 23.325, 23.191,
                                         23, 23, 23.5, 23.6, 24, 23.692, 22.5, 22.1, 21.92,
                                         19), pol.y = c(59.762, 60.5, 60.5, 60.7, 60.5, 59.965,
                                                        59.867, 59.827, 59, 59, 59.05, 58.75, 59.5, 59.5,
                                                        58.35, 58.5, 58.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(16.5, 16.5, 19.7, 19.7, 22.6, 21.4), pol.y = c(60.5,
                                                                                        63.7, 63.7, 63.5, 63.5, 60.5)) > 0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(19.7, 19.7, 25.7, 25.7, 19.7), pol.y = c(63.7,
                                                                                  63.5, 63.5, 67, 67)) > 0] <-ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(23.325, 23.325, 23.191, 23, 23, 30.5, 30.5),
                               pol.y = c(60.5, 59.965, 59.867, 59.827, 59, 59, 60.5)) >
                0] <- ifelse(string,'2532', '3')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(12.297, 12.13, 12.45, 12.81, 12.94, 13.21,
                                         12.5, 12.448), pol.y = c(56.13, 55.48, 55.31, 55.38,
                                                                  55.41, 55.71, 56.29, 56.305)) > 0] <- ifelse(string,'2224', '2')
    ICES.area[point.in.polygon(point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
                               pol.x = c(10.1, 10.75, 10.71, 11.58, 11.58, 11.99, 11.94,
                                         11.97, 12, 12, 9.3, 9.3), pol.y = c(56.6, 56.3, 56.15,
                                                                             55.9, 55.65, 55, 54.67, 54.56, 54.56, 53.75, 53.75,
                                                                             56.6)) > 0] <- ifelse(string,'2224', '2')
    return(ICES.area)
  }
  
  ICESrectangle <- function (dF) {
    rectChar1n2 <- as.integer(2 * (dF[, "SI_LATI"] - 35.5))
    rectChar3 <- ifelse(dF[, "SI_LONG"] <= -40, "A", ifelse(dF[, 
                                                               "SI_LONG"] <= -30, "B", ifelse(dF[, "SI_LONG"] <= -20, 
                                                                                              "C", ifelse(dF[, "SI_LONG"] <= -10, "D", ifelse(dF[, 
                                                                                                                                                 "SI_LONG"] <= 0, "E", ifelse(dF[, "SI_LONG"] <= 10, 
                                                                                                                                                                              "F", ifelse(dF[, "SI_LONG"] <= 20, "G", ifelse(dF[, 
                                                                                                                                                                                                                                "SI_LONG"] <= 30, "H", "I"))))))))
    rectChar4 <- as.integer(dF[, "SI_LONG"]%%10)
    rectID <- paste(rectChar1n2, rectChar3, rectChar4, sep = "")
    return(rectID)
  }
  
  
  ICESrectangle2LonLat <- function (statsq, midpoint = F) {
    part1 <- substr(statsq, 1, 2)
    part2 <- substr(statsq, 3, 4)
    labels <- 0:90
    latlabels <- ifelse(labels < 10, paste("0", labels, sep = ""),
                        as.character(labels))
    latvalues <- seq(35.5, 80.5, 0.5) + 0.25
    lonlabels <- paste(rep(LETTERS[2:8], rep(10, 7)), rep(0:9,
                                                          7), sep = "")
    lonvalues <- (-40:29) + 0.5
    indx <- match(part1, latlabels)
    lat <- latvalues[indx]
    indx <- match(part2, lonlabels)
    lon <- lonvalues[indx]
    if (any(is.na(lat)) | any(is.na(lon)))
      warning("Some stat squares have not been recognised.")
    if (midpoint == F) {
      lat <- lat - 0.25
      lon <- lon - 0.5
    }
    return(data.frame(SI_LATI = lat, SI_LONG = lon))
  }
  
  
  
  
  
  loadGraph <- function(){
    # load the graph
    #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
    coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
                                       paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
    coord <- as.matrix(as.vector(coord))
    coord <- matrix(coord, ncol=3)
    colnames(coord) <- c('x', 'y', 'harb')
    plot(coord[,1], coord[,2])
    
    graph <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
                                       paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
    graph <- as.matrix(as.vector(graph))
    graph <- matrix(graph, ncol=3)
    segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R
    
    return(graph)
  }
  
  
  loadCoord <- function(){
    # load the graph
    #load(file.path(general$main.path.igraph, paste(general$igraph, "_graphibm.RData",sep=''))) # built from the R code
    coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
                                       paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
    coord <- as.matrix(as.vector(coord))
    coord <- matrix(coord, ncol=3)
    colnames(coord) <- c('x', 'y', 'harb')
    plot(coord[,1], coord[,2])
    
    graph <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
                                       paste("graph", general$igraph, ".dat", sep=""))) # build from the c++ gui
    graph <- as.matrix(as.vector(graph))
    graph <- matrix(graph, ncol=3)
    segments(coord[graph[,1]+1,1], coord[graph[,1]+1,2], coord[graph[,2]+1,1], coord[graph[,2]+1,2], col=4) # CAUTION: +1, because c++ to R
    
    return(coord)
  }
  
  
  
  
  
  
  
  #--------------------------------
  explicit_pops_params <- explicit_pops   # save
  implicit_pops_params <- implicit_pops   # save
  
  ## tstep
  # t.seq <- seq(as.POSIXct(paste(general$a.year,"-01-01 00:00:00",sep='')),
  #              as.POSIXct(paste(as.numeric(as.character(general$a.year))+4,"-12-31 00:00:00",sep='')), by="hours")  # 5 years including the start y
  # 
  t.seq <- seq(as.POSIXct(paste(general$a.year,"-01-01 00:00:00",sep='')),
               as.POSIXct(paste(as.numeric(as.character(general$a.year))+9,"-12-31 00:00:00",sep='')), by="hours")  # 5 years including the start y
  
  for (sce in general$namefolderoutput){
    lst_loglike_agg_all <- list()
    lst_loglike_agg_den <- list()
    lst_loglike_agg_selected_set1 <- list()
    lst_loglike_agg_selected_set2 <- list()
    lst_loglike_agg_selected_set3 <- list()
    lst_loglike_agg_deu <- list()
    lst_loglike_agg_swe <- list()
    lst_loglike_agg_ita <- list()
    lst_loglike_agg_hrv <- list()
    lst_loglike_agg_irl <- list()
    lst_loglike_agg_fra <- list()
    lst_loglike_agg_gbr <- list()
    lst_loglike_agg_nld <- list()
    lst_loglike_agg_bel <- list()
    lst_loglike_agg_est <- list()
    lst_loglike_agg_fin <- list()
    lst_loglike_agg_ltu <- list()
    lst_loglike_agg_lva <- list()
    lst_loglike_agg_pol <- list()
    lst_loglike_agg_vid <- list()
    lst_loglike_agg_vid_port <- list()
    lst_loglike_agg_port <- list()
    lst_loglike_agg_met <- list()
    
    
    
    explicit_pops <- explicit_pops_params
    implicit_pops <- implicit_pops_params
    
    
    
    for (sim in general$namesimu[[sce]]){
      
      
      if(!general$use_sqlite){
        ## robust read for the 'loglike' output
        er <- try(   {
          filename <- file.path(general$main.path, general$namefolderinput,  sce, 
                                paste("loglike_", sim, ".dat", sep=''))
          
          #loglike <- read.table(file=filename)
          # because not same number of elements per line, replaced by:
          
          cnts<- count.fields(filename) 
          lf<-readLines(filename)
          lf<-cbind(lf,cnts)
          print(nrow(lf))
          most_freq_nb_of_field <- names(table(cnts)) [table(cnts)==max(table(cnts))]
          #!#!#!#!#!#!#!#!#!
          lf <- lf[lf[,2]==most_freq_nb_of_field,][,-2] # filter out if != most_freq_nb_of_field
          #!#!#!#!#!#!#!#!#!
          print(length(lf))
          # Write data out and read it back in (temporarily)
          filename2 <-  file.path(general$main.path, general$namefolderinput,  sce, 
                                  paste("loglike_", sim, "_corr", ".dat", sep=''))
          write(lf,file=paste(filename2))
          loglike <-read.table(filename2)
          loglike <-read.table(filename)
          gc(reset=T)
          
          
        }, silent=TRUE)
        
      } else{
        # Use sqlite
        library(DBI)
        library(RSQLite)
        con <- dbConnect(RSQLite::SQLite(), file.path(general$main.path, general$namefolderinput,  sce, 
                                                      paste(general$namefolderinput, "_", sim, "_out", ".db", sep='')))
        head(dbReadTable(con, "VesselLogLike"))
        head(dbReadTable(con, "VesselDef"))
        head(dbReadTable(con, "VesselLogLikeCatches"))
        
        res <- dbSendQuery(con, "SELECT TStep,SUM(Catches) FROM VesselLogLike JOIN VesselDef ON Id = VesselId JOIN VesselLogLikeCatches ON RowId = LoglikeId WHERE Nationality = 'DNK' GROUP BY TStep")
        dbFetch(res, n= -1)
        dbClearResult(res)
        
        res <- dbSendQuery(con, "SELECT TStep,AVG(vpuf) FROM VesselLogLike JOIN VesselDef ON Id = VesselId WHERE Nationality = 'DNK' GROUP BY TStep")
        dd <- dbFetch(res, n= -1)
        dbClearResult(res)
        
        res <- dbSendQuery(con, "SELECT TStep,SUM(gav) FROM VesselLogLike JOIN VesselDef ON Id = VesselId WHERE Nationality = 'DNK' GROUP BY TStep")
        dd <- dbFetch(res, n= -1)
        dbClearResult(res)
        sum(dd[,2])
        
        #plot(dd)
        dd$runningaverage <- cumsum(dd[,2])/(1:length(dd[,2]))
        #plot(dd[,1], dd$runningaverage)
        
        
        # todo....
      }
      
      
      if(class(er)!="try-error"){                                       
        
        
        colnames (loglike) <- c('tstep_dep', 'tstep_arr', 'reason_back','cumsteaming', 'idx_node', 
                                'idx_vessel', 'VE_REF', 'timeatsea', 'fuelcons', 'traveled_dist', 
                                paste('pop.', 0:(general$nbpops-1), sep=''), "freq_metiers", "revenue", "rev_from_av_prices", "rev_explicit_from_av_prices", 
                                "fuelcost", "vpuf", "gav", "gradva", 
                                "sweptr", "revpersweptarea",
                                paste('disc.',  explicit_pops, sep=''), "GVA", "GVAPerRevenue", "LabourSurplus", "GrossProfit", "NetProfit", 
                                "NetProfitMargin", "GVAPerFTE", "RoFTA", "BER", "CRBER", "NetPresentValue", "numTrips")   
        
        
        
        
        
        
        # discard rate (caution: bi-modal where a lots of 1 i.e. all discarded!) 
        disc_rates <- matrix(0, ncol=length(explicit_pops), nrow=nrow(loglike))
        colnames(disc_rates)  <- paste("disc_rate_", explicit_pops, sep="")
        loglike <- cbind.data.frame(loglike,  disc_rates)
        for (a_st in explicit_pops){
          loglike[, paste("disc_rate_", a_st, sep="")]   <-  loglike   [, paste("disc.", a_st, sep="")]  / (loglike   [, paste("disc.", a_st, sep="")]  + loglike [, paste("pop.", a_st, sep="")]) 
        }
        
        
        # add an energy efficiency calculation
        loglike$vpuf <- as.numeric(as.character(loglike$revenue))/ 
          as.numeric(as.character(loglike$fuelcons)) #value per unit of fuel in euro per litre
        loglike$vapuf <- as.numeric(as.character(loglike$rev_from_av_prices))/ 
          as.numeric(as.character(loglike$fuelcons)) #value per unit of fuel in euro per litre
        
        
        
        ##...and get back the time info from this...
        loglike$time       <-   t.seq[as.numeric(as.character(loglike$tstep_arr))]
        loglike$month      <-   format(strptime(  loglike$time , tz='GMT',  "%Y-%m-%e %H:%M:%S"  ), "%m")
        loglike$year       <-   format(strptime(  loglike$time , tz='GMT',  "%Y-%m-%e %H:%M:%S"  ), "%Y")
        loglike$year.month <-   paste(loglike$year, '.', loglike$month, sep='')
        #!#!#!#!#!#
        ##=> caution: we are taking the time at the ARRIVAL date to assign a month...
        #(TO DO: better to assign repsective part to each month)
        # so possibly the physical limit (around 600 hours) can be overshoot 
        # if a given trip of the last month end up to the current month(it is just an artefact then...)
        #!#!#!#!#!#
        
        print("reading loglike----OK")
        
        ## get back the port name
        port_names <- read.table(file=file.path(general$main.path.ibm, 
                                                paste("harboursspe_",general$namefolderinput,sep=''), 
                                                paste("names_harbours.dat", sep='')), sep=";", header=TRUE)
        port_names           <- cbind(port_names, port=rownames(port_names))
        coord <- read.table(file=file.path(general$main.path.ibm, "graphsspe", 
                                           paste("coord", general$igraph, ".dat", sep=""))) # build from the c++ gui
        coord <- as.matrix(as.vector(coord))
        coord <- matrix(coord, ncol=3)
        colnames(coord) <- c('x', 'y', 'harb')
        
        #coord                <- cbind(coord, idx=0:(nrow(coord)-1))
        #head(coord[coord[,"harb"]==303,])
        loglike$land_port    <- port_names[   coord[loglike$idx_node +1 , "harb"]   , 'port']
        # => caution to this +1 because offset by 1 between loglike and coord row index!
        loglike$ld_port_x    <- coord[loglike$idx_node +1, "x"]  
        loglike$ld_port_y    <- coord[loglike$idx_node +1, "y"]  
        
        
        
        
        print("ports----OK")
        
        ## sort per vessel and compute effort
        library(doBy)
        loglike        <- orderBy(~VE_REF+tstep_dep, data=loglike)
        loglike$effort <- loglike$tstep_arr - loglike$tstep_dep # trip duration in hours because hourly time step...
        
        if(what=="cpue"){
          loglike$feffort <- (loglike$tstep_arr - loglike$tstep_dep) -loglike$cumsteaming
          loglike[,grep("pop.", colnames(loglike))] <-  loglike[,grep("pop.", colnames(loglike))] / loglike$feffort
          loglike <- loglike[,-grep("feffort", colnames(loglike))] # remove the col now it has been used.
        }
        
        ## add the total landings
        nm <- colnames(loglike)
        idx.col.c <- grep('pop.', nm)
        loglike$totland <- apply(loglike[,idx.col.c], 1, sum, na.rm=TRUE)
        
        ## add the total landings explicit
        nm <- colnames(loglike)
        #idx.col.c <- grep('pop.', nm)
        loglike$totland_explicit <- apply(loglike[, paste("pop.", explicit_pops, sep='')], 1, sum, na.rm=TRUE)
        if(!is.null(implicit_pops)) {
          loglike$totland_implicit <- apply(loglike[, paste("pop.", implicit_pops, sep='')], 1, sum, na.rm=TRUE)
        } else{
          loglike$totland_implicit <- 0
        }
        
        
        ##=> note that we have now the duration of each trip,
        ## and the duration between trips (i.e. stay on quayside) is given by time between two successive records
        ## and nb of trips per month can also be deduced here......
        loglike$nbtrip <- 1
        loglike$bwtrip <- 0
        loglike$bwtrip[1:(nrow(loglike)-1)] <- 
          loglike$tstep_dep[2:nrow(loglike)] - loglike$tstep_arr[1:(nrow(loglike)-1)] # trip duration in hours because hourly time step...
        loglike[diff(as.numeric(loglike$VE_REF))==1, "bwtrip"] <- 0 # correct when change of vid
        
        
        ##-------------
        ## agg utils ##
        ##-------------
        aggregateLoglike <- function(loglike, agg_by=c("year.month"), what="weight"){
          library(data.table)
          nm <- names(loglike)
          idx.col.c   <- grep('pop.', nm)
          idx.col.d   <- grep('disc.', nm, fixed=TRUE)
          idx.col.e   <- grep('effort', nm)
          idx.col.s   <- grep('cumsteaming', nm)
          idx.col.t   <- grep('nbtrip', nm)
          idx.col.b   <- grep('bwtrip', nm)
          idx.col.f   <- grep('fuelcons', nm)
          idx.col.r   <- grep('revenue', nm, fixed = TRUE)
          idx.col.sa  <- grep('sweptr', nm, fixed = TRUE)
          idx.col.ra  <- grep('rev_from_av_prices', nm)
          idx.col.fc  <- grep('fuelcost', nm)
          idx.col.g   <- grep('gav', nm, fixed = TRUE)
          idx.col.g2  <- grep('gradva', nm)
          idx.col.l   <-  which(nm== 'totland')
          idx.col.l2  <- which(nm== 'totland_explicit')
          idx.col.l3  <- which(nm== 'totland_implicit')
          #idx.col.pops  <- which(nm %in% paste('pop.', explicit_pops, sep=''))
          idx.col.g3  <- which(nm== 'rev_explicit_from_av_prices') 
          idx.col <- c(idx.col.c,  idx.col.d, idx.col.e, idx.col.s, idx.col.t, idx.col.b, idx.col.f, idx.col.r, 
                       idx.col.ra,  idx.col.sa, idx.col.fc, idx.col.g,  idx.col.g2, idx.col.l, idx.col.l2, idx.col.l3,
                       #idx.col.pops,
                       idx.col.g3)
          colnames(loglike)[idx.col] # check             
          DT  <- data.table(loglike) # library data.table for fast grouping replacing aggregate()
          # AGGREGATE PER SPECIES -----> SUM (IF WEIGHT) OR MEAN (IF CPUE)
          a_mean <- function(x, na.rm) mean(x[x!=0], na.rm=na.rm) # modify the mean() so that 0 are first removed....
          if(what=="weight") eq1  <- c.listquote( paste ("sum(",nm[idx.col],",na.rm=TRUE)",sep="") )
          if(what=="cpue") eq1  <- c.listquote( paste ("a_mean(",nm[idx.col],",na.rm=TRUE)",sep="") )
          a_by <- c.listquote(  agg_by ) 
          loglike.agg <- DT[,eval(eq1), by= eval(a_by)]
          loglike.agg <- data.frame( loglike.agg)
          colnames(loglike.agg) <- c(agg_by, colnames(loglike)[idx.col] )
          library(doBy)
          loglike.agg <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=loglike.agg) # order to ensure same order when collating....
          # AGGREGATE PER SPECIES -----> MEAN
          idx.col.e  <- grep('effort', nm)
          idx.col.b  <- grep('bwtrip', nm)
          idx.col.t  <- grep('traveled_dist', nm)
          idx.col.v  <- grep('vpuf', nm)
          idx.col.v2  <- grep('vapuf', nm)
          idx.col.rsa <- grep('revpersweptarea', nm, fixed = TRUE)
          idx.col.disc_rate  <- which(nm %in% paste('disc_rate_', explicit_pops, sep=''))
          idx.col <- c(idx.col.e, idx.col.b, idx.col.t,  idx.col.v, idx.col.v2, idx.col.disc_rate, idx.col.rsa)
          eq1  <- c.listquote( paste ("mean(",nm[idx.col],",na.rm=TRUE)",sep="") )
          a_by <- c.listquote(  agg_by ) 
          loglike.agg2 <- DT[,eval(eq1),by=eval(a_by)]
          loglike.agg2 <- data.frame( loglike.agg2)
          some_col_names           <- colnames(loglike)[idx.col]
          some_col_names_redundant <-  some_col_names [some_col_names %in%  colnames(loglike.agg[,-c(1:length(agg_by))])] 
          some_col_names [some_col_names %in% some_col_names_redundant] <- paste0("av_",some_col_names_redundant)  # a fix
          colnames(loglike.agg2)   <- c(agg_by, some_col_names )
          library(doBy)
          loglike.agg2 <- orderBy (as.formula(paste("~ ", paste(agg_by, collapse="+"))), data=loglike.agg2) # order to ensure same order when collating....
          # collate
          loglike.agg <- cbind(loglike.agg, loglike.agg2[,-c(1:length(agg_by))])
          
          # av_vpuf per trip can be biased by large outliers, so instead look at:
          loglike.agg$av_vapuf_month <- loglike.agg$rev_from_av_prices /  loglike.agg$fuelcons
          
          return(loglike.agg)
        }
        
        
        ##-------------
        ## aggregate by year.month ALL COUNTRIES
        loglike.agg <-  aggregateLoglike(loglike, agg_by=c("year.month"))
        
        # split per country
        loglike.den     <- loglike[grep('DNK',loglike$VE_REF),]  # caution 
        loglike.agg.den <-  aggregateLoglike(loglike.den, agg_by=c("year.month"))
        
        # split per country
        loglike.swe     <- loglike[grep('SWE',loglike$VE_REF),]  # caution 
        loglike.agg.swe <-  aggregateLoglike(loglike.swe, agg_by=c("year.month"))
        
        # split per country
        loglike.ita     <- loglike[grep('ITA',loglike$VE_REF),]  # caution 
        loglike.agg.ita <-  aggregateLoglike(loglike.ita, agg_by=c("year.month"))
        
        # split per country
        loglike.hrv     <- loglike[grep('HRV',loglike$VE_REF),]  # caution 
        loglike.agg.hrv <-  aggregateLoglike(loglike.hrv, agg_by=c("year.month"))
        
        # split per country
        loglike.irl     <- loglike[grep('IRL',loglike$VE_REF),]  # caution 
        loglike.agg.irl <-  aggregateLoglike(loglike.irl, agg_by=c("year.month"))
        
        # split per country
        loglike.fra     <- loglike[grep('FRA',loglike$VE_REF),]  # caution 
        loglike.agg.fra <-  aggregateLoglike(loglike.fra, agg_by=c("year.month"))
        
        # split per country
        loglike.gbr     <- loglike[grep('GBR',loglike$VE_REF),]  # caution 
        loglike.agg.gbr <-  aggregateLoglike(loglike.gbr, agg_by=c("year.month"))
        
        # split per country
        loglike.nld     <- loglike[grep('NLD',loglike$VE_REF),]  # caution 
        loglike.agg.nld <-  aggregateLoglike(loglike.nld, agg_by=c("year.month"))
        
        # split per country
        loglike.bel     <- loglike[grep('BEL',loglike$VE_REF),]  # caution 
        loglike.agg.bel <-  aggregateLoglike(loglike.bel, agg_by=c("year.month"))
        
        # split per country
        loglike.deu     <- loglike[grep('DEU',loglike$VE_REF),]  # caution 
        loglike.agg.deu <-  aggregateLoglike(loglike.deu, agg_by=c("year.month"))
        
        # split per country
        loglike.est     <- loglike[grep('EST',loglike$VE_REF),]  # caution 
        loglike.agg.est <-  aggregateLoglike(loglike.est, agg_by=c("year.month"))
        
        # split per country
        loglike.fin     <- loglike[grep('FIN',loglike$VE_REF),]  # caution 
        loglike.agg.fin <-  aggregateLoglike(loglike.fin, agg_by=c("year.month"))
        
        # split per country
        loglike.ltu     <- loglike[grep('LTU',loglike$VE_REF),]  # caution 
        loglike.agg.ltu <-  aggregateLoglike(loglike.ltu, agg_by=c("year.month"))
        
        # split per country
        loglike.lva     <- loglike[grep('LVA',loglike$VE_REF),]  # caution 
        loglike.agg.lva <-  aggregateLoglike(loglike.lva, agg_by=c("year.month"))
        
        # split per country
        loglike.pol     <- loglike[grep('POL',loglike$VE_REF),]  # caution 
        loglike.agg.pol <-  aggregateLoglike(loglike.pol, agg_by=c("year.month"))
        
        # split per country
        loglike.swe     <- loglike[grep('SWE',loglike$VE_REF),]  # caution 
        loglike.agg.swe <-  aggregateLoglike(loglike.swe, agg_by=c("year.month"))
        
        # split per selected set of vessels
        if(!is.null(selected_vessels_set1)){
          loglike_selected_set1        <- loglike[loglike$VE_REF %in% selected_vessels_set1,]  # caution
          loglike_selected_set1$VE_REF <- factor(loglike_selected_set1$VE_REF)
          loglike.agg.selected.set1    <-  aggregateLoglike(loglike_selected_set1, agg_by=c("year.month"))
        }
        
        # split per selected set of vessels
        if(!is.null(selected_vessels_set2)){
          loglike_selected_set2        <- loglike[loglike$VE_REF %in% selected_vessels_set2,]  # caution
          loglike_selected_set2$VE_REF <- factor(loglike_selected_set2$VE_REF)
          loglike.agg.selected.set2    <-  aggregateLoglike(loglike_selected_set2, agg_by=c("year.month"))
        }
        
        # split per selected set of vessels
        if(!is.null(selected_vessels_set3)){
          loglike_selected_set3        <- loglike[loglike$VE_REF %in% selected_vessels_set3,]  # caution
          loglike_selected_set3$VE_REF <- factor(loglike_selected_set3$VE_REF)
          loglike.agg.selected.set3    <-  aggregateLoglike(loglike_selected_set3, agg_by=c("year.month"))
        }
        
        
        
        ## aggregate by year.month, PER VESSEL
        loglike.agg.vid <-  aggregateLoglike(loglike, agg_by=c( "VE_REF", "year.month"))
        
        
        # CONTINGENCY TABLE -----> SPECIAL CASE FOR REASON_BACK     
        loglike$VE_REF     <- factor(loglike$VE_REF)
        loglike.agg3.vid   <- data.frame(xtabs(~year.month+VE_REF+reason_back, data=loglike))
        loglike.agg3.vid   <- orderBy (~year.month+VE_REF, data=loglike.agg3.vid)
        
        loglike.agg3.vid_1 <- loglike.agg3.vid[loglike.agg3.vid$reason_back=="1",]
        
        print(sce)
        print(head(loglike.agg3.vid))
        print(dim(loglike.agg3.vid ))
        print(dim(loglike.agg3.vid_1 ))
        print(head(loglike.agg3.vid_1$year.month))
        print(head(loglike.agg3.vid_1$VE_REF))
        print(dim(paste (loglike.agg3.vid_1$year.month,".",loglike.agg3.vid_1$VE_REF, sep='')))
        
        loglike.agg3.vid_2 <- loglike.agg3.vid[loglike.agg3.vid$reason_back=="2",]
        loglike.agg3.vid_3 <- loglike.agg3.vid[loglike.agg3.vid$reason_back=="3",]
        if(nrow(loglike.agg3.vid_1)>0) rownames(loglike.agg3.vid_1) <- paste (loglike.agg3.vid_1$year.month,".",loglike.agg3.vid_1$VE_REF, sep='')
        if(nrow(loglike.agg3.vid_2)>0) rownames(loglike.agg3.vid_2) <- paste (loglike.agg3.vid_2$year.month,".",loglike.agg3.vid_2$VE_REF, sep='')
        if(nrow(loglike.agg3.vid_3)>0) rownames(loglike.agg3.vid_3) <- paste (loglike.agg3.vid_3$year.month,".",loglike.agg3.vid_3$VE_REF, sep='')
        if(nrow(loglike.agg3.vid)>0)rownames(loglike.agg.vid) <- paste (loglike.agg.vid$year.month,".",loglike.agg.vid$VE_REF, sep='')
        # collate
        if(nrow(loglike.agg3.vid)>0) loglike.agg.vid <- cbind(loglike.agg.vid, 
                                                              reason_1= loglike.agg3.vid_1[rownames(loglike.agg.vid),c(4)],
                                                              reason_2= loglike.agg3.vid_2[rownames(loglike.agg.vid),c(4)],
                                                              reason_3= loglike.agg3.vid_3[rownames(loglike.agg.vid),c(4)] )
        
        
        
        
        ## aggregate by year.month, PER VESSEL AND PORT
        loglike.agg.vid.port <-  aggregateLoglike(loglike, agg_by=c("VE_REF", "year.month", "land_port","ld_port_x", "ld_port_y"))
        loglike.agg.port     <-  aggregateLoglike(loglike, agg_by=c("year.month", "land_port","ld_port_x", "ld_port_y"))
        
        
        #loglike.agg3.vid.port_0 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="0",]
        #loglike.agg3.vid.port_1 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="1",]
        #loglike.agg3.vid.port_2 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="2",]
        #loglike.agg3.vid.port_3 <- loglike.agg3.vid.port[loglike.agg3.vid.port$reason_back=="3",]
        #rownames(loglike.agg3.vid.port_0) <- paste (loglike.agg3.vid.port_0$year.month,".",loglike.agg3.vid.port_0$VE_REF,".",loglike.agg3.vid.port_0$land_port, sep='')
        #rownames(loglike.agg3.vid.port_1) <- paste (loglike.agg3.vid.port_1$year.month,".",loglike.agg3.vid.port_1$VE_REF,".",loglike.agg3.vid.port_1$land_port, sep='')
        #rownames(loglike.agg3.vid.port_2) <- paste (loglike.agg3.vid.port_2$year.month,".",loglike.agg3.vid.port_2$VE_REF,".",loglike.agg3.vid.port_2$land_port, sep='')
        #rownames(loglike.agg3.vid.port_3) <- paste (loglike.agg3.vid.port_3$year.month,".",loglike.agg3.vid.port_3$VE_REF,".",loglike.agg3.vid.port_3$land_port, sep='')
        #rownames(loglike.agg.vid.port) <- paste (loglike.agg.vid.port$year.month,".",loglike.agg.vid.port$VE_REF,".",loglike.agg.vid.port$land_port, sep='')
        # collate
        
        #loglike.agg.vid.port <- cbind(loglike.agg.vid.port, loglike.agg2.vid.port[,-(1:5)] #,  
        #                   reason_0= loglike.agg3.vid.port_0[rownames(loglike.agg.vid.port),c(4)],
        #                     reason_1= loglike.agg3.vid.port_1[rownames(loglike.agg.vid.port),c(4)],
        #                      reason_2= loglike.agg3.vid.port_2[rownames(loglike.agg.vid.port),c(4)],
        #                       reason_3= loglike.agg3.vid.port_3[rownames(loglike.agg.vid.port),c(4)] 
        #                            )
        
        
        
        
        
        
        
        print("Aggregations from loglike----OK")
        
        
        
        ## plot landings per species
        #X11()
        #matplot( loglike.agg[,paste('pop.', 0:(general$nbpops-1), sep='')]/1e3, ylim=c(1,1e4), type="b", main="Simulated landings per species",
        #         pch=as.character(1:general$nbpops), xlab="Month", ylab="Landings [tons]", lwd=2, lty=1)
        
        ## plot total effort
        #X11()
        #matplot( loglike.agg[,'effort'], ylim=c(1,max(loglike.agg[,'effort'])), type="b", main="simulated total effort",
        #         xlab="Month", ylab="Effort [hours]", lwd=2, lty=1)
        
        #graphics.off()
        
        
        lst_loglike_agg_all[[sim]] <- loglike.agg
        if(!is.null(selected_vessels_set1)){ lst_loglike_agg_selected_set1[[sim]] <- loglike.agg.selected.set1  }
        if(!is.null(selected_vessels_set2)){ lst_loglike_agg_selected_set2[[sim]] <- loglike.agg.selected.set2  }
        if(!is.null(selected_vessels_set3)){ lst_loglike_agg_selected_set3[[sim]] <- loglike.agg.selected.set3  }
        if('DEU' %in% general$a.country){ lst_loglike_agg_deu[[sim]] <- loglike.agg.deu }
        if('SWE' %in% general$a.country){ lst_loglike_agg_swe[[sim]] <- loglike.agg.swe }
        if('DNK' %in% general$a.country){ lst_loglike_agg_den[[sim]] <- loglike.agg.den }
        if('ITA' %in% general$a.country){ lst_loglike_agg_ita[[sim]] <- loglike.agg.ita }
        if('HRV' %in% general$a.country){ lst_loglike_agg_hrv[[sim]] <- loglike.agg.hrv }
        if('IRL' %in% general$a.country){ lst_loglike_agg_irl[[sim]] <- loglike.agg.irl }
        if('GBR' %in% general$a.country){ lst_loglike_agg_gbr[[sim]] <- loglike.agg.gbr }
        if('BEL' %in% general$a.country){ lst_loglike_agg_bel[[sim]] <- loglike.agg.bel }
        if('NLD' %in% general$a.country){ lst_loglike_agg_nld[[sim]] <- loglike.agg.nld }
        if('FRA' %in% general$a.country){ lst_loglike_agg_fra[[sim]] <- loglike.agg.fra }
        if('DEU' %in% general$a.country){ lst_loglike_agg_deu[[sim]] <- loglike.agg.deu }
        if('EST' %in% general$a.country){ lst_loglike_agg_est[[sim]] <- loglike.agg.est }
        if('FIN' %in% general$a.country){ lst_loglike_agg_fin[[sim]] <- loglike.agg.fin }
        if('LTU' %in% general$a.country){ lst_loglike_agg_ltu[[sim]] <- loglike.agg.ltu }
        if('LVA' %in% general$a.country){ lst_loglike_agg_lva[[sim]] <- loglike.agg.lva }
        if('POL' %in% general$a.country){ lst_loglike_agg_pol[[sim]] <- loglike.agg.pol }
        lst_loglike_agg_vid[[sim]]      <- loglike.agg.vid
        lst_loglike_agg_vid_port[[sim]] <- loglike.agg.vid.port
        lst_loglike_agg_port[[sim]]     <- loglike.agg.port
        ## if('ITA' %in% general$a.country){ lst_loglike_agg_met[[sim]] <- loglike.agg.met}
        
        
        
        cat(paste(sce,"...OK\n"))
        cat(paste(sim,"...OK\n"))
      } else{
        cat(paste("error detected for this", sim, ":remove from the list of simus"))
        general$namesimu[[sce]] <- general$namesimu[[sce]][!general$namesimu[[sce]] %in% sim]
      }
      
      
      
    } # end for sim
    
    
    assign(paste("lst_loglike_agg_",what,"_all_", sce, sep=''), lst_loglike_agg_all)
    if(!is.null(selected_vessels_set1)){ assign(paste("lst_loglike_agg_",what,"_selected_set1_", sce, sep=''), lst_loglike_agg_selected_set1)  }
    if(!is.null(selected_vessels_set2)){ assign(paste("lst_loglike_agg_",what,"_selected_set2_", sce, sep=''), lst_loglike_agg_selected_set2)  }
    if(!is.null(selected_vessels_set3)){ assign(paste("lst_loglike_agg_",what,"_selected_set3_", sce, sep=''), lst_loglike_agg_selected_set3)  }
    if('DEU' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_deu_", sce, sep=''), lst_loglike_agg_deu) }
    if('SWE' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_swe_", sce, sep=''), lst_loglike_agg_swe) }
    if('DNK' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_den_", sce, sep=''), lst_loglike_agg_den) }
    if('ITA' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_ita_", sce, sep=''), lst_loglike_agg_ita) }
    if('HRV' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_hrv_", sce, sep=''), lst_loglike_agg_hrv) }
    if('IRL' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_irl_", sce, sep=''), lst_loglike_agg_irl) }
    if('GBR' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_gbr_", sce, sep=''), lst_loglike_agg_gbr) }
    if('FRA' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_fra_", sce, sep=''), lst_loglike_agg_fra) }
    if('BEL' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_bel_", sce, sep=''), lst_loglike_agg_bel) }
    if('NLD' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_nld_", sce, sep=''), lst_loglike_agg_nld) }
    if('EST' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_est_", sce, sep=''), lst_loglike_agg_est) }
    if('FIN' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_fin_", sce, sep=''), lst_loglike_agg_fin) }
    if('LTU' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_ltu_", sce, sep=''), lst_loglike_agg_ltu) }
    if('LVA' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_lva_", sce, sep=''), lst_loglike_agg_lva) }
    if('POL' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_pol_", sce, sep=''), lst_loglike_agg_pol) }
    assign(paste("lst_loglike_agg_",what,"_vid_", sce, sep=''), lst_loglike_agg_vid)
    assign(paste("lst_loglike_agg_",what,"_vid_port_", sce, sep=''), lst_loglike_agg_vid_port)
    assign(paste("lst_loglike_agg_",what,"_port_", sce, sep=''), lst_loglike_agg_port)
    ## if('ITA' %in% general$a.country){ assign(paste("lst_loglike_agg_",what,"_met_", sce, sep=''), lst_loglike_agg_met) }
    
    # save for later use....
    save(list=c(paste("lst_loglike_agg_",what,"_all_", sce, sep=''),
                
                if('GER' %in% general$a.country){paste("lst_loglike_agg_",what,"_ita_", sce, sep='')},
                if('DNK' %in% general$a.country){paste("lst_loglike_agg_",what,"_den_", sce, sep='')},
                if('DEU' %in% general$a.country){paste("lst_loglike_agg_",what,"_deu_", sce, sep='')},
                if('SWE' %in% general$a.country){paste("lst_loglike_agg_",what,"_swe_", sce, sep='')},
                if('ITA' %in% general$a.country){paste("lst_loglike_agg_",what,"_ita_", sce, sep='')},
                if('HRV' %in% general$a.country){paste("lst_loglike_agg_",what,"_hrv_", sce, sep='')},
                if('IRL' %in% general$a.country){paste("lst_loglike_agg_",what,"_irl_", sce, sep='')},
                if('GBR' %in% general$a.country){paste("lst_loglike_agg_",what,"_gbr_", sce, sep='')},
                if('FRA' %in% general$a.country){paste("lst_loglike_agg_",what,"_fra_", sce, sep='')},
                if('BEL' %in% general$a.country){paste("lst_loglike_agg_",what,"_bel_", sce, sep='')},
                if('NLD' %in% general$a.country){paste("lst_loglike_agg_",what,"_nld_", sce, sep='')},
                if('EST' %in% general$a.country){paste("lst_loglike_agg_",what,"_est_", sce, sep='')},
                if('FIN' %in% general$a.country){paste("lst_loglike_agg_",what,"_fin_", sce, sep='')},
                if('LTU' %in% general$a.country){paste("lst_loglike_agg_",what,"_ltu_", sce, sep='')},
                if('LVA' %in% general$a.country){paste("lst_loglike_agg_",what,"_lva_", sce, sep='')},
                if('POL' %in% general$a.country){paste("lst_loglike_agg_",what,"_pol_", sce, sep='')},
                
                if(!is.null(selected_vessels_set1)){ paste("lst_loglike_agg_",what,"_selected_set1_", sce, sep='')},
                if(!is.null(selected_vessels_set2)){ paste("lst_loglike_agg_",what,"_selected_set2_", sce, sep='')},
                if(!is.null(selected_vessels_set3)){ paste("lst_loglike_agg_",what,"_selected_set3_", sce, sep='')},
                
                paste("lst_loglike_agg_",what,"_vid_", sce, sep=''),
                paste("lst_loglike_agg_",what,"_vid_port_", sce, sep=''),
                paste("lst_loglike_agg_",what,"_port_", sce, sep='') #,
                
                #if('ITA' %in% general$a.country){ paste("lst_loglike_agg_",what,"_met_", sce, sep='')}
                
    ),
    file=file.path(general$main.path, general$namefolderinput,
                   sce, paste("lst_loglike_",what,"_agg_", sce,".RData", sep='') )   
    )
    
    
    
  } # end for sce
  
  
  
  return()
}


#### END Function ####


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~
# 3) VESSEL SELECTION
#~~~~~~~~~~~~~~~~~~~~~~~

# First of all, figure out the list of vessels we want to keep for _selected...


# RUN THIS WHEN FIRST TIME ONLY ####


## For BalticSea application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(general$namefolderinput=="BalticSea"){ 
  
  implicit_pops <- c(0, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 36) #See file pop_names_BalticSea.txt in /DISPLACE_input_BalticSea
  explicit_pops <- c(0:36)[-(implicit_pops+1)] 
  
  
  filename2 <- file.path(general$main.path, general$namefolderinput, "scelgnbcouplingnoclosure", 
                         paste("loglike_", "simu1", ".dat", sep=''))
  loglike <- read.table(filename2)
  loglike[,"V7"] <- as.factor(loglike[,"V7"])
  all_vids <- levels(head(loglike[,"V7"]))
  
  
  selected_vessels_set1   <- all_vids
  selected_vessels_set2  <-  c(all_vids[grep("GNS",all_vids)],  all_vids[grep("GTR",all_vids)], all_vids[grep("GND",all_vids)])
  selected_vessels_set3  <-  all_vids[!all_vids %in% selected_vessels_set2]
  
  
  getAggLoglikeFiles(general=general, what="weight",
                     explicit_pops=explicit_pops,
                     implicit_pops=NULL,
                     selected_vessels_set1=selected_vessels_set1,
                     selected_vessels_set2=selected_vessels_set2,
                     selected_vessels_set3=selected_vessels_set3)  
}


#### END: RUN THIS WHEN FIRST TIME ONLY ####


#><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><


#~~~~~~~~~~~~~~~~~~~~~~~~
# 4) Make the Boxplots
#~~~~~~~~~~~~~~~~~~~~~~~~

# First we read the loglike files.



# 4.1) Read the loglike files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# reload scenarios for what="weight" 
if(general$case_study == "BalticSea"){ 
  
  source(file.path(general$main.path.param,"DISPLACE_R_outputs_ForBalticSea","loadAggLoglikeFiles.R"))
  loadLoglikeFiles(general=general, use_port_info=FALSE) 
  
}


# 4.2) Define vessels, implicit & explicit populations & baseline scenario
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here we specify explicitly for which fishery & fish population we want to
# produce the boxplots. As the boxplots are all plotted relative to the baseline,
# we also have to define which scenario constitutes the baseline scenario.

if(general$case_study=="BalticSea"){
  
  sets <- c("_selected_set1_", "_selected_set2_", "_selected_set3_") # All, gillnetters, trawlers
  
  implicit_pops <- c(0, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29, 30, 32, 33, 34, 36)
  explicit_pops <- c(0:36)[-(implicit_pops+1)] 
  
  
  # INDICATE SCENARIO FOR BASELINE #
  #********************************#
  a_baseline <- "scelgnbcouplingnoclosure" #Baseline
  #********************************#
  
}


#  4.3) Calculate % over baseline
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## For each indicator and vessel, we calculate the
## percentage over the baseline ration.


for (selected in sets){
  
  
  #########
  ### Calculate % over ratio for each DISPLACE indicator
  #########
  
  outcomes <- NULL
  
  
  what2 <- "weight"
  lst_loglike_w_agg_all_1     <- get(paste("lst_loglike_agg_",what2, selected, a_baseline, sep=''))
  dd                          <- table(unlist(lapply(lst_loglike_w_agg_all_1, nrow)))
  expected_nb_rows            <- as.numeric(names(dd[dd==max(dd)]))[1] # most common number of rows
  idx                         <- unlist(lapply(lst_loglike_w_agg_all_1, function(x) nrow(x)==expected_nb_rows))
  namesimu1                   <- names(unlist(lapply(lst_loglike_w_agg_all_1, function(x) nrow(x)==expected_nb_rows)))[idx]
  lst_loglike_w_agg_all_1     <- lst_loglike_w_agg_all_1[namesimu1]
  
  lst_loglike_w_agg_vid_1     <- get(paste("lst_loglike_agg_",what2,"_vid_", a_baseline, sep=''))
  
  
  
  #*****************************#
  others_than_baseline        <- general$namefolderoutput[!general$namefolderoutput %in% a_baseline] ## All scenarios other than the baseline scenario
  #*****************************#
  
  
  # quick check at the vessel id level
  # plot(density(lst_loglike_w_agg_vid_1[[1]]$gradva))
  # lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", others_than_baseline[1], sep=''))
  # for (ff in namesimu1){
  #   lines(density(lst_loglike_w_agg_vid_1[[ff]]$gradva),  col=1)
  #   lines(density(lst_loglike_w_agg_vid_2[[ff]]$gradva),  col=2)
  # }
  
  
  # fishing effort
  ####################
  feffort <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    
    cat (paste(sce, "------------------------feffort--------------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2, selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(x[,'effort'])- sum(x[,'cumsteaming'])))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'effort'])- sum(x[,'cumsteaming'])))[namesimu1])
    print(t.test(dd1,dd2))
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'effort']))- sum(as.numeric(x[,'cumsteaming'])))))
    ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'effort']))-sum(as.numeric(x[,'cumsteaming'])))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'effort']))-sum(as.numeric(x[,'cumsteaming']))))[namesimu1] *100) -100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<400]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    feffort[sce,"mean"] <- signif(a_mean,5)
    feffort[sce,"a_CI"] <- signif(a_CI,3)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="feffort"))
  }
  print(feffort)
  
  # note that 
  #boxplot(dd1/dd2, dd1/rev(dd2)) almost identical so we don\B4t care comparing sim with sim whatever the sim
  
  
  # steaming effort
  ###################
  seffort <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "--------------------------seffort------------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'cumsteaming'])))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'cumsteaming'])))[namesimu1])
    print(t.test(dd1,dd2))
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'cumsteaming']))))
    ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'cumsteaming']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'cumsteaming'])))[namesimu1] *100) -100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<400]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    seffort[sce,"mean"] <- signif(a_mean,5)
    seffort[sce,"a_CI"] <- signif(a_CI,3)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="seffort"))
  }
  print(seffort)
  
  
  # percent fishing effort compared to total trip effort
  ########################################################
  propfeffort <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "-------------------------propfeffort-------------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort'])))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort'])))[namesimu1])
    print(t.test(dd1,dd2))
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort']))))
    ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) (sum(x[,'effort'])- sum(x[,'cumsteaming']))/sum(x[,'effort'])))[namesimu1] *100) -100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<200]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    propfeffort[sce,"mean"] <- signif(a_mean,5)
    propfeffort[sce,"a_CI"] <- signif(a_CI,3)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="propfeffort"))
  }
  print(propfeffort)
  
  
  # nbtrip
  ##########
  nbtrip <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "----------------------------nbtrip----------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'nbtrip'])))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'nbtrip'])))[namesimu1])
    print(t.test(dd1,dd2))
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,'nbtrip']))))
    ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'nbtrip']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,'nbtrip'])))[namesimu1] *100) -100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<400]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    nbtrip[sce,"mean"] <- signif(a_mean,5)
    nbtrip[sce,"a_CI"] <- signif(a_CI,3)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="nbtrip"))
  }
  print(nbtrip)
  
  
  # av_bwtrip
  #############
  av_bwtrip <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "--------------------------av_bwtrip------------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_bwtrip']))))
    ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_bwtrip']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_bwtrip'])))[namesimu1] *100) -100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    #ratio_percent <- ratio_percent[ratio_percent<200]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    av_bwtrip[sce,"mean"] <- signif(a_mean,5)
    av_bwtrip[sce,"a_CI"] <- signif(a_CI,3)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_bwtrip")) 
  }
  print(av_bwtrip)
  
  
  # trip_duration
  ###################
  av_trip_duration <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "--------------------------av_effort------------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_effort'])))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_effort'])))[namesimu1])
    print(t.test(dd1,dd2))
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_effort']))))
    ratio_percent <- ( (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_effort']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_effort'])))[namesimu1] *100) -100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<200]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    av_trip_duration[sce,"mean"] <- signif(a_mean,5)
    av_trip_duration[sce,"a_CI"] <- signif(a_CI,3)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_trip_duration")) 
  }
  print(av_trip_duration)
  
  
  
  # totland
  ############
  if(any(lst_loglike_w_agg_all_1[[1]]$totland!=0)) {
    totland <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
    for (sce in others_than_baseline){
      cat (paste(sce, "------------------------------totland--------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
      namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
      nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'totland']))))
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland']))))[namesimu1])
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland']))))[namesimu1])
      print(t.test(dd1,dd2))
      ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland']))))[namesimu1] *100) -100
      ratio_percent <- ratio_percent[!is.na(ratio_percent)]
      ratio_percent <- ratio_percent[ratio_percent<600]
      a_mean        <- mean(ratio_percent) 
      a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      totland[sce,"mean"] <- signif(a_mean,5)
      totland[sce,"a_CI"] <- signif(a_CI,3)
      outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="totland")) 
    }
    print(totland)
  }
  
  
  # totland_explicit
  #####################
  totland_explicit <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "--------------------------totland_explicit------------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'totland_explicit']))))
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1])
    print(t.test(dd1,dd2))
    ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_explicit']))))[namesimu1] *100) -100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    #ratio_percent <- ratio_percent[ratio_percent<200]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    totland_explicit[sce,"mean"] <- signif(a_mean,5)
    totland_explicit[sce,"a_CI"] <- signif(a_CI,3)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="totland_explicit")) 
  }
  print(totland_explicit)
  
  
  # totland_
  ###########
  for(sp in explicit_pops){ 
    print(sp)
    totland_thissp <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
    for (sce in others_than_baseline){
      cat (paste(sce, "--------------------------totland_------------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
      namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
      nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ])))))
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ]))))[namesimu1])
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ]))))[namesimu1])
      print(t.test(dd1,dd2))
      ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='') ]))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,paste('pop.', sp, sep='')]))))[namesimu1] *100) -100
      if(all(is.na(ratio_percent))) ratio_percent[] <- 0
      if(is.infinite(sum(ratio_percent,na.rm=TRUE))) ratio_percent[] <- 0
      ratio_percent <- ratio_percent[!is.na(ratio_percent)]
      # ratio_percent <- ratio_percent[ratio_percent<2000]
      a_mean        <- mean(ratio_percent) 
      a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      totland_thissp[sce,"mean"] <- signif(a_mean,5)
      totland_thissp[sce,"a_CI"] <- signif(a_CI,3)
      outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=paste('totland_', sp, sep='') )) 
    }
    print(totland_thissp)
  }
  
  
  
  # revenue
  #############
  revenue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "--------------------------revenue------------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'rev_from_av_prices'])))))
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1])
    print(t.test(dd1,dd2))
    ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'rev_from_av_prices']))))[namesimu1] *100) -100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    #ratio_percent <- ratio_percent[ratio_percent<500]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    revenue[sce,"mean"] <- signif(a_mean,5)
    revenue[sce,"a_CI"] <- signif(a_CI,3)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="revenue")) 
  }
  print(revenue)
  
  
  
  # swept area
  #############
  sweptarea <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "--------------------------sweptarea------------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'sweptr'])))))
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'sweptr']),na.rm=TRUE)))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1])
    print(t.test(dd1,dd2))
    ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'sweptr']))))[namesimu1] *100) -100
    if(all(is.na(ratio_percent))) ratio_percent[] <- 1 # when sweptr is is irrelevant
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<500]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    sweptarea[sce,"mean"] <- signif(a_mean,5)
    sweptarea[sce,"a_CI"] <- signif(a_CI,3)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="sweptarea")) 
  }
  print(sweptarea)
  
  ## revenue per swept area
  # revenuepersweptarea <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  # for (sce in others_than_baseline){
  #  cat (paste(sce, "--------------------------revpersweptarea------------------------------------\n"))
  #      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
  #      nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'revpersweptarea'])))))
  #      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1])
  #      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1])
  #      print(t.test(dd1,dd2))
  #      ratio_percent <-  (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'revpersweptarea']))))[namesimu1] *100) -100
  #      ratio_percent <- ratio_percent[!is.na(ratio_percent)]
  #      ratio_percent <- ratio_percent[ratio_percent<500]
  #      a_mean        <- mean(ratio_percent) 
  #      a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
  #     revenuepersweptarea[sce,"mean"] <- signif(a_mean,5)
  #    revenuepersweptarea[sce,"a_CI"] <- signif(a_CI,3)
  #    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="revenuepersweptarea")) 
  # }
  # print(revenuepersweptarea)
  
  
  # fuel cost
  #############
  fuelcost <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "----------------------------fuelcost----------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'fuelcost'])))))
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1])
    print(t.test(dd1,dd2))
    ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'fuelcost']))))[namesimu1] *100 )-100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<500]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    fuelcost[sce,"mean"] <- signif(a_mean,4)
    fuelcost[sce,"a_CI"] <- signif(a_CI,4)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="fuelcost")) 
  }
  print(fuelcost)
  
  
  #  gradva
  ##########
  gav <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "-------------------------gradva-------------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'gradva']))))
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1])
    print(t.test(dd1,dd2))
    ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']))))[namesimu1] *100 )-100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<500]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    gav[sce,"mean"] <- signif(a_mean,4)
    gav[sce,"a_CI"] <- signif(a_CI,4)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="gav")) 
  }
  print(gav)
  
  
  #  npv net present value NPV assuming 4% discount rate i.e. NPV= Ry1/(1+0.04)^1 + Ry2/(1+0.04)^2 +... 
  ######################################################################################################
  nbyears         <- nrow(lst_loglike_w_agg_all_1[[1]])/12 # because monthly data
  discount_factor <- sapply(rep(paste("1/((1+0.04)^",1:nbyears,")"), each=12), function(x) eval(parse(text=x)))
  
  npv <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "----------------------------npv----------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'gradva']))))
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva']) *discount_factor      )))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva']) *discount_factor      )))[namesimu1])
    print(t.test(dd1,dd2))
    ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'gradva'])*discount_factor)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'gradva'])*discount_factor)))[namesimu1] *100 )-100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<500]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    npv[sce,"mean"] <- signif(a_mean,4)
    npv[sce,"a_CI"] <- signif(a_CI,4)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="npv")) 
  }
  print(npv)
  
  
  # av_vapuf
  ############
  av_vpuf <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "------------------------------av_vapuf--------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,'vapuf'], na.rm=TRUE)))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'vapuf'], na.rm=TRUE)))[namesimu1])
    print(t.test(dd1,dd2))
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'vapuf'], na.rm=TRUE))))
    ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'vapuf'], na.rm=TRUE)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'vapuf'], na.rm=TRUE)))[namesimu1] *100 )-100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    #ratio_percent <- ratio_percent[ratio_percent<500]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    av_vpuf[sce,"mean"] <- signif(a_mean,4)
    av_vpuf[sce,"a_CI"] <- signif(a_CI,4)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_vpuf")) 
  }
  print(av_vpuf)
  
  
  # av_vapuf_month
  #################
  av_vapuf_month <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "----------------------------av_vapuf_month----------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,'av_vapuf_month'], na.rm=TRUE)))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,'av_vapuf_month'], na.rm=TRUE)))[namesimu1])
    print(t.test(dd1,dd2))
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_vapuf_month'], na.rm=TRUE))))
    ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,'av_vapuf_month'], na.rm=TRUE)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,'av_vapuf_month'], na.rm=TRUE)))[namesimu1] *100 )-100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    #ratio_percent <- ratio_percent[ratio_percent<500]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    av_vapuf_month[sce,"mean"] <- signif(a_mean,4)
    av_vapuf_month[sce,"a_CI"] <- signif(a_CI,4)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="av_vpuf_month")) 
  }
  print(av_vapuf_month)
  
  
  # trip-based CPUEs (useless because somehow redundant with vpuf)
  ###################################################################
  cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "------------------------------trip_based_cpue--------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1])
    print(t.test(dd1,dd2))
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland']))/sum(x[,'effort']))))
    ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland']))/sum(x[,'effort'])))[namesimu1] *100 )-100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    #ratio_percent <- ratio_percent[ratio_percent<200]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    cpue[sce,"mean"] <- signif(a_mean,4)
    cpue[sce,"a_CI"] <- signif(a_CI,4)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="trip_based_cpue")) 
  }
  print(cpue)
  
  
  # fishing-based CPUEs
  ######################
  if(any(lst_loglike_w_agg_all_1[[1]]$totland!=0)) {
    cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
    for (sce in others_than_baseline){
      cat (paste(sce, "------------------------------cpue_totland--------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
      namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
      print(t.test(dd1,dd2))
      nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_implicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
      ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] *100 )-100
      ratio_percent <- ratio_percent[!is.na(ratio_percent)]
      #ratio_percent <- ratio_percent[ratio_percent<400]
      a_mean        <- mean(ratio_percent) 
      a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
      cpue[sce,"a_CI"] <- signif(a_CI,4)
      outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="fishing_based_cpue")) 
    }
    print(cpue)
  }
  
  # fishing-based CPUEs (explicit only)
  #######################################
  cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "-----------------------------cpue_explicit---------------------------------\n"))
    lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
    print(t.test(dd1,dd2))
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
    ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] / 
                        unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(as.numeric(x[,'totland_explicit']))/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] *100 )-100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    #ratio_percent <- ratio_percent[ratio_percent<600]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    cpue[sce,"mean"] <- signif(a_mean,4)
    cpue[sce,"a_CI"] <- signif(a_CI,4)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="fishing_based_cpue_explicit")) 
  }
  print(cpue)
  
  
  
  # fishing-based CPUEs 
  ########################
  for(sp in explicit_pops){ 
    print(sp)
    cpue <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
    for (sce in others_than_baseline){
      cat (paste(sce, "--------------------------------cpue sp------------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
      namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  sum(x[,paste('pop.',sp,sep='') ])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1])
      print(t.test(dd1,dd2))
      nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming'])))))
      ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) sum(x[,paste('pop.',sp,sep='')])/(sum(x[,'effort'])- sum(x[,'cumsteaming']))))[namesimu1] *100 )-100
      if(all(is.na(ratio_percent))) ratio_percent[] <- 0
      ratio_percent <- ratio_percent[!is.na(ratio_percent)]
      if(is.infinite(sum(ratio_percent,na.rm=TRUE))) ratio_percent[] <- 0
      #ratio_percent <- ratio_percent[ratio_percent<400]
      a_mean        <- mean(ratio_percent) 
      a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      cpue[sce,"mean"] <- signif(a_mean,4)
      cpue[sce,"a_CI"] <- signif(a_CI,4)
      outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=paste('fishing_based_cpue_',sp,sep='') )) 
    }
    print(cpue)
  } 
  
  
  
  # discard rate disc_rate_
  ############################
  for(sp in explicit_pops){ 
    print(sp)
    disc_rate_thissp <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
    for (sce in others_than_baseline){
      cat (paste(sce, "----------------------------------disc_rate_----------------------------\n"))
      lst_loglike_w_agg_all_2 <- get(paste("lst_loglike_agg_",what2,selected, sce, sep=''))
      namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
      dd1 <- (unlist(lapply(lst_loglike_w_agg_all_1, function (x)  mean(x[,paste('disc_rate_', sp, sep='') ], na.rm=TRUE)))[namesimu1])
      dd2 <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x)  mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE)))[namesimu1])
      if(all(is.na(dd1)))  dd1 [] <-0
      if(all(is.na(dd2)))  dd2 [] <-0
      # print(t.test(dd1,dd2))
      nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE))))
      ratio_percent <- (unlist(lapply(lst_loglike_w_agg_all_2, function (x) mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE)))[namesimu1] / unlist(lapply(lst_loglike_w_agg_all_1, function (x) mean(x[,paste('disc_rate_', sp, sep='')], na.rm=TRUE)))[namesimu1] *100 )-100
      if(all(is.na(ratio_percent)))  ratio_percent [] <-0
      if(is.infinite(sum(ratio_percent,na.rm=TRUE))) ratio_percent[] <- 0
      ratio_percent <- ratio_percent[!is.na(ratio_percent)]
      #ratio_percent <- ratio_percent[ratio_percent<500]
      a_mean        <- mean(ratio_percent) 
      a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
      disc_rate_thissp[sce,"mean"] <- signif(a_mean,4)
      disc_rate_thissp[sce,"a_CI"] <- signif(a_CI,4)
      outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable=paste('disc_rate_', sp, sep=''))) 
    }
    print(disc_rate_thissp)
  }
  
  
  # measure of equity, diversity or distributional profit
  # Shannon index is sum from i=1 to nbVessels of p_i*ln (p_i) with p_i is  the proportion of vessels belonging to the ith class of revenue in the dataset of interest. 
  # nb of classes of revenue to decide upon?
  # but better to use the ginin index or 20:20 Ratio, ...
  #or the the Robin Hood index H (because bwteen 0 to 1 then can be useful for a ratio)
  # http://en.wikipedia.org/wiki/Income_inequality_metrics
  HooverIndex <- matrix(0, ncol=2, nrow=length(others_than_baseline), dimnames=list(others_than_baseline, c("mean","a_CI")))
  for (sce in others_than_baseline){
    cat (paste(sce, "--------------------------------------------------------------\n"))
    lst_loglike_w_agg_vid_2 <- get(paste("lst_loglike_agg_",what2,"_vid_", sce, sep=''))
    namesimu1 <- intersect(names(lst_loglike_w_agg_all_1), names(lst_loglike_w_agg_all_2) )  # debug if missing simus...
    nm            <- names(unlist(lapply(lst_loglike_w_agg_all_2, function (x) sum(x[,'gav']))))
    dd1 <- (unlist(lapply(lst_loglike_w_agg_vid_1, function (x) {                              
      ei <- quantile(x[,'gradva'], prob=seq(0,1,length=21)) 
      ei <- ei[ei!=0] # debug- possibly some revenue at 0 e.g. for early version of hpg_harbknowl_biolsce_Linfs08_M_mig_weight where displacement toward unrealistic fgrounds...                          
      ai <- table(cut(x[,'gradva'], breaks=ei))
      ei <- ei[-1]
      hoover <- 0.5* sum(abs((ei/sum(ei))-(ai/sum(ai))))   #  where 0 indicates perfect equality and 1 (100%) indicates maximum inequality.
    }
    ))[namesimu1])
    dd2 <- (unlist(lapply(lst_loglike_w_agg_vid_2, function (x) {   
      ei <- quantile(x[,'gradva'], prob=seq(0,1,length=21)) 
      ei <- ei[ei!=0] # debug- possibly some revenue at 0 e.g. for early version of hpg_harbknowl_biolsce_Linfs08_M_mig_weight where displacement toward unrealistic fgrounds...                          
      ai <- table(cut(x[,'gradva'], breaks=ei))
      ei <- ei[-1]
      hoover <- 0.5* sum(abs((ei/sum(ei))-(ai/sum(ai))))   #  where 0 indicates perfect equality and 1 (100%) indicates maximum inequality.
      
    }
    ))[namesimu1])
    
    print(t.test(dd1,dd2))
    ratio_percent <- (dd2 /
                        dd1 *100 )-100
    ratio_percent <- ratio_percent[!is.na(ratio_percent)]
    ratio_percent <- ratio_percent[ratio_percent<300]
    a_mean        <- mean(ratio_percent) 
    a_CI          <- 1.96* sqrt(var(ratio_percent)/length(nm))
    HooverIndex[sce,"mean"] <- signif(a_mean,4)
    HooverIndex[sce,"a_CI"] <- signif(a_CI,4)
    outcomes <- rbind.data.frame(outcomes, cbind.data.frame(ratio_percent, simu=names(ratio_percent), scenario=sce, variable="hoover")) 
  }
  print(HooverIndex)
  
  
  
  
  
  # look at the reason to go back.....
  #idx <- rownames(lst_loglike_agg_weight_vid_high_profit_grounds[[3]])
  #lst_loglike_agg_weight_vid_high_profit_grounds_biolsce_mig[[3]] [,c('reason_1', 'reason_2', 'reason_3')] - lst_loglike_agg_weight_vid_high_profit_grounds[[3]][idx, c('reason_1', 'reason_2', 'reason_3')]  
  
  #boxplot(cbind(as.matrix(lst_loglike_agg_weight_vid_area_closure_WMF_NAT2000_and_SMS[[3]] [,c('reason_1', 'reason_2', 'reason_3')]),
  # as.matrix(lst_loglike_agg_weight_vid_high_profit_grounds[[3]][idx, c('reason_1', 'reason_2', 'reason_3')])))
  
  
  #dd <- as.matrix(lst_loglike_agg_weight_vid_area_closure_WMF_NAT2000_and_SMS[[3]] [,c('reason_1', 'reason_2', 'reason_3')])
  #dd2 <- as.matrix(lst_loglike_agg_weight_vid_high_profit_grounds_and_SMS[[3]] [,c('reason_1', 'reason_2', 'reason_3')])
  
  #common <- unique(c(rownames(dd)[rownames(dd)%in% rownames(dd2)], rownames(dd2)[rownames(dd2)%in% rownames(dd)]))
  #comp <- dd[common,]-dd2[common,]
  #comp <- replace(comp, comp==0 ,NA)
  #boxplot(comp)
  
  
  # export
  write.table(outcomes,
              file=file.path(general$main.path, general$namefolderinput, 
                             paste("outcomes_all_simus_relative_to_baseline_sce_",selected, ".txt", sep='')),
              sep=";", quote=FALSE, row.names=FALSE) 
  
} # end for-loop on sets


#  4.4) Make the boxplots
#~~~~~~~~~~~~~~~~~~~~~~~~~

## Define the figure
FIG <- c("main", "suppl")[1] # main = figures displayed on main ms, suppl = figures displayed in the supplementray material

## Define indicators for supplementary material
## Note: figures in supplementary material only explore the landings and catches of other species
ISPPL <- c("catches", "landings")[2]


## Define the desired indicators (economic, biologic, behavioral)
if(FIG == "main"){
  
  #If i want to select only info on cod, choose number "2" -> wbs cod ID in displace
  selected_variables <- c("feffort", #behavioral
                          "seffort", #behavioral
                          "nbtrip",  #behavioral
                          "av_trip_duration", #behavioral
                          
                          "fishing_based_cpue", #biological
                          "totland", #biological
                          "fishing_based_cpue_2", #biological
                          "totland_2", #WBS cod landings; biological indicator
                          
                          "revenue", #economic
                          "npv", #economic
                          #"av_vpuf_month", #economic
                          "av_vpuf",
                          "hoover")#economic
  
} else if(FIG == "suppl" & ISPPL == "landings"){
  
  selected_variables <- c("totland_1",
                          #"totland_2",
                          "totland_3",
                          "totland_11",
                          "totland_12",
                          "totland_22",
                          "totland_23",
                          "totland_31",
                          "totland_35")
  
  
} else if(FIG == "suppl" & ISPPL == "catches"){
  
  selected_variables <- c("fishing_based_cpue_1",
                          #"fishing_based_cpue_2",
                          "fishing_based_cpue_3",
                          "fishing_based_cpue_11",
                          "fishing_based_cpue_12",
                          "fishing_based_cpue_22",
                          "fishing_based_cpue_23",
                          "fishing_based_cpue_31",
                          "fishing_based_cpue_35")
  
}



## Define the vessels (set1 = all vessels, set2 = gillnetters, set3 = trawlers)
#sets <- c("_selected_set1_", "_selected_set2_", "_selected_set3_")
sets <- c("_selected_set2_", "_selected_set3_")



for (selected in sets){
  
  outcomes <- read.table(file.path(general$main.path, general$namefolderinput, 
                                   paste("outcomes_all_simus_relative_to_baseline_sce_",selected, ".txt", sep='')), header=TRUE, sep=";")
  
  
  
  # add baseline at 0,0,0, etc.
  baseline <- outcomes[outcomes$scenario == "scelgnbcouplingwclosure",]  # choose an arbitrary scenario from the scenario list (except not the one used as a baseline!!!)
  baseline$ratio_percent <- 0
  #baseline$scenario <- "scebaseline"
  baseline$scenario <- a_baseline
  outcomes <- rbind.data.frame(baseline, outcomes)
  outcomes$scenario <- factor(outcomes$scenario)
  
  
  
  # Select here the desired indicators (biological, economic and behavioral)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # #If i want to select only info on cod, choose number "2" -> wbs cod ID in displace
  # selected_variables <- c("feffort", #behavioral
  #                         "seffort", #behavioral
  #                         "nbtrip",  #behavioral
  #                         "av_trip_duration", #behavioral
  #                           
  #                         "fishing_based_cpue", #biological
  #                         "fishing_based_cpue_2", #biological
  #                         "totland", #biological
  #                         "totland_2", #WBS cod landings; biological indicator
  #                           
  #                         "revenue", #economic
  #                         "npv", #economic
  #                         #"av_vpuf_month", #economic
  #                         "av_vpuf",
  #                         "hoover")#economic
  
  
  ## Landings and catches for other species - Supplementary Material
  # selected_variables <- c("totland_1",
  #                         "totland_2",
  #                         "totland_11",
  #                         "totland_12",
  #                         "totland_22",
  #                         "totland_23",
  #                         "totland_31",
  #                         "totland_35")
  #                          # "fishing_based_cpue_1",
  #                          # "fishing_based_cpue_2",
  #                          # "fishing_based_cpue_11",
  #                          # "fishing_based_cpue_12",
  #                          # "fishing_based_cpue_22",
  #                          # "fishing_based_cpue_23",
  #                          # "fishing_based_cpue_31",
  #                          # "fishing_based_cpue_35")
  
  
  
  
  outcomes           <- outcomes[outcomes$variable %in% selected_variables,]
  
  
  outcomes$variable <- factor(outcomes$variable)
  
  if(FIG == "main"){
    outcomes$variable <- factor(outcomes$variable, levels=selected_variables, labels= c( "Fishing effort", #CARE!! Same ordering as above
                                                                                         "Steaming effort", 
                                                                                         "Number of trips",
                                                                                         "Trip duration", 
                                                                                         
                                                                                         "Total catches",
                                                                                         "Total landings",
                                                                                         "Cod catches",
                                                                                         "Cod landings",
                                                                                         
                                                                                         "Revenue",
                                                                                         "NPV", 
                                                                                         "VPUF",
                                                                                         "Income inequality"))
    
  } else if(FIG == "suppl" & ISPPL == "landings"){
    outcomes$variable <- factor(outcomes$variable, levels=selected_variables, labels= c(
      "Landings cod21",
      #"Landings cod2224",
      "Landings cod2532",
      "Landings her3a22",
      "Landings her2532",
      "Landings ple2123",
      "Landings ple2432",
      "Landings spr2232",
      "Landings tur2232"))
    
    
    
  } else if(FIG == "suppl" & ISPPL == "catches"){
    outcomes$variable <- factor(outcomes$variable, levels=selected_variables, labels= c("CPUE cod21",
                                                                                        #"CPUE cod2224",
                                                                                        "CPUE cod2532",
                                                                                        "CPUE her3a22",
                                                                                        "CPUE her2532",
                                                                                        "CPUE ple2123",
                                                                                        "CPUE ple2432",
                                                                                        "CPUE spr2232",
                                                                                        "CPUE turbot2232"))
    
    
  }
  
  
  # CARE: needs to be the same as in the general settings in the start of the script!!!!
  # selected_scenarios <-    c("scebaselinenoclosure",
  #                             "scebaseline",
  #                             "scenbcpcouplingnoclosure",
  #                             "scenbcpcoupling",
  #                             "scenbcpcouplingspw",
  #                             "scenbcpcouplingrec")
  
  selected_scenarios <- dput(names(general$namesimu)) #scelgnbcouplingnoclosure, scenbcpcouplingnoclosure
  
  
  # CARE: needs to be the same as in the general settings in the start of the script!!!!
  # name_scenarios <- c("No closure", 
  #                     "Standard spawning closure", 
  #                     "Alternative spawning closure", 
  #                     "Nursery closure"
  #                    )                          
  
  name_scenarios <- dput(the_scenarios1)
  
  outcomes <- outcomes[outcomes$scenario %in%selected_scenarios,]
  outcomes$scenario <- factor(outcomes$scenario)
  outcomes$scenario <- factor(outcomes$scenario, levels = selected_scenarios, labels =  name_scenarios)
  
  #library(lattice)                            
  #bwplot(ratio_percent~variable| scenario, data=outcomes)
  
  
  ## rename vessel selection for clearer understanding
  vessels <- ifelse(selected == "_selected_set1_", "AllVessels",
                    ifelse(selected == "_selected_set2_", "Gillnetters", "Trawlers"))
  
  
  #namefile       <- paste(paste("indicators_boxplot_persce_",selected, sep=""))
  
  if(FIG == "main"){
    namefile       <- paste(paste("indicators_boxplot_persce_",vessels, sep=""))
    
  } else if(FIG == "suppl" & ISPPL == 'landings'){
    namefile       <- paste(paste("SupplementaryMaterial","Biological_indicators", ISPPL, vessels, sep="_"))
    
  } else if(FIG == "suppl" & ISPPL == 'catches'){
    namefile       <- paste(paste("SupplementaryMaterial","Biological_indicators",ISPPL, vessels, sep="_"))
  }
  
  output.folder  <- file.path(general$main.path, general$namefolderinput)
  the_dim        <- c(30, 10)
  
  png(filename=file.path(output.folder, paste(namefile, ".png", sep="" )),
       width = the_dim[1], height = the_dim[2], 
       units = "cm", pointsize = 12,  res=450)
  
  library(ggplot2)
  library(dplyr)
  
  ## CHECK - Why did Francois recategorize these values?
  #outcomes[outcomes$ratio_percent< -25, "ratio_percent"] <- -25
  #outcomes[outcomes$ratio_percent>25, "ratio_percent"] <- 25
  
  
  # p <- ggplot(outcomes[outcomes$ratio_percent>=-25 & outcomes$ratio_percent<=25,], aes(factor(variable), ratio_percent))  + geom_boxplot(outlier.shape=1)  + 
  #   labs(x = "Indicators", y = "% ratio over the baseline") # + ylim(-20, 20) 
  # print(
  # #  p   + facet_wrap( ~ scenario, ncol=2, scales="free_y")    + theme_bw()+ # For DISPLACE vs LGNB-DISPLACE scenarios
  #     p   + facet_wrap( ~ scenario, ncol=4)    + theme_bw()+ # For alternative closure scenarios
  #     theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1), strip.text.x =element_text(size =10),  panel.grid.major = element_line(colour = grey(0.4),linetype = 3 ),
  #           strip.background = element_blank(),
  #           panel.border = element_rect(colour = "black")) + 
  #     geom_abline(intercept=0, slope=0, color="#0073C2FF", lty=2, lwd=1)  + geom_boxplot(outlier.shape=NA)
  # )
  
  
  print(outcomes %>% 
          # filter(!(scenario %in% "Baseline")
          #                   & ratio_percent>=-25 &  ratio_percent<=25) %>% 
          filter(!(scenario %in% "Baseline")) %>% 
          #filter(!(scenario %in% c("Baseline", "Spawning area closure", "Nursery area closure","Old spawners area closure",  "Feeding area closure"))) %>% 
          #filter(scenario %in% c("Seasonal Spawning Closure", "Spawning Area Closure + ITQ","Nursery Area Closure + ITQ", "Old Spawner Area Closure + ITQ", "Feeding Area Closure + ITQ")) %>% 
          
          #filter(scenario %in% c("Baseline", "Seasonal Spawning Closure", "Seasonal Spawning Closure")) %>% 
          
          ggplot( aes(factor(variable), ratio_percent))  +
          geom_boxplot(outlier.shape=1)  + 
          geom_boxplot(outlier.shape=NA) +
          labs(x = "Indicators", y = "% ratio over the baseline") + # + ylim(-20, 20) 
          facet_wrap( ~ scenario, ncol=5) + 
          geom_abline(intercept=0, slope=0, color="#0073C2FF", lty=2, lwd=1)  +
          theme_bw()+ # For alternative closure scenarios
          theme(axis.text.x = element_text(angle = 90, vjust=0.5,hjust=1), 
                strip.text.x =element_text(size =10),  
                panel.grid.major = element_line(colour = grey(0.4),linetype = 3 ),
                strip.background = element_blank(),
                panel.border = element_rect(colour = "black")) 
  )
  dev.off()
  
} # end FOR-loop over sets

