
  general <- list()
  general$case_study <- "BalticSea"

 if(.Platform$OS.type == "unix") {}
  general$main.path         <- file.path("~","ibm_vessels","DISPLACE_outputs")
  general$main.path.igraph  <- file.path("~","ibm_vessels","DISPLACE_input_raw", "igraph")
  general$main.path.param   <- file.path("~","ibm_vessels", paste("DISPLACE_input_",general$case_study, sep=""))
  general$main.path.ibm     <- file.path("~","ibm_vessels", paste("DISPLACE_input_", general$case_study, sep=''))
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
                                 "scerestrictionsonnets",
                                 "scerestrictionontrawling1eez",
                                 "scerestrictionontrawling5eez",
                                 "scerestrictionontrawling10eez",
                                 "scerestrictionontrawling15eez",
                                 "scerestrictionontrawling20eez",
                                 "scerestrictionontrawling25eez",
                                 "scerestrictionontrawling30eez",
                                 "scerestrictionontrawling50eez",
                                 "scerestrictionontrawling30eezH",                                
                                 "scerestrictionontrawling50eezH",                                
                                 "scerestrictionontrawling10eez10lesstrip",                                
                                 #"scerestrictionontrawling20eez20lesstrip",                                
                                 #"scerestrictionontrawling30eez30lesstrip",                                
                                 #"scerestrictionontrawling1hab",
                                 #"scerestrictionontrawling5hab",
                                 #"scerestrictionontrawling10hab",
                                 #"scerestrictionontrawling15hab",
                                 #"scerestrictionontrawling20hab",
                                 #"scerestrictionontrawling25hab",
                                 "scerestrictionontrawling30hab",
                                 #"scerestrictionontrawling50hab"
                                 #"scerestrictionsonnetsandtrawl15hab",
                                 #"scerestrictionsonnetsandtrawl15eez",
                                 #"scerestrictionsonnetsandtrawl20hab",
                                 #"scerestrictionsonnetsandtrawl20eez",
                                 #"scerestrictionsonnetsandtrawl25eez",
                                 #"scerestrictionsonnetsandtrawl25hab",
                                 "scerestrictionsonnetsandtrawl30eez"
                                 #"scerestrictionsonnetsandtrawl30hab"
                                 )    
     general$namesimu           <- list(
                                 "scebaseline"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnets"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling1eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling5eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling10eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling15eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling20eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling25eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling30eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling50eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling30eezH"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling50eezH"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling10eez10lesstrip"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling20eez20lesstrip"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling30eez30lesstrip"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling1hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling5hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling10hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling15hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling20hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling25hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling30hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionontrawling50hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl15hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl15eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl20hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl20eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl25eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl25hab"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl30eez"=   paste("simu", c(1:10), sep=''),
                                 "scerestrictionsonnetsandtrawl30hab"=   paste("simu", c(1:10), sep='')
                                 )
    

    general$igraph   <- c(
                                 "scebaseline"=100,
                                 "scerestrictionsonnets"=101,
                                 "scerestrictionontrawling5eez"=102,
                                 "scerestrictionontrawling10eez"=103,
                                 "scerestrictionontrawling15eez"=104,
                                 "scerestrictionontrawling20eez"=105,
                                 "scerestrictionontrawling5hab"=106,
                                 "scerestrictionontrawling10hab"=107,
                                 "scerestrictionontrawling15hab"=108,
                                 "scerestrictionontrawling20hab"=109,
                                 #"scerestrictionsonnetsandtrawl15hab"=112,
                                 #"scerestrictionsonnetsandtrawl15eez"=113,
                                 #"scerestrictionsonnetsandtrawl20hab"=110,
                                 #"scerestrictionsonnetsandtrawl20eez"=111,
                                 "scerestrictionontrawling25eez"=114,
                                 "scerestrictionontrawling25hab"=115,
                                 "scerestrictionontrawling30eez"=116,
                                 "scerestrictionontrawling30hab"=117,
                                 "scerestrictionsonnetsandtrawl25eez"=118,
                                 "scerestrictionsonnetsandtrawl25hab"=119,
                                 "scerestrictionsonnetsandtrawl30eez"=120,
                                 "scerestrictionsonnetsandtrawl30hab"=121,
                                 "scerestrictionontrawling50eez"=122,
                                 "scerestrictionontrawling50hab"=123,
                                 "scerestrictionontrawling1eez"=124,
                                 "scerestrictionontrawling1hab"=125,
                                 "scerestrictionontrawling30eezH"=126,
                                 "scerestrictionontrawling50eezH"=127,
                                 "scerestrictionontrawling10eez10lesstrip"=103,                                
                                 "scerestrictionontrawling20eez20lesstrip"=105,                                
                                 "scerestrictionontrawling30eez30lesstrip"=116
                                 )
 

   }






 if(general$case_study=="BalticSea"){

   source(file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis_BalticSea","DISPLACE_R_outputs_ForBalticSea","loadAggLoglikeFiles.R"))
   #loadLoglikeFiles(general=general, use_port_info=TRUE) # retrieve longlat to plot per area....
   loadLoglikeFiles(general=general, use_port_info=FALSE)


 }


  outcome <- data.frame(NULL)
 
#  what2 <- "weight"
#  selected <- "_selected_set1_"     # all 
#  a_unit <- 1e6  # millions euros
#  for (sce in general$namefolderoutput)
#  {
#    lst_loglike_w_agg_all <- get(paste("lst_loglike_agg_",what2, selected, sce, sep=''))
    #lst_loglike_w_agg_all <- get(paste("lst_loglike_agg_",what2, "_vid_port_", sce, sep=''))
   
#    a_var <- "gradva"
#    a_var_name <- "Margin contribution (All)"
#    for(sim in general$namesimu[[sce]])
#    {
#        if(!is.null(lst_loglike_w_agg_all[[sim]]))
#          outcome <- rbind.data.frame(outcome,
#                        #data.frame(sce=rep(sce,12*5), sim=rep(sim, 12*5), time=lst_loglike_w_agg_all[[sim]]$year.month, var=a_var_name, value=(lst_loglike_w_agg_all[[sim]][,a_var]/a_unit))
#                        data.frame(sce=rep(sce,12), sim=rep(sim, 12), time=tail(lst_loglike_w_agg_all[[sim]]$year.month, 12), var=a_var_name, value=tail(lst_loglike_w_agg_all[[sim]][,a_var]/a_unit, 12))
#                        )
#    }
#  }
  
  #plot(outcome[outcome$sim=="simu1" & outcome$sce=="scebaseline","value"])

   # convert to UTM
  library(sp)
  library(rgdal)
  UTMzone  <- 32
 
   feffort_cut_per_eez_1               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','feffort_cut_per_eez_1'), 
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))
   feffort_cut_per_eez_1                <-  spTransform(feffort_cut_per_eez_1, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   area1 <- area(feffort_cut_per_eez_1)
   feffort_cut_per_eez_5               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','feffort_cut_per_eez_5'), 
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))
   feffort_cut_per_eez_5                <-  spTransform(feffort_cut_per_eez_5, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   area5 <- area(feffort_cut_per_eez_5)
   feffort_cut_per_eez_10               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','feffort_cut_per_eez_10'), 
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))
   feffort_cut_per_eez_10                <-  spTransform(feffort_cut_per_eez_10, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   area10 <- area(feffort_cut_per_eez_10)
   feffort_cut_per_eez_15               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','feffort_cut_per_eez_15'), 
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))
   feffort_cut_per_eez_15                <-  spTransform(feffort_cut_per_eez_15, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   area15 <- area(feffort_cut_per_eez_15)
   feffort_cut_per_eez_20               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','feffort_cut_per_eez_20'), 
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))
   feffort_cut_per_eez_20                <-  spTransform(feffort_cut_per_eez_20, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   area20 <- area(feffort_cut_per_eez_20)
   feffort_cut_per_eez_25               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','feffort_cut_per_eez_25'), 
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))
   feffort_cut_per_eez_25                <-  spTransform(feffort_cut_per_eez_25, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   area25 <- area(feffort_cut_per_eez_25)
   feffort_cut_per_eez_30               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','feffort_cut_per_eez_30'), 
                                                             proj4string=CRS("+proj=longlat +ellps=WGS84"))
   feffort_cut_per_eez_30                <-  spTransform(feffort_cut_per_eez_30, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   area30 <- area(feffort_cut_per_eez_30)
   feffort_cut_per_eez_50               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','feffort_cut_per_eez_50'), 
                                                             proj4string=CRS("+proj=longlat +ellps=WGS84")) 
   feffort_cut_per_eez_50                <-  spTransform(feffort_cut_per_eez_50, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   area50 <- area(feffort_cut_per_eez_50)
   feffort_cut_per_eez_50H               <- readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','feffort_cut_per_eez_50_highest'), 
                                                             proj4string=CRS("+proj=longlat +ellps=WGS84")) 
   feffort_cut_per_eez_50H                <-  spTransform(feffort_cut_per_eez_50H, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   area50H <- area(feffort_cut_per_eez_50)
 HELCOMbirdsBreedingAreas1_wgs84       <-   readShapePoly(file.path('C:','Users','fbas','Documents','GitHub',
                                                            'DISPLACE_input_gis_BalticSea','MANAGEMENT','HELCOMbirdsBreedingAreas1_wgs84'), 
                                                          proj4string=CRS("+proj=longlat +ellps=WGS84"))
   HELCOMbirdsBreedingAreas1_wgs84                <-  spTransform(HELCOMbirdsBreedingAreas1_wgs84, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    # convert to UTM
   areaNat2000 <- sum(area(HELCOMbirdsBreedingAreas1_wgs84))
  
   area_km2_restrictions <- cbind(percentCut=c(1,5,10,15,20,25,30,50), areaCut=c(area1, area5, area10,area15,area20,area25,area30, area50)/1e6)
   
   # save informative plot
   output.folder  <- file.path(general$main.path, general$namefolderinput)
   namefile       <- paste0("surface_area_effort_cuts.tiff")
   tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )), 
              width = the_dim[1], height = the_dim[2],
              units = "px", pointsize = 12,  res=300, compression=c("lzw"))
   plot(area_km2_restrictions,  pch=16, xlab=("% effort cut and displaced"), ylab="restricted surface area (km^2)")
   lines(area_km2_restrictions, lwd=2, pch=16)
   dev.off() 
  
  

get_estimates_of_spatial_extent_visited <- function(){
 
  nametype <- "cumftime"
  a_pop <- ""
  field_pos <- 4
  the_breaks_baseline<-  c(0, round(exp(seq(-1.5, 3.5, by=0.3)),1), 10000)
  func <- ""
  grid_degrees <- 0.05 # 3 minutes
  grid_degrees <- 0.05 # 3 minutes
  
  xlims  <- c(7,25)
  ylims  <- c(53,66)

 spatial_extent <- NULL
  for (sce in general$namefolderoutput)
  {  
   #-------------------------
   my_data <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_",nametype,"_layer",a_pop,".txt", sep='')), header=FALSE, skip = 1)
    colnames(my_data) <- c("node","lat",  "long")
    colnames(my_data) [field_pos] <- paste0(nametype, a_pop)
    nametype <- paste0(nametype, a_pop)
 
   #-------------------------
   library(vmstools) # for c_square
   my_data$c_square <- vmstools::CSquare (lon=my_data[,'long'], lat=my_data[,'lat'], degrees=grid_degrees)

   # then, aggregate the data per c_square...
   a_func           <- "mean"
   my_data           <- aggregate(my_data[,nametype], list(my_data$c_square), a_func, na.rm=TRUE)
   colnames(my_data) <- c("c_square", nametype)
   my_data          <- cbind.data.frame(my_data, CSquare2LonLat(my_data$c_square, grid_degrees)) # get the mid point coordinates

   colnames(my_data) <- c("c_square", nametype, "mid_lat", "mid_lon")

 

   my_data[,nametype] <- replace (my_data[,nametype],
                                                 my_data[,nametype]>the_breaks_baseline[length(the_breaks_baseline)],
                                                 the_breaks_baseline[length(the_breaks_baseline)])


   my_data <- my_data[!is.na(my_data$mid_lat),] # remove failure

   my_data$cell_area <- (cos(my_data$mid_lat *pi/180) * 111.325 )*3/60  * (111*3/60) # 0.05 degree is 3 minutes

   if(!func %in% c("rate", "no_density")) my_data[,nametype]  <- round(my_data[,nametype])  / my_data$cell_area

   head(my_data[!is.na(my_data[, nametype]),])

   # get the spatial extent of the fishing effort distribution!!
   where_there_is_effort <- my_data[!is.na(my_data[, nametype]),]
   where_there_is_effort <- where_there_is_effort [as.numeric(as.character(where_there_is_effort[, nametype]))>10,] # cuation: a threshold in hour here. And also scale dependent (here grid 0.05 degress)....
   where_there_is_effort <- where_there_is_effort[!duplicated(where_there_is_effort$c_square),]
   spatial_extent <- rbind.data.frame(spatial_extent,
                         cbind.data.frame(sce=sce, spatial_extent_km2= sum(where_there_is_effort[, "cell_area"]))
                         )

   } # end sce
   
  # see quick_map for a plot...
   
 return (spatial_extent)
}  
  
  
  
  
get_lasty_estimates_from_loglike <- function(a_var="gradva", 
                                             a_var2="",
                                             a_var_name="Margin contribution",
                                             a_unit=1e6, 
                                             a_unit2="",
                                             suppressed_fields="",
                                             kept_fields=""
                                             )  {

  what2 <- "weight"
  outcome <- data.frame(NULL)
  kept_fields <- c("year.month", kept_fields)
  for (sce in general$namefolderoutput)
  {  
    selected <- "_selected_set1_"     # all 
    
    for(seg in c("All", "Netters", "Trawlers"))
    {
  
       if(seg=="All") selected <- "_selected_set1_"    
       if(seg=="Netters") selected <- "_selected_set2_"    
       if(seg=="Trawlers") selected <- "_selected_set3_"     
    
       lst_loglike_w_agg_all <- get(paste("lst_loglike_agg_",what2, selected, sce, sep=''))
      
       var_name <- paste0(a_var_name, "(",seg,")")
       for(sim in general$namesimu[[sce]])
       {
         if(a_var=="totland2" && length(suppressed_fields)>0) {
            suppress <- grep(paste(suppressed_fields, collapse="|"), colnames(lst_loglike_w_agg_all[[sim]]) ) # suppress some fields
            lst_loglike_w_agg_all[[sim]] <- lst_loglike_w_agg_all[[sim]][, -suppress] 
            idxcols <- grep("pop.", colnames(lst_loglike_w_agg_all[[sim]]))   
            lst_loglike_w_agg_all[[sim]] <- cbind.data.frame(lst_loglike_w_agg_all[[sim]], totland2=apply(lst_loglike_w_agg_all[[sim]][,idxcols], 1, sum, na.rm=TRUE))
            } 
        if(a_var=="totland3" && length(kept_fields)>0) {
            kept <- grep(paste(kept_fields, collapse="|"), colnames(lst_loglike_w_agg_all[[sim]]) ) # kept some fields only
            lst_loglike_w_agg_all[[sim]] <- lst_loglike_w_agg_all[[sim]][, kept] 
            idxcols <- grep("pop.", colnames(lst_loglike_w_agg_all[[sim]]))   
            lst_loglike_w_agg_all[[sim]] <- cbind.data.frame(lst_loglike_w_agg_all[[sim]], totland3=apply(lst_loglike_w_agg_all[[sim]][,idxcols], 1, sum, na.rm=TRUE))
            }
        if(a_var=="totdisc")
            {
            idxcols <- grep("disc.", colnames(lst_loglike_w_agg_all[[sim]]))      
            lst_loglike_w_agg_all[[sim]] <- cbind.data.frame(lst_loglike_w_agg_all[[sim]], totdisc=apply(lst_loglike_w_agg_all[[sim]][,idxcols], 1, sum, na.rm=TRUE))
            }
            
       
        if(a_var2=="" && !is.null(lst_loglike_w_agg_all[[sim]]))
          outcome <- rbind.data.frame(outcome,
                        #data.frame(sce=rep(sce,12*5), sim=rep(sim, 12*5), time=lst_loglike_w_agg_all[[sim]]$year.month, var=var_name, value=(lst_loglike_w_agg_all[[sim]][,a_var]/a_unit))
                        data.frame(activity=seg, sce=rep(sce,12), sim=rep(sim, 12), time=tail(lst_loglike_w_agg_all[[sim]]$year.month, 12), var=var_name, 
                                   value=tail(lst_loglike_w_agg_all[[sim]][,a_var]/a_unit, 12))
                        )
        if(a_var2!="" && !is.null(lst_loglike_w_agg_all[[sim]]))
          outcome <- rbind.data.frame(outcome,
                        data.frame(activity=seg, sce=rep(sce,12), sim=rep(sim, 12), time=tail(lst_loglike_w_agg_all[[sim]]$year.month, 12), var=var_name, 
                                   value=tail((lst_loglike_w_agg_all[[sim]][,a_var]/a_unit)/(lst_loglike_w_agg_all[[sim]][,a_var2]/a_unit2), 12) )
                        )
        } # end sim    
      }  # end seg
    } # end sce
    
  return(outcome)
}


  # CALLS
  outcome_landvol   <-  get_lasty_estimates_from_loglike(a_var="totland2", a_var_name="Landings volume (thousand tons)", a_unit=1e6, suppressed_fields=c("pop.35", "pop.36"))    # thousand tons
  outcome_bycatch   <-  get_lasty_estimates_from_loglike(a_var="totland3", a_var_name="Bycatch (thousand tons)", a_unit=1e6, kept_fields=c("pop.35", "pop.36"))    
  outcome_disc      <-  get_lasty_estimates_from_loglike(a_var="totdisc", a_var_name="Discards (thousand tons)", a_unit=1e6)    
  outcome_income    <-  get_lasty_estimates_from_loglike(a_var="rev_from_av_prices", a_var_name="Income from landings (millions euros)", a_unit=1e6)    # millions euros
  outcome_gva       <-  get_lasty_estimates_from_loglike(a_var="gradva", a_var_name="Margin contribution (millions euros)", a_unit=1e6)    # millions euros
  outcome_vpuf      <-  get_lasty_estimates_from_loglike(a_var="av_vapuf_month", a_var_name="VPUF (Euros per litre)", a_unit=12)    # euros per litre fuel /12 (divide by 12 bc will be summed over month later on)
  outcome_fuelcost  <-  get_lasty_estimates_from_loglike(a_var="fuelcost", a_var_name="Fuel cost (millions euros)", a_unit=1e6)    # euros per litre fuel /12 (divide by 12 bc will be summed over month later on)
  outcome_sweptr    <-  get_lasty_estimates_from_loglike(a_var="sweptr", a_var_name="Swept area (km2)", a_unit=1e6)    #  km2
  outcome_spatfoot1 <-  get_lasty_estimates_from_loglike(a_var="totland2", a_var2="sweptr", a_var_name="Spatial footprint (volume kg per km2)", a_unit=1,  a_unit2=1e6,  suppressed_fields=c("pop.35", "pop.36"))    # kg per are swept km2
  outcome_spatfoot2 <-  get_lasty_estimates_from_loglike(a_var="rev_from_av_prices", a_var2="sweptr", a_var_name="Spatial footprint (euro per km2)", a_unit=1,  a_unit2=1e6)    # euros per km2
  
  outcome <- rbind.data.frame(outcome_landvol,
                              outcome_bycatch,
                              outcome_disc,
                              outcome_income,
                              outcome_gva,
                              outcome_fuelcost,
                              outcome_vpuf,
                              outcome_sweptr,
                              outcome_spatfoot1,
                              outcome_spatfoot2)
  
    
  # squeeze the month dimension
  outcomeLog_lasty <- aggregate(x=outcome$value, by=list(activity=outcome$activity, sce=outcome$sce, sim=outcome$sim, var=outcome$var), FUN=sum, na.rm=TRUE)
  colnames(outcomeLog_lasty)[ncol(outcomeLog_lasty)] <- "value"

  # rename some variables
  #levels(outcomeLog_lasty$var) [levels(outcomeLog_lasty$var) %in% "totdisc"] <- "Discards"
  #levels(outcomeLog_lasty$var) [levels(outcomeLog_lasty$var) %in% "bycatch"] <- "Unwanted bycatch"
  #levels(outcomeLog_lasty$var) [levels(outcomeLog_lasty$var) %in% "totland"] <- "Landings"
  #levels(outcomeLog_lasty$var) [levels(outcomeLog_lasty$var) %in% "rev_from_av_prices"]  <- "Incomes from landings"
  #levels(outcomeLog_lasty$var) [levels(outcomeLog_lasty$var) %in% "gradva"]  <- "Margin contribution"


  # add RBS
  # read spatial layers with subregions e.g. ices areas? 
  library(rgdal)
  library(maptools)
  # read shape file
  shp  <- readOGR(file.path("C:","Users","fbas","Documents","GitHub","DISPLACE_input_gis_BalticSea","MANAGEMENT","ices_areas", paste0("ices_areas_baltic",".shp") ))
  if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

  head(shp@data)
  namevar <- "ICES_area"


   # get the baseline info 
   thisbaseline <- read.table(file=file.path(general$main.path, general$namefolderinput, "scebaseline",
                              paste("average_cumbenthos_layer.txt", sep='')), header=FALSE, skip = 1)
   thisbaseline <- cbind.data.frame(thisbaseline, "all")
   colnames(thisbaseline) <- c("node","lat",  "long", "cumbenthos", "funcgrp")
   rownames(thisbaseline) <- as.character(thisbaseline$node)
   
   # get a map code to add to nodes
   # overlay
   xfield <- "long"
   yfield <- "lat"
   xy <- thisbaseline[, c(xfield,yfield) ] 
   library(sp)
   library(rgdal)
   library(raster)
   spp <- SpatialPoints(cbind(as.numeric(as.character(xy[,1])), as.numeric(as.character(xy[,2]))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
   xy <-  spTransform(spp,  projection(shp))       
   df_query                      <- over(xy, shp)
   thisbaseline                        <- cbind.data.frame(thisbaseline, as.character(df_query[,namevar]))
   colnames(thisbaseline) [ncol(thisbaseline)] <- namevar

   thisbaseline$ICES_area <- as.character(thisbaseline$ICES_area)
   thisbaseline[!complete.cases(thisbaseline), "ICES_area"] <- "Other"
   thisbaseline$ICES_area <- as.factor(thisbaseline$ICES_area)
   area_node_lookup <- thisbaseline [, c("node", "ICES_area")] 


 outcomeRBS1_lasty <- NULL
 outcomeRBS2_lasty <- NULL
 outcomeRBS3_lasty <- NULL
   for(sce in   general$namefolderoutput)
     {
     cat(paste(sce, "...\n"))  
     
     # read in the average map
     this <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                              paste("average_cumbenthos_layer.txt", sep='')), header=FALSE, skip = 1)
     this <- cbind.data.frame(this, "all")
     colnames(this) <- c("node","lat",  "long", "RBS", "funcgrp")
     
     # add the code area
     this <- merge(this, area_node_lookup)
   
     # bind
     this <-cbind.data.frame(this, sce=sce)
    
     # bind
     this1 <- this[this$ICES_area %in% c("22", "24"),]
     #some_nodes <- this[this1$RBS<0.95, "node"]
     #this1 <- this1[this1$node %in% some_nodes,]
     this2 <- this[this$ICES_area %in% c("25", "26"),]
     #some_nodes <- this[this1$RBS<0.95, "node"]
     #this1 <- this1[this1$node %in% some_nodes,]
     this3 <- this[this$ICES_area %in% c("IIIa"),]
     #some_nodes <- this2[this1$RBS<0.95, "node"]
     #this2 <- this2[this2$node %in% some_nodes,]
 
     outcomeRBS1_lasty <- rbind.data.frame(outcomeRBS1_lasty,
                        data.frame(activity="all", sce=rep(sce,nrow(this1)), sim="simu1", var="RBS", value=this1$RBS)
                        ) 
     outcomeRBS2_lasty <- rbind.data.frame(outcomeRBS2_lasty,
                        data.frame(activity="all", sce=rep(sce,nrow(this2)), sim="simu1", var="RBS", value=this2$RBS)
                        ) 
     outcomeRBS3_lasty <- rbind.data.frame(outcomeRBS3_lasty,
                        data.frame(activity="all", sce=rep(sce,nrow(this3)), sim="simu1", var="RBS", value=this3$RBS)
                        ) 
     }  # end for sce
   # rename
   levels(outcomeRBS1_lasty$var) [levels(outcomeRBS1_lasty$var) %in% "RBS"]  <- "Relative Benthos Status (22, 24)"
   levels(outcomeRBS2_lasty$var) [levels(outcomeRBS2_lasty$var) %in% "RBS"]  <- "Relative Benthos Status (25, 26)"
   levels(outcomeRBS3_lasty$var) [levels(outcomeRBS3_lasty$var) %in% "RBS"]  <- "Relative Benthos Status (IIIa)"


   # stack
   outcome_lasty <- rbind.data.frame(outcomeLog_lasty, outcomeRBS1_lasty, outcomeRBS2_lasty, outcomeRBS3_lasty)


 
 # a better plot
  selected_scenarios <-  c("scebaseline"="Baseline",
                           "scerestrictionsonnets"="Restrictions on Nets",
                           "scerestrictionontrawling1eez"="1% cut (EEZ)",
                           "scerestrictionontrawling5eez"="5% cut (EEZ)",
                           "scerestrictionontrawling10eez"="10% cut (EEZ)",
                           "scerestrictionontrawling15eez"="15% cut (EEZ)",
                           "scerestrictionontrawling20eez"="20% cut (EEZ)",
                           "scerestrictionontrawling25eez"="25% cut (EEZ)",
                           "scerestrictionontrawling30eez"="30% cut (EEZ)",
                           "scerestrictionontrawling50eez"="50% cut (EEZ)",
                           "scerestrictionontrawling30eezH"="30% cut (EEZ) core",
                           "scerestrictionontrawling50eezH"="50% cut (EEZ) core",
                           "scerestrictionontrawling10eez10lesstrip"="10% cut (EEZ) + 10% less trip",
                           "scerestrictionontrawling30hab"="30% cut (HAB)",
                           "scerestrictionsonnetsandtrawl30eez"="30% cut (EEZ) + restr.nets"
                           )
                                 
                               
                                 
  outcome_lasty$sce <- factor(outcome_lasty$sce)
  outcome_lasty$init_sce_names <- factor(outcome_lasty$sce)
  outcome_lasty$sce <- factor(outcome_lasty$sce, levels=names(selected_scenarios), labels=  selected_scenarios)


   
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # do a plot
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  
  
  #library(ggplot2)
  #p <- ggplot(outcomeRBS_lasty[outcomeRBS_lasty$var %in% c("Relative Benthos Status"),], aes(x=sce, y=value, color=var)) +
  #geom_boxplot()
  #p

 # gradva
 the_dim        <- c(1400, 2400)
 namefile       <- paste0("responsecurves_gradva_laty_withRBS")
 output.folder  <- file.path(general$main.path, general$namefolderinput)
 tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
                                   width = the_dim[1], height = the_dim[2],
                                   units = "px", pointsize = 12,  res=300, compression=c("lzw"))

 
 library(ggplot2)
   p <- ggplot(outcome_lasty[outcome_lasty$var %in% c("Margin contribution (millions euros)(Trawlers)",
                                 "Margin contribution (millions euros)(Netters)", "Relative Benthos Status (IIIa)",  "Relative Benthos Status (22, 24)", "Relative Benthos Status (25, 26)"),], 
                    aes(x=sce, y=value, color=activity))  + 
                     geom_point(stat="summary", fun.y="mean" )  +
                    geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0.15) + 
                      #+ geom_boxplot(outlier.shape=1)  +
                  labs(x = "Scenario", y = "Final y contribution margin (millions euros), or RBS (0 to 1)")  + facet_wrap( ~ var, ncol=1, scales="free_y")     # + ylim(-20, 20)
 print(
       p   + 
       theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x =element_text(size =10),  panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +
        geom_abline(intercept=0, slope=0, color="grey", lty=2)  #+ geom_boxplot(outlier.shape=NA)
       )

 dev.off()


 # totland and totdiscard and bycatch
 the_dim        <- c(2400, 2400)
 namefile       <- paste0("responsecurves_totland_laty_",selected)
 output.folder  <- file.path(general$main.path, general$namefolderinput)
 tiff(filename=file.path(output.folder, paste(namefile, ".tiff", sep="" )),
                                   width = the_dim[1], height = the_dim[2],
                                   units = "px", pointsize = 12,  res=300, compression=c("lzw"))

 
 library(ggplot2)
   p <- ggplot(outcome_lasty[outcome_lasty$var %in% c("Landings volume (thousand tons)(All)", "Discards (thousand tons)(All)", "Bycatch (thousand tons)(All)"),], 
                aes(x=sce, y=value, color=var))  + geom_boxplot(outlier.shape=1)  +
             labs(x = "Scenario", y = "Value") # + facet_wrap( ~ var, ncol=2, scales="free_y")     # + ylim(-20, 20)
 print(
       p   + 
       theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x =element_text(size =10),  panel.grid.major = element_line(colour = grey(0.4),linetype =3 ),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) +  ylim(0,400) +
        geom_abline(intercept=0, slope=0, color="grey", lty=2)  + geom_boxplot(outlier.shape=NA)
       )

 dev.off()



   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # do traffic light table
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
   # for a traffic lights table
   #ggplot_build(p)$dat
   spatextent <- get_estimates_of_spatial_extent_visited()  
   

   #aggregate per scenario and compute Conidence Interval
   trafficlights    <- tapply(outcome_lasty$value, list(outcome_lasty$sce, outcome_lasty$var), mean, na.rm=TRUE)
   trafficlights    <- cbind.data.frame( Scenario=rownames(trafficlights), trafficlights)
   trafficlights_CI <- tapply(outcome_lasty$value, list(outcome_lasty$sce, outcome_lasty$var), FUN=function(x) {1.96*(sd(x)/sqrt(length(x)))})
   colnames(trafficlights_CI) <- paste0(colnames(trafficlights_CI), ".CI") 
   
   # merge to spatial extent
   spatextent$sce <- factor(spatextent$sce)
   spatextent <- merge(spatextent, data.frame(sce=names(selected_scenarios),name_sce=selected_scenarios))
   trafficlights <- merge(trafficlights, spatextent, by.x="Scenario", by.y="name_sce")
   trafficlights <- trafficlights[, !colnames(trafficlights) %in% "sce" ]
   rownames(trafficlights) <- trafficlights$Scenario
   trafficlights <- trafficlights[rownames(trafficlights_CI),] # reorder!
   
   # compute percent change compared to baseline
   the_baseline <- "Baseline"
   trafficlights_percentchange <-  round((sweep(x=as.matrix(trafficlights[,-1]), 2, as.matrix(trafficlights[trafficlights$Scenario==the_baseline,-1]), FUN="/")*100)  -100, 2)
   colnames(trafficlights_percentchange) <- paste0(colnames(trafficlights_percentchange), ".PercentChange")
   trafficlights <- cbind.data.frame(trafficlights, trafficlights_CI, trafficlights_percentchange) 
   
   # round
   idx_cols_to_not_round <- c("Scenario", grep("Benthos|VPUF", colnames(trafficlights), value = TRUE ))
   trafficlights[,!colnames(trafficlights) %in% idx_cols_to_not_round] <- round(trafficlights[, !colnames(trafficlights) %in% idx_cols_to_not_round], 1)
   
   # export
   namefile       <- paste0("trafficlights_table.csv")
   output.folder  <- file.path(general$main.path, general$namefolderinput) 
   write.table(trafficlights,   file=file.path(output.folder, namefile), col.names=TRUE, row.names=TRUE, sep=";", quote=FALSE)
  
  
    # add the CI manually
   for (a_var in gsub(".CI", "", colnames(trafficlights_CI)))
   {
      cols <- c(a_var , paste0(a_var, ".CI"))
      trafficlights[,a_var] <- apply( round(trafficlights[ , cols ],3) , 1 , paste , collapse = " \u00b1 " )
      trafficlights <- trafficlights[ , !( names( trafficlights ) %in% paste0(a_var, ".CI") ) ]
   }

   # add the .PercentChange manually
   #for (a_var in gsub(".PercentChange", "", colnames(trafficlights_percentchange)))
   #{
   #   cols <- c(a_var , paste0(a_var, ".PercentChange"))
   #   trafficlights[,a_var] <- apply( trafficlights[ , cols ] , 1 , paste , collapse = " (" )
   #   trafficlights <- trafficlights[ , !( names( trafficlights ) %in% paste0(a_var, ".PercentChange") ) ]
   #}

   
   # format and export for ALL
   trafficlights_all <- trafficlights[,  c(
                    "Spatial footprint (volume kg per km2)(All).PercentChange", "spatial_extent_km2.PercentChange",
                    "Relative Benthos Status (22, 24).PercentChange", "Relative Benthos Status (25, 26).PercentChange",
                    "Relative Benthos Status (IIIa).PercentChange", "Margin contribution (millions euros)(All).PercentChange", 
                    "VPUF (Euros per litre)(All).PercentChange"
                    ) ]
  
  
                    
    # export
   namefile       <- paste0("trafficlights_table_ALL.csv")
   output.folder  <- file.path(general$main.path, general$namefolderinput) 
   trafficlights_all <- cbind.data.frame(Scenario=rownames(trafficlights_all), trafficlights_all)
   write.table(trafficlights_all,   file=file.path(output.folder, namefile), col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE)
 
   # format and export for NETTERS
   trafficlights_activity <- trafficlights[, 
                    c("Landings volume (thousand tons)(Netters)", "Income from landings (millions euros)(Netters)", 
                    "Margin contribution (millions euros)(Netters)", "VPUF (Euros per litre)(Netters)", 
                    "Landings volume (thousand tons)(Trawlers)", "Income from landings (millions euros)(Trawlers)", 
                    "Margin contribution (millions euros)(Trawlers)", "VPUF (Euros per litre)(Trawlers)"                    
                    ) ]
                    
    # export
   namefile       <- paste0("trafficlights_table_activity.csv")
   output.folder  <- file.path(general$main.path, general$namefolderinput) 
   trafficlights_activity <- cbind.data.frame(Scenario=rownames(trafficlights_activity), trafficlights_activity)
   write.table(trafficlights_activity,   file=file.path(output.folder, namefile), col.names=TRUE, row.names=FALSE, sep=";", quote=FALSE)

  
  
   #or: useful to copy/paste into Excel!
   #write.table(trafficlights, "clipboard", sep="\t", row.names=TRUE)   # export to excel
