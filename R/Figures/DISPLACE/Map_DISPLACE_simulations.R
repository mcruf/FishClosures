


#' Generate maps from averaging stochastic DISPLACE spatial layers
#'
#' This function generates maps from an average layer as a second step 
#' after call to getAggNodesLayerFiles()
#'


library(rgeos)
library(maps)
library(maptools)
library(rgdal)
library(raster)
library(wesanderson)
library(RColorBrewer)



#### Function ####

mapNodeAverageLayerFiles <- function(general, grid_degrees=0.1, in_relative=FALSE, a_type="cumcatches", a_type2="", func="ratio",    # or func="rate",
                                     field_pos=4, a_pop="", the_baseline= "svana_baseline",
                                     selected_scenarios_for_plot=general$namefolderoutput,
                                     selected_scenarios_for_table=general$namefolderoutput,
                                     namesce=general$namefolderoutput,
                                     selected_areas_for_table=c("22", "23", "24", "25", "IIIa", "IVa", "IVb", "IVc"), #Francois input
                                     #selected_areas_for_table=c("22", "23", "24", "25", "IIIa"), #select only KAT/WBS to test
                                     the_breaks_baseline= c(0.5, 1, round(exp(seq(0.5, 14, by=1.2))), 1000000),
                                     the_breaks=c(rev(-round(exp(seq(0, 7, by=1)))),  0, round(exp(seq(0, 7, by=1)))),
                                     gis_shape=list(),
                                     a_width= 3400, a_height =3500, xlims_crop=c(4, 25), ylims_crop=c (53, 58.5),  xlims=c(7.5, 23), ylims=c (55.5, 56.5), #Francois input
                                     #a_width= 3400, a_height =3500, xlims_crop=c(9, 15.5), ylims_crop=c (53.7, 56.5),  xlims=c(7.5, 23), ylims=c (55.5, 56.5),
                                     legend_text1="Total Catches kg per",
                                     someletters=c("(a)", "(b)"), add_label_axis_1=TRUE, add_label_axis_2=TRUE
){
  
  
  distance <- function (lon, lat, lonRef, latRef)  # vmstools::distance()
  {
    pd <- pi/180
    a1 <- sin(((latRef - lat) * pd)/2)
    a2 <- cos(lat * pd)
    a3 <- cos(latRef * pd)
    a4 <- sin(((lonRef - lon) * pd)/2)
    a <- a1 * a1 + a2 * a3 * a4 * a4
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    return(6371 * c)
  }
  
  legend.gradient2 <-
    function (pnts, cols = heat.colors(100), limits = c(0, 1), title = "Legend", legend="",
              ...)
    {
      pnts = try(as.matrix(pnts), silent = T)
      if (!is.matrix(pnts))
        stop("you must have a 4x2 matrix")
      if (dim(pnts)[1] != 4 || dim(pnts)[2] != 2)
        stop("Matrix must have dimensions of 4 rows and 2 columms")
      if (length(cols) < 2)
        stop("You must have 2 or more colors")
      yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length = length(cols) +
                    1)
      for (i in 1:length(cols)) {
        polygon(x = pnts[, 1], y = c(yvals[i], yvals[i], yvals[i +
                                                                 1], yvals[i + 1]), col = cols[i], border = F)
      }
      text(max(pnts[, 1]), min(pnts[, 2]), labels = limits[1],
           pos = 4, ...)
      text(max(pnts[, 1]), max(pnts[, 2]), labels = limits[2],
           pos = 4, ...)
      start_pos <- (min(pnts[, 2])+((max(pnts[, 2])-min(pnts[, 2]))/length(legend))/2)
      for (i in 1: length(legend)){
        text(max(pnts[, 1])-0, start_pos + ((i-1) * ((max(pnts[, 2])-min(pnts[, 2]))/length(legend)) ), labels = legend[i],
             pos = 4, ...)
        #browser()
      }
      text(min(pnts[, 1])-0.1, max(pnts[, 2])-0.1, labels = title, adj = c(0,
                                                                           -1), ...)
    }
  
  
  # export raster file for GIS engine
  # exportGTiff <- function(
  #   a_raster=rst,
  #   namefile_gtiff= file.path(general$main.path, general$namefolderinput, namefile, paste0("map_averaged_",nametype,"_", plotid)),
  #   a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
  # ) {
  #   require(raster) 
  #   crs(a_raster) <- "+proj=longlat +datum=WGS84"                
  #   rstr_proj       <- projectRaster(a_raster, crs=a_crs)  # e.g. European EEA projection
  #   rstr_proj[is.na(rstr_proj)] <- -999  # arbitrary code, to get rid of true 0s in GIS
  #   #rstr_proj[rstr_proj<0.001]  <- -999
  #   # SHUT DOWN THE R CONSOLE IF A ERROR POPPING UP HERE:
  #   writeRaster(rstr_proj, namefile_gtiff, format = "GTiff", overwrite=TRUE)
  #   return()
  # }
  
  
  
  library(maptools)
  
  
  
  table_obj <- matrix(0, nrow=length(selected_scenarios_for_table), ncol=length(selected_areas_for_table)+1)
  rownames(table_obj) <- c(selected_scenarios_for_table)
  colnames(table_obj) <- c(selected_areas_for_table, "Other")
  
  
  
  if(a_type2!="") nametype <- paste0(paste0(a_type, a_pop),"over",a_type2) else nametype <- paste0(a_type, a_pop)
  namefile  <- file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",nametype,"_selected_in_relative", in_relative, ".png") )
  namefile2 <- file.path(general$main.path, general$namefolderinput, paste0("table_",nametype,".txt") )
  
  
  plotid <- 0
  # tiff(filename=namefile,   width = a_width, height = a_height,
  #      units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  png(filename=namefile,   width = a_width, height = a_height,
       units = "px", pointsize = 12,  res=450)
  
  #if(length(selected_scenarios_for_plot)==3) m <- rbind(c(1, 1), c(1, 1),c(2, 3))
  if(length(selected_scenarios_for_plot)==2) m <- rbind(c(1, 2))
  if(length(selected_scenarios_for_plot)==3) m <- rbind(c(1, 2, 3))
  if(length(selected_scenarios_for_plot)==4) m <- rbind(c(1, 2), c(3,4))
  if(length(selected_scenarios_for_plot)==5) m <- rbind(c(1, 1), c(1, 1),c(2, 3), c(4, 5))
  if(length(selected_scenarios_for_plot)==6) m <- rbind(c(1, 2) ,c(3, 4), c(5, 6))
  if(length(selected_scenarios_for_plot)==7) m <- rbind(c(1, 1), c(1, 1),c(2, 3), c(4, 5),  c(6, 7))
  if(length(selected_scenarios_for_plot)==8) m <- rbind(c(1, 2) ,c(3, 4), c(5, 6), c(7, 8))
  layout(m)
   
  #par(oma=c(4,4,1,1)) #bottom, left, top, right
  #par(mar=c(2,2,3,1))
  
  par(mar=c(3,3,3,3))
  par(oma=c(4,4,4,4))


  
  
  #table_obj <- NULL
  
  count <-0
  for(sce in   selected_scenarios_for_table){
    count <- count+1
    
    plotid <- plotid +1
    
    this <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                      paste("average_",a_type,"_layer",a_pop,".txt", sep='')), header=FALSE, skip = 1)
    colnames(this) <- c("node","lat",  "long")
    colnames(this) [field_pos] <- paste0(a_type, a_pop)
    nametype <- paste0(a_type, a_pop)
    
    # filter out close to 0 values
    if(nametype!="cumbenthos" || nametype!="cumbenthosclass3") this[,nametype]  <- replace(this[,nametype], this[,nametype]<1e-1, 0)
    
    if(a_type2!=""){
      this  <- replace(this, is.na(this), 0)
      this[,a_type]  <- replace(this[,a_type], is.infinite(this[,a_type]), 0)
      this2 <- read.table(file=file.path(general$main.path, general$namefolderinput, sce,
                                         paste("average_",a_type2,"_layer",a_pop,".txt", sep='')), header=FALSE, skip = 1)
      colnames(this2) <- c("node","lat",  "long")
      colnames(this2) [field_pos] <- a_type2
      this2  <- replace(this2, is.na(this2), 0)
      this2[,a_type2]  <- replace(this2[,a_type2], is.infinite(this2[,a_type2]), 0)
      
      # filter out close to 0 values
      this2[,a_type2] <- replace(this2[,a_type2], this2[,a_type2]<1e-1, 0)
      
      this <- merge(this, this2)
      if(func=="ratio") this[,paste0(nametype,"over",a_type2)] <- this [,nametype]/this [,a_type2]  # assuming a ratio
      if(func=="rate") this[,paste0(nametype,"over",a_type2)] <- (this [,nametype])/(this [,nametype]+this [,a_type2])  # assuming a rate
      nametype <- paste0(paste0(nametype,a_pop),"over",a_type2) # rename
    }
    
    
    
    this_for_gis <- this
    if(nametype!="cumbenthos" || nametype!="cumbenthosclass3") this_for_gis[,4] <- ceiling(this_for_gis[,4]) # because weird bug when importing XY data in GIS if not an integer!!!
    write.table(this_for_gis, file=file.path(general$main.path, general$namefolderinput, sce,
                                             paste("average_",nametype,"_layer_",sce,".txt", sep='')), col.names=TRUE, row.names=FALSE)
    
    
    # get an idea per area
    dispo <- require(vmstools)
    if(dispo && TRUE){
      this$SI_LATI <- this$lat
      this$SI_LONG <- this$long
      data(ICESareas)
      this$area <- ICESarea(this, ICESareas, fast=TRUE)
      this$area <- factor(this$area)
      levels(this$area)[! levels(this$area) %in% selected_areas_for_table] <- "Other"
    } else{
      if (is.null(this$area)) {
        this$area <- NA
        warning("No area code found here. Try to install vmstools if within ICES area and re-run, otherwise add an area field by hand to the input file", call. = FALSE)
      }
    }
    table_obj[sce, ] <-  tapply(this [, nametype], this$area, sum, na.rm=TRUE)[colnames(table_obj)]
    
    
    
    this$round_long <- this$long
    this$round_lat  <- this$lat
    
    # transform to density data when necessary    
    this$cell_area <- (cos(this$round_lat *pi/180) * 111.325 )* (grid_degrees*3/0.05)/60  * (111*(grid_degrees*3/0.05)/60) # 0.05 degree is 3 minutes
    if(!func %in% c("rate", "no_density")) this[,nametype]  <- round(this[,nametype])  / this$cell_area
    
    
    this$cell_id <-  paste(this$round_long, this$round_lat, sep="_")
    
    
    
    # baseline
    if(sce == the_baseline) {
      the_baseline_layer <- this
      a_func <- "sum"
      if(nametype=="cumbenthos" || nametype=="cumbenthosclass3")  a_func<- "mean"
      the_baseline_layer <- aggregate(the_baseline_layer[,nametype],
                                      list(the_baseline_layer$round_long, the_baseline_layer$round_lat, the_baseline_layer$cell_id), a_func, na.rm=TRUE)
      colnames(the_baseline_layer) <- c("round_long", "round_lat", "cell_id", nametype)
      
      
      
      
      #the_breaks_baseline <-   c(0.5, 1, round(exp(seq(0.5, 14, by=1.1))), 1000000)
      
      the_baseline_layer[,nametype] <- replace (the_baseline_layer[,nametype], 
                                                the_baseline_layer[,nametype]>the_breaks_baseline[length(the_breaks_baseline)], 
                                                the_breaks_baseline[length(the_breaks_baseline)])
      
      the_points <- tapply(the_baseline_layer[,nametype],
                           list(the_baseline_layer$round_lat, the_baseline_layer$round_long), sum, na.rm=TRUE)
      
      #the_points <- replace (the_points, the_points>the_breaks_baseline[length(the_breaks_baseline)], the_breaks_baseline[length(the_breaks_baseline)])
      
      
      my_data <- this
      
      library(vmstools) # for c_square
      my_data$c_square <- vmstools::CSquare (lon=my_data[,'long'], lat=my_data[,'lat'], degrees=grid_degrees)
      
      # then, aggregate the data per c_square...
      a_func <- "sum"
      if(nametype=="cumbenthos" || nametype=="cumbenthosclass3")  a_func <- "mean"
      my_data           <- aggregate(my_data[,nametype], list(my_data$c_square), a_func, na.rm=TRUE)
      colnames(my_data) <- c("c_square", nametype)
      my_data          <- cbind.data.frame(my_data, CSquare2LonLat(my_data$c_square, grid_degrees)) # get the mid point coordinates
      
      colnames(my_data) <- c("c_square", nametype, "mid_lat", "mid_lon")
      
      
      my_data[,nametype] <- replace (my_data[,nametype],
                                     my_data[,nametype]>the_breaks_baseline[length(the_breaks_baseline)],
                                     the_breaks_baseline[length(the_breaks_baseline)])
      
      my_data <- my_data[!is.na(my_data$mid_lat),] # remove failure
      
      
      
      # ...and map
      # Satellite.Palette.baseline <-colorRampPalette(c("cyan","aquamarine","orange","red"))
      # #if(nametype=="cumbenthos" || nametype=="cumbenthosclass3") Satellite.Palette.baseline <- colorRampPalette(c("red","orange","aquamarine","cyan"))  # reverse palette
      # if(nametype=="cumbenthos" || nametype=="cumbenthosclass3") Satellite.Palette.baseline <-colorRampPalette(c("#053061","aquamarine","orange","#67001f"))
      # if(nametype=="cumcatches") Satellite.Palette.baseline <- colorRampPalette(c("cyan","salmon", "sienna3", "seagreen4"))

      
      # My color palette
      # Satellite.Palette.baseline <-  colorRampPalette(c(wes_palette("Zissou1", 5, type = "discrete")))
      # if(nametype=="cumbenthos" || nametype=="cumbenthosclass3") Satellite.Palette.baseline <-  colorRampPalette(c(wes_palette("Zissou1", 5, type = "discrete")))
      # if(nametype=="cumcatches") Satellite.Palette.baseline <-   colorRampPalette(c(wes_palette("Zissou1", 5, type = "discrete")))
      # 
      
      Satellite.Palette.baseline <-  colorRampPalette(c(rev(brewer.pal(n = 6, name = "RdBu"))))
      if(nametype=="cumbenthos" || nametype=="cumbenthosclass3") Satellite.Palette.baseline <-  colorRampPalette(c(rev(brewer.pal(n = 6, name = "RdBu"))))
      if(nametype=="cumcatches") Satellite.Palette.baseline <-   colorRampPalette(c(rev(brewer.pal(n = 6, name = "RdBu"))))
      
      
      spdata           <- unique(my_data[,c("c_square", "mid_lat", "mid_lon")])  # make sure to remove duplicates...(but should already have vanished from the prior aggregation)
      rownames(spdata) <- spdata$c_square
      
      
      r1 <- rbind(x = c(-1, -1, 1,  1, -1) * grid_degrees/2,
                  y = c(-1,  1, 1, -1, -1) * grid_degrees/2)
      
      library(sp)
      spatialLookup <-
        SpatialPolygons(
          lapply(1:nrow(spdata),
                 function(i) {
                   Polygons(
                     list(
                       Polygon(
                         t(r1 + c(spdata$mid_lon[i], spdata$mid_lat[i]))
                       )
                     ), ID = spdata$c_square[i])
                 }
          )
        )
      
      # now assign data to the columns
      out      <- spatialLookup[my_data$c_square,]
      out$data <- my_data[,nametype]
      
      out$color <- Satellite.Palette.baseline(length(the_breaks_baseline[-1])) [cut(out$data, the_breaks_baseline)]
      
      #plot(out, col =  out$color, border=NA, xlim = xlims, ylim=ylims)   # x and y lims not good.
      #axis(1); axis(2, las=2)
      ly <<- out
      mydat <<- my_data
      
      
      ices_areas <- readShapePoly(file.path("~","Desktop","WBSsimu",'DISPLACE_input_gis_BalticSea',"MANAGEMENT",'ices_areas','ices_areas'),
                                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
      #ices_areas <- ices_areas[ices_areas$ICES_area %in% c("IIIa", "22", "23", "24", "25", "26", "27", "28-1", "28-2", "IVa", "IVb", "IVc"),] #Francoi's original input
      ices_areas <- ices_areas[ices_areas$ICES_area %in% c("IIIa", "22", "23", "24"),]
      

      CP <- as(extent(xlims_crop[1], xlims_crop[2], ylims_crop[1], ylims_crop[2]), "SpatialPolygons")
      ICES_clipped <- gIntersection(ices_areas, CP, byid=TRUE)  # clip
      
      
      
      # coastline cropped
      sh_coastlines        <- readShapePoly(file.path("~","Desktop","WBSsimu","DISPLACE_input_gis_BalticSea","GRAPH","land_around_Baltic_Sea_from_ICES_areas_shape.shp"))
      
      proj4string(sh_coastlines) <- CRS("+proj=longlat +datum=WGS84")
      #CP <- as(extent(4, 25, 53, 58.5), "SpatialPolygons")
      proj4string(CP) <- CRS("+proj=longlat +datum=WGS84")
      library(rgeos)
      sh_coastlines_clipped <- gIntersection(sh_coastlines, CP, byid=TRUE)  # clip
      
      
      library(rgeos)
      out2  <- gIntersection(out, CP, byid=TRUE)  # clip by the CP...
      pid   <- sapply(slot(out2, "polygons"), function(x) slot(x, "ID")) # ...and coerce the SpatialPolygons back to a SpatialPolygonDataframe!!
      p.df  <- data.frame( ID=1:length(out2), row.names = pid)
      out2  <- SpatialPolygonsDataFrame(out2, p.df)
      rownames(my_data) <-  paste(my_data$c_square, "1")
      out2$data  <- my_data[pid, nametype]
      out2$color <- Satellite.Palette.baseline(length(the_breaks_baseline[-1])) [cut(out2$data, the_breaks_baseline)]
      out2$color[is.na(out2$color)] <- Satellite.Palette.baseline(1)[1]
      
      # PLOT
      #plot(sh_coastlines_clipped, xlim=xlims, ylim=ylims)
      plot(out2, col =  out2$color, border=NA,xlim=xlims, ylim=ylims,xaxs="i",yaxs="i")
      plot(sh_coastlines_clipped, col="gray80", xlim=xlims, ylim=ylims,add=T)
      
      plot(ICES_clipped, add=TRUE)
      #text(coordinates(ices_areas)[,1]-0.5, coordinates(ices_areas)[,2], paste(ices_areas$ICES_area), cex=2)
      
      # additional layers plot if  necessary:    
      library(maps)
      # bb <- as(extent(c(xlims,ylims)), "SpatialPolygons")
      # if (!is.null(gis_shape)) for (i in 1:length(gis_shape[[the_baseline]])) {
      #   shp <- gis_shape[[the_baseline]][[i]]
      #   proj4string(bb) <- proj4string(shp)
      #   shp <- gIntersection(shp, bb, byid=TRUE)
      #   #if(i==1) plot(shp, add=TRUE, col=grey(0.96), lwd=0.5, border=TRUE, asp=1.5)
      #   #if(i>1) plot(shp, add=TRUE, density=30, lwd=0.5, border=TRUE, asp=1.5)
      #   
      #   if(i==1) plot(shp, add=TRUE, lwd=2)
      #   if(i>1) plot(shp, add=TRUE, lwd=2)
      # }
      
      
      #plot(NG1,add=T,lwd=2, density=20); plot(NG1,add=T,lwd=3)
      #plot(NG2,add=T,lwd=2,density=20); plot(NG2,add=T,lwd=3)
       

      # plot(SG2,add=T, lwd=2, density=20); plot(SG2,add=T, lwd=3)
      # plot(SG3,add=T, lwd=2, density=20); plot(SG2,add=T, lwd=3)
      
      
      # plot(OSG1,add=T,lwd=2, density=20); plot(OSG1,add=T,lwd=3)
      # plot(OSG2,add=T,lwd=2, density=20); plot(OSG2,add=T,lwd=3)
      # plot(OSG3,add=T,lwd=2, density=20); plot(OSG3,add=T,lwd=3)
      # plot(OSG4,add=T,lwd=2, density=20); plot(OSG4,add=T,lwd=3)
      
      
      #plot(FG1,add=T,lwd=2, density=20); plot(FG1,add=T,lwd=3)
      
      
      xrange <- range(the_baseline_layer$round_long)
      yrange <- range(the_baseline_layer$round_lat)
      
      r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2], res=c(grid_degrees/2, grid_degrees/2), crs=CRS("+proj=longlat +datum=WGS84"))
      some_coords <- SpatialPoints(cbind(lon=the_baseline_layer$round_long, lat=the_baseline_layer$round_lat))
      rstr        <- rasterize(x=some_coords, y=r, field=the_baseline_layer[,nametype], fun=sum) 
      
      # export a raster file
      # require(raster) 
      # crs(rstr) <- "+proj=longlat +datum=WGS84"                
      # a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
      # rstr_proj       <- projectRaster(rstr, crs=a_crs)  # e.g. European EEA projection
      # rstr_proj[is.na(rstr_proj)] <- -999  # arbitrary code, to get rid of true 0s in GIS
      # #rstr_proj[rstr_proj<0.001]  <- -999
      # rstr_proj[rstr_proj< the_breaks_baseline[1]] <- the_breaks_baseline[1]
      # for(int in 1: length(the_breaks_baseline[-1])) {
      #   rstr_proj[rstr_proj>the_breaks_baseline[int] & rstr_proj<the_breaks_baseline[int+1]]  <- the_breaks_baseline[int+1]
      # }
      # namefile_gtiff= file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",nametype,"_", plotid,"_", sce))
      # writeRaster(rstr_proj, namefile_gtiff, format = "GTiff", overwrite=TRUE)
      # 
      
      
      #box()
      mtext(side=3, namesce[count], cex=1.5, line=0.5)
      mtext(side=3, someletters[count], cex=2, line=1, adj=0)
      #axis(1, cex.axis=1.5)
      #axis(2, las=2, cex.axis=1.5)

      # bottomleft
      #x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
      #y = c(ylims[1]+0.5, ylims[1]+3, ylims[1]+3, ylims[1]+0.5)
      x = c(xlims[1]+0.1, xlims[1]+0.3, xlims[1]+0.3, xlims[1]+0.1)
      y = c(ylims[1]+0.5, ylims[1]+2.3, ylims[1]+2.3, ylims[1]+0.5)
      
      
      # topright
      #x = c(xlims_crop[2]-3.7, xlims_crop[2]-3.4, xlims_crop[2]-3.4, xlims_crop[2]-3.7)
      #y = c(ylims_crop[1]+1, ylims_crop[1]+1, ylims_crop[2]-1.8, ylims_crop[2]-1.8)
      
      
      the_breaks_leg <-NULL
      a_title <- legend_text1
      #a_title <- substitute( expression(paste(legend_text1, km^2)), list(legend_text1=legend_text1))
      if(func %in% c("rate", "no_density")) a_title <- legend_text1  # overwrite
      for(i in 1: length(the_breaks_baseline[-1])){ if(the_breaks_baseline[i]>1) {the_breaks_leg[i] <- round(the_breaks_baseline[i])} else{the_breaks_leg[i]<- the_breaks_baseline[i]}}
      legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette.baseline(length(the_breaks_baseline[-1])),
                        limits="", title=eval(a_title),
                        legend= the_breaks_leg,
                        cex=1.3, col="black")
      
      
      
    }   else{ #for all scenarios other than the baseline
      
      
      
      this <- aggregate(this[,nametype], list(this$round_long, this$round_lat, this$cell_id), sum, na.rm=TRUE)
      colnames(this) <- c("round_long", "round_lat", "cell_id", nametype)
      
      # Merge!
      this           <- merge(the_baseline_layer, this, by.x="cell_id", by.y="cell_id")
      # filter for close to 0 values
      if(nametype!="cumbenthos" ) this[,paste0(nametype,".x")] <- replace(this[,paste0(nametype,".x")], this[,paste0(nametype,".x")]<1e-1, 0)
      if(nametype!="cumbenthos") this[,paste0(nametype,".y")] <- replace(this[,paste0(nametype,".y")], this[,paste0(nametype,".y")]<1e-1, 0)
      
      
      
      a_func <- "sum"
      if(nametype=="cumbenthos" || nametype=="cumbenthosclass3")  a_func <- "mean"
      if(in_relative){
        # percent
        this[,nametype]  <- (100* as.numeric(as.character(this[,paste0(nametype,".y")])) / as.numeric(as.character(this[,paste0(nametype,".x")])) )  -100
        # CAUTION!!!!: correct for area with low absolute value to avoid visual effect
        if(!nametype=="cumbenthos"  || nametype!="cumbenthosclass3") this[,nametype] [ this[,paste0(nametype,".x")] <quantile(this[,paste0(nametype,".x")] [ this[,paste0(nametype,".x")] !=0], prob=0.05)]  <- 0
        
        the_points <- tapply( this[,nametype],
                              list(this$round_lat.y, this$round_long.y), a_func)
       #Satellite.Palette <-colorRampPalette(c("cyan","aquamarine","white","yellow","red"))  
        #Satellite.Palette <- colorRampPalette(c(wes_palette("Zissou1", 5, type = "discrete")))
        Satellite.Palette <-  colorRampPalette(c(rev(brewer.pal(n = 5, name = "RdBu"))))
      
      } else{
        # absolute values for this sce
        this[,nametype]  <- this[,paste0(nametype,".y")]
        # CAUTION!!!!: correct for area with low absolute value to avoid visual effect
        #if(!nametype=="cumbenthos" ) this[,nametype] [ this[,paste0(nametype,".x")] <quantile(this[,paste0(nametype,".x")] [ this[,paste0(nametype,".x")] !=0], prob=0.05)]  <- 0
        
        the_points <- tapply(this[,paste0(nametype,".y")],
                             list(this$round_lat.y, this$round_long.y), a_func)
        the_breaks <-  the_breaks_baseline
        Satellite.Palette <- Satellite.Palette.baseline
        
      }
      
      
      the_points <- replace (the_points, the_points>the_breaks[length(the_breaks)], the_breaks[length(the_breaks)])
      
      
      # in ?
      sum(as.numeric(as.character(the_points)), na.rm=TRUE)
      
      
      if(sce %in% selected_scenarios_for_plot){
        
        
        my_data <- this
        
        library(vmstools) # for c_square
        my_data$c_square <- vmstools::CSquare (lon=my_data[,'round_long.y'], lat=my_data[,'round_lat.y'], degrees=grid_degrees)
        
        # then, aggregate the data per c_square...
        my_data           <- aggregate(my_data[,nametype], list(my_data$c_square), a_func, na.rm=TRUE)
        colnames(my_data) <- c("c_square", nametype)
        my_data          <- cbind.data.frame(my_data, CSquare2LonLat(my_data$c_square, grid_degrees)) # get the mid point coordinates
        
        colnames(my_data) <- c("c_square", nametype, "mid_lat", "mid_lon")
        
        my_data[,nametype] <- replace (my_data[,nametype],
                                       my_data[,nametype]>the_breaks[length(the_breaks)],
                                       the_breaks[length(the_breaks)])
        
        my_data <- my_data[!is.na(my_data$mid_lat),] # remove failure
        
        
        # ...and map
        
        spdata           <- unique(my_data[,c("c_square", "mid_lat", "mid_lon")])  # make sure to remove duplicates...(but should already have vanished from the prior aggregation)
        rownames(spdata) <- spdata$c_square
        
        
        r1 <- rbind(x = c(-1, -1, 1,  1, -1) * grid_degrees/2,
                    y = c(-1,  1, 1, -1, -1) * grid_degrees/2)
        
        library(sp)
        spatialLookup <-
          SpatialPolygons(
            lapply(1:nrow(spdata),
                   function(i) {
                     Polygons(
                       list(
                         Polygon(
                           t(r1 + c(spdata$mid_lon[i], spdata$mid_lat[i]))
                         )
                       ), ID = spdata$c_square[i])
                   }
            )
          )
        
        # now assign data to the columns
        out      <- spatialLookup[my_data$c_square,]
        out$data <- my_data[,nametype]
        
        out$color <- Satellite.Palette(length(the_breaks[-1])) [cut(out$data, the_breaks)]
        #out$color <- Satellite.Palette.baseline(length(the_breaks_baseline[-1])) [cut(out$data, the_breaks_baseline)] #From the baseline
        #plot(out, col =  out$color, border=NA, xlim = xlims, ylim=ylims)
        #axis(1); axis(2, las=2)
        
        
        

        ices_areas                  <- readShapePoly(file.path("~","Desktop","WBSsimu",'DISPLACE_input_gis_BalticSea',"MANAGEMENT",'ices_areas','ices_areas'),
                                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
        #ices_areas <- ices_areas[ices_areas$ICES_area %in% c("IIIa", "22", "23", "24", "25", "26", "27", "28-1", "28-2", "IVa", "IVb", "IVc"),] # Francoi's original input
        ices_areas <- ices_areas[ices_areas$ICES_area %in% c("IIIa", "22", "23", "24"),]
        
        
        
        CP <- as(extent(xlims_crop[1], xlims_crop[2], ylims_crop[1], ylims_crop[2]), "SpatialPolygons")
        ICES_clipped <- gIntersection(ices_areas, CP, byid=TRUE)  # clip
        
        
        # coastline cropped
        sh_coastlines        <- readShapePoly(file.path("~","Desktop","WBSsimu","DISPLACE_input_gis_BalticSea","GRAPH","land_around_Baltic_Sea_from_ICES_areas_shape.shp"))
        proj4string(sh_coastlines) <- CRS("+proj=longlat +datum=WGS84")
        library(raster)
        #CP <- as(extent(4, 25, 53, 58.5), "SpatialPolygons")
        proj4string(CP) <- CRS("+proj=longlat +datum=WGS84")
        library(rgeos)
        sh_coastlines_clipped <- gIntersection(sh_coastlines, CP, byid=TRUE)  # clip
        
        
        library(rgeos)
        out2  <- gIntersection(out, CP, byid=TRUE)  # clip by the CP...
        pid   <- sapply(slot(out2, "polygons"), function(x) slot(x, "ID")) # ...and coerce the SpatialPolygons back to a SpatialPolygonDataframe!!
        p.df  <- data.frame( ID=1:length(out2), row.names = pid)
        out2  <- SpatialPolygonsDataFrame(out2, p.df)
        rownames(my_data) <-  paste(my_data$c_square, "1")
        out2$data  <- my_data[pid, nametype]
        out2$color <- Satellite.Palette(length(the_breaks[-1])) [cut(out2$data, the_breaks)]
        out2$color[is.na(out2$color)] <- Satellite.Palette(1)[1]
        
        # PLOT
        #plot(sh_coastlines_clipped, xlim=xlims, ylim=ylims)
        plot(out2, col =  out2$color, border=NA,xlim=xlims, ylim=ylims,xaxs="i",yaxs="i")
        plot(sh_coastlines_clipped, col="gray80",xlim=xlims, ylim=ylims,add=T)
        
        
        plot(ICES_clipped, add=TRUE)
        #text(coordinates(ices_areas)[,1]-0.5, coordinates(ices_areas)[,2], paste(ices_areas$ICES_area), cex=2)
        
        library(maps)
        # bb <- as(extent(c(xlims,ylims)), "SpatialPolygons")
        # if (!is.null(gis_shape)) for (i in 1:length(gis_shape[[sce]])) {
        #   shp <- gis_shape[[sce]][[i]]
        #   proj4string(bb) <- proj4string(shp)
        #   shp <- gIntersection(shp, bb, byid=TRUE)
        #   #if(i==1) plot(shp, add=TRUE, col=grey(0.96), lwd=0.5, border=TRUE, asp=1.5)
        #   #if(i>1) plot(shp, add=TRUE, density=30, lwd=0.5, border=TRUE, asp=1.5)
        #   if(i==1) plot(shp, add=TRUE, lwd=2)
        #   if(i>1) plot(shp, add=TRUE)
        # }
        # 
        
        # plot(NG1,add=T,lwd=2, density=20); plot(NG1,add=T,lwd=3)
        # plot(NG2,add=T,lwd=2,density=20); plot(NG2,add=T,lwd=3)
         
        # plot(SG2,add=T, lwd=2, density=20); plot(SG2,add=T, lwd=3)
        # plot(SG3,add=T, lwd=2, density=20); plot(SG2,add=T, lwd=3)
        
        
        # plot(OSG1,add=T,lwd=2, density=20); plot(OSG1,add=T,lwd=3)
        # plot(OSG2,add=T,lwd=2, density=20); plot(OSG2,add=T,lwd=3)
        # plot(OSG3,add=T,lwd=2, density=20); plot(OSG3,add=T,lwd=3)
        # plot(OSG4,add=T,lwd=2, density=20); plot(OSG4,add=T,lwd=3)
        
         
        #plot(FG1,add=T,lwd=2, density=20); plot(FG1,add=T,lwd=3)
        
        
        # export a raster file
        # r           <- raster(xmn=xrange[1], xmx=xrange[2], ymn=yrange[1], ymx=yrange[2],  res=c(grid_degrees/2, grid_degrees/2), crs=CRS("+proj=longlat +datum=WGS84"))
        # some_coords <- SpatialPoints(cbind(lon=this$round_long.x, lat=this$round_lat.y))
        # rstr        <- rasterize(x=some_coords, y=r, field=this[,nametype], fun="sum") 
        # exportGTiff(
        #   a_raster= rstr, 
        #   namefile_gtiff= file.path(general$main.path, general$namefolderinput, paste0("map_averaged_",nametype,"_", plotid,"_", sce)),
        #   a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
        # )
        
        
        
        #box()
        mtext(side=3, namesce[count], cex=1.5, line=0.5)
        mtext(side=3, someletters[count], cex=2, line=1, adj=0)
        #axis(1, cex.axis=1.5)
        #axis(2, las=2, cex.axis=1.5)
        
        
        # bottomleft
        #x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
        #y = c(ylims[1]+0.5, ylims[1]+3, ylims[1]+3, ylims[1]+0.5)
        x = c(xlims[1]+0.1, xlims[1]+0.3, xlims[1]+0.3, xlims[1]+0.1)
        y = c(ylims[1]+0.5, ylims[1]+2.3, ylims[1]+2.3, ylims[1]+0.5)
        
        # topright
        # x = c(xlims_crop[2]-3.7, xlims_crop[2]-3.4, xlims_crop[2]-3.4, xlims_crop[2]-3.7)
        # y = c(ylims_crop[1]+1, ylims_crop[1]+1, ylims_crop[2]-1.8, ylims_crop[2]-1.8)
        
        
        
        #if(in_relative) a_title_leg <- substitute( expression(paste("% difference \n per cell")))  #Francois' input
        if(in_relative) a_title_leg <- "" #my own input to remove this legen
        if(!in_relative) a_title_leg <- "" #My own contribution
        #if(!in_relative) a_title_leg <- substitute( expression(paste(legend_text1, km^2, sep="")), list(legend_text1=legend_text1)) #Francois contribution
        ##if(!in_relative && func %in% c("rate", "no_density")) a_title_leg <- legend_text1  # overwrite
        
        the_breaks_leg <-NULL
        for(i in 1: length(the_breaks[-1])){ if(the_breaks[i]>1) {the_breaks_leg[i] <- round(the_breaks[i])} else{the_breaks_leg[i]<- the_breaks[i]}}
        legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette(length(the_breaks[-1])),
                          limits="", title=eval(a_title_leg),
                          legend= the_breaks_leg,
                          cex=1.3, col="black")
        
        
        
      } # end selected sce for plot
    } # end  Baseline
  } # end sce
  
  if(add_label_axis_1) mtext("Latitude", 1, line=2, cex=1.5, outer=TRUE)
  if(add_label_axis_2) mtext(side=2,"Longitude",line=2, cex=1.5, outer=TRUE)
  
  dev.off()
  
  
  table_obj <- cbind(table_obj, Total= apply(table_obj, 1, sum, na.rm=TRUE) ) # marginal value
  table_obj_relative_to_baseline <- cbind(round(sweep(table_obj, 2, table_obj[1,], FUN="/")*100, 1)- 100)
  table_obj_relative_to_baseline[1,] <- table_obj[1,]
  write.table(table_obj_relative_to_baseline,   file=namefile2, col.names=TRUE, row.names=TRUE, sep=";", quote=FALSE)
  print(namefile2)
  
  # useful to copy/paste into Excel!
  write.table(table_obj_relative_to_baseline, "clipboard", sep="\t", row.names=TRUE)   # export to excel
  
  # check in absolute numbers:
  # sum(table_obj_relative_to_baseline["svana_sub1mx20",]/100*table_obj_relative_to_baseline["svana_baseline",])
  
  
  
  return(table_obj_relative_to_baseline)
}



#### end function ####

##-------------------------------------------------------------------##
##-------------------CALLS-------------------------------------------##


#### General input ####

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
    
    
    general$namefolderoutput  <- c(  "scelgnbcouplingnoclosure",
                                     #"scelgnbcouplingnoclosurenoitq",
                                     "scelgnbcouplingwclosure",
                                     #"scelgnbcouplingwclosurenoitq",
                                     "scelgnbcouplingspwclosure",
                                     "scelgnbcouplingoldspwclosure",
                                     "scelgnbcouplingnurseclosure",
                                     "scelgnbcouplingfeedclosure"
    ) 
    
    
    general$namesimu           <- list("scelgnbcouplingnoclosure"  =   paste("simu", c(1:20), sep=''),
                                      #"scelgnbcouplingnoclosurenoitq"  =   paste("simu", c(1:5), sep=''),
                                      "scelgnbcouplingwclosure" =   paste("simu", c(1:20), sep=''),
                                      #"scelgnbcouplingwclosurenoitq"  =   paste("simu", c(1:5), sep=''),
                                      "scelgnbcouplingspwclosure"  =   paste("simu", c(1:20), sep=''),
                                      "scelgnbcouplingoldspwclosure" =   paste("simu", c(1:20), sep=''),
                                      "scelgnbcouplingnurseclosure"  =   paste("simu", c(1:20), sep=''),
                                      "scelgnbcouplingfeedclosure"  =   paste("simu", c(1:20), sep='')
                                        
    ) 
    
    
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
  
  
} # end FALSE

#### End general input ####



#### Read Shapefiles ####

sh_coastlines   <- readShapePoly(file.path("~","Desktop","WBSsimu",
                                           'DISPLACE_input_gis_BalticSea','MANAGEMENT','francois_EU.shp'), 
                                 proj4string=CRS("+proj=longlat +ellps=WGS84"))


# Spawner closure
# SG1 <- readShapePoly("~/Desktop/WBSsimu/Data/Hotspots/Oct_2022/Adults/Spawner_box1.shp",
#                      proj4string=CRS("+proj=longlat +ellps=WGS84"))

SG2 <- readShapePoly("~/Desktop/WBSsimu/Data/Hotspots/Oct_2022/Adults/Spawner_box2.shp",
                       proj4string=CRS("+proj=longlat +ellps=WGS84"))

SG3 <- readShapePoly("~/Desktop/WBSsimu/Data/Hotspots/Oct_2022/Adults/Spawner_box3.shp",
                     proj4string=CRS("+proj=longlat +ellps=WGS84"))


# Old spawner closure
OSG1 <- readShapePoly("~/Desktop/WBSsimu/Data/Hotspots/Oct_2022/Adults_A5+/A5+_JAN-DEC_box1.shp",
                     proj4string=CRS("+proj=longlat +ellps=WGS84"))

OSG2 <- readShapePoly("~/Desktop/WBSsimu/Data/Hotspots/Oct_2022/Adults_A5+/A5+_JAN-DEC_box2.shp",
                      proj4string=CRS("+proj=longlat +ellps=WGS84"))

OSG3 <- readShapePoly("~/Desktop/WBSsimu/Data/Hotspots/Oct_2022/Adults_A5+/A5+_JAN-DEC_box3.shp",
                      proj4string=CRS("+proj=longlat +ellps=WGS84"))

OSG4 <- readShapePoly("~/Desktop/WBSsimu/Data/Hotspots/Oct_2022/Adults_A5+/A5+_JAN-DEC_box4.shp",
                      proj4string=CRS("+proj=longlat +ellps=WGS84"))

# OSG5 <- readShapePoly("~/Desktop/WBSsimu/Data/Hotspots/Oct_2022/Adults_A5+/A5+_JAN-DEC_box5.shp",
#                       proj4string=CRS("+proj=longlat +ellps=WGS84"))




# Nursery closures
NG1 <- readShapePoly("~/Desktop/Review_FR/R2/Results/Hotspot/Juveniles/Recruits_box1.shp",
                     proj4string=CRS("+proj=longlat +ellps=WGS84"))


NG2 <- readShapePoly("~/Desktop/Review_FR/R2/Results/Hotspot/Juveniles/Recruits_box2.shp",
                     proj4string=CRS("+proj=longlat +ellps=WGS84"))



# Feeding closure (only overlapping one)
FG1 <- readShapePoly("~/Desktop/Review_FR/R2/Results/Hotspot/Feeding/Adult_Recruit_feeding_intersection.shp",
                     proj4string=CRS("+proj=longlat +ellps=WGS84"))




#### End Shapefiles ####


## Load the data
source(file.path("~","Desktop","WBSsimu",
  "DISPLACE_input_gis_BalticSea","DISPLACE_R_outputs_ForBalticSea","loadAggLoglikeFiles.R"))
loadLoglikeFiles(general=general, use_port_info=TRUE)









##options(error=recover) #to debug function,if necessary
#Note: ALWAYS start with the baseline scenario



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1) Now go for the plots: Check effectiveness of alternative closures
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
the_baseline <- "scelgnbcouplingnoclosure"
a_pop <- 2 #WBS cod population
#a_pop <- 11 #Herring cod population
selected_scenarios <- dput(names(general$namesimu)) #Alternative closures

ncol <- 2
nrow <- 1


## Map cumulative catches for cod 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mapNodeAverageLayerFiles(general, 
                          in_relative=F,  
                          grid_degrees=0.1, 
                          a_type="cumulcatches_per_pop", 
                          a_pop=paste0("_pop",a_pop),
                          a_type2="", 
                          field_pos=4, 
                          the_baseline= the_baseline,
                          selected_scenarios_for_plot = selected_scenarios[c(1,2)],
                          selected_scenarios_for_table = selected_scenarios[c(1,2)],
                          namesce=c("Baseline",
                                    "Seasonal Spawning Closure"
                                    #"Spawning Area Closure"
                                    #"Old Spawner Area Closure"
                                    #"Nursery Area Closure"
                                    #"Feeding Area Closure"
                          ),
                          #selected_areas_for_table=c("IVa", "22",    "23",    "24",    "25",    "26",  "27",   "28-1",   "28-2", "30", "31", "32"),
                          selected_areas_for_table=c("IIIa", "22", "23", "24"),
                          #the_breaks_baseline=   c(1, round(exp(seq(1.2, 10, by=1.2))), 10000),
                          #the_breaks= c(-100,  -55,   -7,  -1,    0,    1,   7,   55,  100,  1000), #c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                          the_breaks_baseline =   c(0, 50, 100, 200, 500, 1000, 3000, 5000, 10000, 15000),
                          #the_breaks= c(-100,  -80,   -50,  -10,    0,    10,   50,   80,  100, 200), #c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                          the_breaks= c(-100,  -80,   -60,  -40,  -20,   0,   20,   40,   60,  80, 100, 200), #c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                          
                          # gis_shape= list(scelgnbcouplingnoclosure = list(sh_coastlines),
                          #                 scelgnbcouplingwclosure = list(sh_coastlines),
                          #                 scelgnbcouplingspwclosure = list(SG2, SG3),
                          #                 scelgnbcouplingnurseclosure = list(NG1, NG2),
                          #                 scelgnbcouplingoldspwclosure = list(OSG2, OSG3, OSG4, OSG5),
                          #                 scelgnbcouplingfeedclosure = list(FG1)
                          #   ),
                          
                          #a_width= 7000, a_height =7200, xlims = c(7, 25), ylims=c(54,66),
                          #legend_text1="Total Catches kg per ")
                          #a_width= 6132*ncol, a_height =3759*nrow,  xlims_crop=c(4, 25), ylims_crop=c (53, 58.5),  xlims=c(7.5, 23), ylims=c (55.5, 56.5),
                          a_width= 5000*ncol, a_height =3600*nrow,  xlims_crop=c(8.5, 15.5), ylims_crop=c(53.7, 56.5),  xlims=c(8.5, 15.5), ylims=c(53.7, 56.5),
                          legend_text1="", 
                          someletters=c(""), 
                          add_label_axis_1=FALSE, add_label_axis_2=FALSE)





## Map cumulative fishing effort 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mapNodeAverageLayerFiles(general,  
                          in_relative=F,
                          grid_degrees=0.1, 
                          a_type="cumftime",
                          a_type2="",  
                          field_pos=4, 
                          the_baseline= the_baseline,
                          selected_scenarios_for_plot = selected_scenarios[c(1,2)],
                          selected_scenarios_for_table = selected_scenarios[c(1,2)],
                          namesce=c("Baseline",
                                    "Seasonal Spawning Closure"
                                    #"Spawning Area Closure"
                                    #"Old Spawner Area Closure"
                                    #"Nursery Area Closure"
                                    #"Feeding Area Closure"
                          ),
                          #selected_areas_for_table=c("IVa", "22",    "23",    "24",    "25",    "26",  "27",   "28-1",   "28-2", "30", "31", "32"),
                          selected_areas_for_table=c("IIIa", "22", "23", "24"),
                          #the_breaks_baseline=   c(1, round(exp(seq(1.2, 10, by=1.2))), 10000),
                          #the_breaks= c(-100,  -55,   -7,  -1,    0,    1,   7,   55,  100,  1000), #c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                          the_breaks_baseline =   c(0, 20, 50, 100, 200, 500, 1000),
                          the_breaks= c(-100,  -80,   -50,  -10,    0,    10,   50,   80,  100, 200), #c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                          
                          
                          gis_shape=NULL,
                          #a_width= 7000, a_height =7200, xlims = c(7, 25), ylims=c(54,66),
                          #legend_text1="Total Catches kg per ")
                          #a_width= 6132*ncol, a_height =3759*nrow,  xlims_crop=c(4, 25), ylims_crop=c (53, 58.5),  xlims=c(7.5, 23), ylims=c (55.5, 56.5),
                          a_width= 5000*ncol, a_height =3600*nrow,  xlims_crop=c(8.5, 15.5), ylims_crop=c(53.7, 56.5),  xlims=c(8.5, 15.5), ylims=c(53.7, 56.5),
                          legend_text1="", 
                          someletters=c(""),
                          add_label_axis_1=FALSE,
                          add_label_axis_2=FALSE)



## Map cumulative catches of all species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mapNodeAverageLayerFiles(general,  
                         in_relative=F,
                         grid_degrees=0.1, 
                         a_type="cumcatches",
                         a_type2="",  
                         field_pos=4, 
                         the_baseline= the_baseline,
                         selected_scenarios_for_plot = selected_scenarios[c(1,2)],
                         selected_scenarios_for_table = selected_scenarios[c(1,2)],
                         namesce=c("Baseline",
                                   "Seasonal Spawning Closure"
                                   #"Spawning Area Closure"
                                   #"Old Spawner Area Closure"
                                   #"Nursery Area Closure"
                                   #"Feeding Area Closure"
                         ),
                         #selected_areas_for_table=c("IVa", "22",    "23",    "24",    "25",    "26",  "27",   "28-1",   "28-2", "30", "31", "32"),
                         selected_areas_for_table=c("IIIa", "22", "23", "24"),
                         #the_breaks_baseline=   c(1, round(exp(seq(1.2, 10, by=1.2))), 10000),
                         #the_breaks= c(-100,  -55,   -7,  -1,    0,    1,   7,   55,  100,  1000), #c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                         the_breaks_baseline =   c(0, 20, 50, 100, 200, 500, 1000),
                         #the_breaks_baseline =   c(0, 100, 200, 300, 400, 420, 450, 480, 500),
                         
                         the_breaks= c(-100,  -80,   -50,  -10,    0,    10,   50,   80,  100, 200), #c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                         
                         
                         gis_shape=NULL,
                         #a_width= 7000, a_height =7200, xlims = c(7, 25), ylims=c(54,66),
                         #legend_text1="Total Catches kg per ")
                         #a_width= 6132*ncol, a_height =3759*nrow,  xlims_crop=c(4, 25), ylims_crop=c (53, 58.5),  xlims=c(7.5, 23), ylims=c (55.5, 56.5),
                         a_width= 5000*ncol, a_height =3600*nrow,  xlims_crop=c(8.5, 15.5), ylims_crop=c(53.7, 56.5),  xlims=c(8.5, 15.5), ylims=c(53.7, 56.5),
                         legend_text1="", 
                         someletters=c(""),
                         add_label_axis_1=FALSE,
                         add_label_axis_2=FALSE)


