



mapNodeAverageLayerFiles <- function(dataPath, outPath, grid_degrees=0.1, in_relative=FALSE, a_type="cumcatches", a_type2="",
                            export_raster = TRUE, export_plot = TRUE, func="ratio", field_pos=4, a_pop="", the_baseline= "scebaseline",
                            selected_scenarios=sces,
                            namesce=sces,
                            the_breaks_baseline= c(0.5, 1, round(exp(seq(0.5, 14, by=1.2))), 1000000),
                            the_breaks=c(rev(-round(exp(seq(0, 7, by=1)))),  0, round(exp(seq(0, 7, by=1)))),
                            gis_shape=list(),
                            a_width= 3400, a_height =3500, xlims =  c(-1, 17), ylims = c(53,60), 
                            legend_text1="Total Catches kg per "
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
    text(min(pnts[, 1])-0.1, max(pnts[, 2])-0, labels = title, adj = c(0,
        -1), ...)
}



   library(maptools)



    if(a_type2!="") nametype <- paste0(paste0(a_type, a_pop),"over",a_type2) else nametype <- paste0(a_type, a_pop)
    namefile  <- file.path(outPath, paste0("map_averaged_",nametype,"_selected_in_relative", in_relative, ".tiff") )
 

    plotid <- 0
    tiff(filename=namefile,   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))

    if(length(selected_scenarios)==2) m <- rbind(c(1, 2))
    if(length(selected_scenarios)==3) m <- rbind(c(1, 1), c(1, 1),c(2, 3))
    if(length(selected_scenarios)==4) m <- rbind(c(1, 2), c(3,4))
    if(length(selected_scenarios)==5) m <- rbind(c(1, 1), c(1, 1),c(2, 3), c(4, 5))
    if(length(selected_scenarios)==6) m <- rbind(c(1, 2) ,c(3, 4), c(5, 6))
    if(length(selected_scenarios)==7) m <- rbind(c(1, 1), c(1, 1),c(2, 3), c(4, 5),  c(6, 7))
    if(length(selected_scenarios)==8) m <- rbind(c(1, 2) ,c(3, 4), c(5, 6), c(7, 8))
    if(length(selected_scenarios)==9) m <- rbind(c(1, 2,3) ,c(4, 5,6), c(7,8,9))
    layout(m)
    par(mar=c(2,2,3,1))
    par(oma=c(4,4,1,1))
    
   count <-0
   for(sce in selected_scenarios)
     {
     count <- count+1

     plotid <- plotid +1
 
    this <- read.table(file=file.path(dataPath, sce,
                              paste("average_",a_type,"_layer",a_pop,".txt", sep='')), header=FALSE, skip = 1)
    colnames(this)             <- c("node","lat",  "long")
    colnames(this) [field_pos] <- paste0(a_type, a_pop)
    nametype                   <- paste0(a_type, a_pop)

    # filter out close to 0 values
    this[,nametype]  <- replace(this[,nametype], this[,nametype]<1e-1, 0)
   
    if(a_type2!=""){
       this  <- replace(this, is.na(this), 0)
       this[,a_type]  <- replace(this[,a_type], is.infinite(this[,a_type]), 0)
       this2 <- read.table(file=file.path(dataPath, sce,
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
    
    
 
     this$round_long <- this$long
     this$round_lat  <- this$lat
     this$cell_area  <- (cos(this$round_lat *pi/180) * 111.325 )* (grid_degrees*3/0.05)/60  * (111*(grid_degrees*3/0.05)/60) # 0.05 degree is 3 minutes
     if(!func %in% c("rate", "no_density")) this[,nametype]  <- round(this[,nametype])  / this$cell_area
     this$cell_id    <-  paste(this$round_long, this$round_lat, sep="_")
     
     
     
     # baseline
     if(sce == the_baseline) {
       Satellite.Palette.baseline <-colorRampPalette(c("cyan","aquamarine","orange","red"))
   
       the_baseline_layer <- this
       a_func <- "sum"
       the_baseline_layer <- aggregate(the_baseline_layer[,nametype],
                                list(the_baseline_layer$round_long, the_baseline_layer$round_lat, the_baseline_layer$cell_id), a_func, na.rm=TRUE)
       colnames(the_baseline_layer) <- c("round_long", "round_lat", "cell_id", nametype)
       the_baseline_layer[,nametype] <- replace (the_baseline_layer[,nametype], 
                                                 the_baseline_layer[,nametype]>the_breaks_baseline[length(the_breaks_baseline)], 
                                                 the_breaks_baseline[length(the_breaks_baseline)])
      
       library(vmstools) # for c_square
       my_data <- this
       my_data$c_square <- vmstools::CSquare (lon=my_data[,'long'], lat=my_data[,'lat'], degrees=grid_degrees)

       # then, aggregate the data per c_square...
       a_func             <- "sum"
       my_data            <- aggregate(my_data[,nametype], list(my_data$c_square), a_func, na.rm=TRUE)
       colnames(my_data)  <- c("c_square", nametype)
       my_data            <- cbind.data.frame(my_data, CSquare2LonLat(my_data$c_square, grid_degrees)) # get the mid point coordinates

       colnames(my_data)  <- c("c_square", nametype, "mid_lat", "mid_lon")


       my_data[,nametype] <- replace (my_data[,nametype],
                                                 my_data[,nametype]>the_breaks_baseline[length(the_breaks_baseline)],
                                                 the_breaks_baseline[length(the_breaks_baseline)])

       my_data <- my_data[!is.na(my_data$mid_lat),] # remove failure if any
       
       
    
     if(export_raster && in_relative==FALSE){
       require(raster) 
       r           <- raster(xmn=xlims[1], xmx=xlims[2], ymn=ylims[1], ymx=ylims[2], res=c(grid_degrees, grid_degrees),
                             crs=CRS("+proj=longlat +datum=WGS84"))
       some_coords <- SpatialPoints(cbind(lon=my_data$mid_lon, lat=my_data$mid_lat))
       rstr        <- rasterize(x=some_coords, y=r, field=my_data[,nametype], fun=sum, na.rm=TRUE) 
    
       crs(rstr) <- "+proj=longlat +datum=WGS84"                
       a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
       rstr_proj       <- projectRaster(rstr, crs=a_crs)  # e.g. European EEA projection
       #rstr_proj[is.na(rstr_proj)] <- -999  # arbitrary code, to get rid of true 0s in GIS
       #reclassify if needed
       #rstr_proj[rstr_proj< the_breaks_baseline[1]] <- the_breaks_baseline[1]
       #for(int in 1: length(the_breaks_baseline[-1])) {
       #     rstr_proj[rstr_proj>the_breaks_baseline[int] & rstr_proj<the_breaks_baseline[int+1]]  <- the_breaks_baseline[int+1]
       #}
       namefile_gtiff= file.path(outPath, paste0("map_averaged_",nametype,"_", plotid,"_", sce))
       writeRaster(rstr_proj, namefile_gtiff, format = "GTiff", overwrite=TRUE)
     }
  
  
     if(export_plot){
       # ...and map
       Satellite.Palette.baseline <- colorRampPalette(c("cyan","aquamarine","orange","red"))
  

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
     out       <- spatialLookup[my_data$c_square,]
     out$data  <- my_data[,nametype]

     out$color <- Satellite.Palette.baseline(length(the_breaks_baseline[-1])) [cut(out$data, the_breaks_baseline)]

     # so, create rectangle from xlim and ylim to fix bad limits
     bb <- as(extent(c(xlims,ylims)), "SpatialPolygons")
     proj4string(bb) <- proj4string(out)

     library(rgeos)
     out2  <- gIntersection(out, bb, byid=TRUE)  # clip by the bb...
     pid   <- sapply(slot(out2, "polygons"), function(x) slot(x, "ID")) # ...and coerce the SpatialPolygons back to a SpatialPolygonDataframe!!
     p.df  <- data.frame( ID=1:length(out2), row.names = pid) 
     out2  <- SpatialPolygonsDataFrame(out2, p.df)
     rownames(my_data) <-  paste(my_data$c_square, "1")
     out2$data  <- my_data[pid, nametype]
     out2$color <- Satellite.Palette.baseline(length(the_breaks_baseline[-1])) [cut(out2$data, the_breaks_baseline)]
     out2$color[is.na(out2$color)] <- Satellite.Palette.baseline(1)[1]
     plot(out2, add=FALSE, col =  out2$color, border=NA, asp=1.5) #... finally!

     out_baseline <<- out2    
   

   
     library(maps)
     bb <- as(extent(c(xlims,ylims)), "SpatialPolygons")
     if (!is.null(gis_shape)) for (i in 1:length(gis_shape[["scebaseline"]])) 
     {
           shp <- gis_shape[["scebaseline"]][[i]]
           proj4string(bb) <- proj4string(shp)
           shp <- gIntersection(shp, bb, byid=TRUE)
           if(i==1) plot(shp, add=TRUE, col=grey(0.8), lwd=0.5, border=TRUE, asp=1.5)
           if(i>1) plot(shp, add=TRUE, density=30, lwd=0.5, border=TRUE, asp=1.5)
     }
     
      mtext(side=3, namesce[count], cex=1.2, line=0.5)
      axis(1, cex.axis=1.2)
      axis(2, las=2, cex.axis=1.2)


    
      # topleft
      x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
      #y = c(ylims[2]-7, ylims[2]-2, ylims[2]-2, ylims[2]-7)
      y = c(ylims[2]-6, ylims[2]-1, ylims[2]-1, ylims[2]-6)

      the_breaks_leg <-NULL
      a_title <- substitute( expression(paste(legend_text1, km^2)), list(legend_text1=legend_text1))
      if(func %in% c("rate", "no_density")) a_title <- legend_text1  # overwrite
      for(i in 1: length(the_breaks_baseline[-1])){ if(the_breaks_baseline[i]>1) {the_breaks_leg[i] <- round(the_breaks_baseline[i])} else{the_breaks_leg[i]<- the_breaks_baseline[i]}}
       legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette.baseline(length(the_breaks_baseline[-1])),
         limits="", title=eval(a_title),
         legend= the_breaks_leg,
         cex=1, col="black")
      } # endif export plot

    }   else{



      this <- aggregate(this[,nametype], list(this$round_long, this$round_lat, this$cell_id), sum, na.rm=TRUE)
      colnames(this) <- c("round_long", "round_lat", "cell_id", nametype)

      # Merge!
      this           <- merge(the_baseline_layer, this, by.x="cell_id", by.y="cell_id")
      # filter for close to 0 values
      this[,paste0(nametype,".x")] <- replace(this[,paste0(nametype,".x")], this[,paste0(nametype,".x")]<1e-1, 0)
      this[,paste0(nametype,".y")] <- replace(this[,paste0(nametype,".y")], this[,paste0(nametype,".y")]<1e-1, 0)
     
  

     a_func <- "sum"
     if(in_relative){
        # percent
        this[,nametype]  <- (100* as.numeric(as.character(this[,paste0(nametype,".y")])) / as.numeric(as.character(this[,paste0(nametype,".x")])) )  -100
        # CAUTION!!!!: correct for area with low absolute value to avoid visual effect
        this[,nametype] [ this[,paste0(nametype,".x")] <quantile(this[,paste0(nametype,".x")] [ this[,paste0(nametype,".x")] !=0], prob=0.05)]  <- 0
        Satellite.Palette <-colorRampPalette(c("cyan","aquamarine","white","yellow","red"))  
     } else{
        # absolute values for this sce
        this[,nametype]   <- this[,paste0(nametype,".y")]
        the_breaks        <- the_breaks_baseline
        Satellite.Palette <- Satellite.Palette.baseline
     }



     if(sce %in% selected_scenarios){
 
        my_data <- this
    
        library(vmstools) # for c_square
        my_data$c_square <- vmstools::CSquare (lon=my_data[,'round_long.y'], lat=my_data[,'round_lat.y'], degrees=grid_degrees)

        # then, aggregate the data per c_square...
        my_data            <- aggregate(my_data[,nametype], list(my_data$c_square), a_func, na.rm=TRUE)
        colnames(my_data)  <- c("c_square", nametype)
        my_data            <- cbind.data.frame(my_data, CSquare2LonLat(my_data$c_square, grid_degrees)) # get the mid point coordinates

        colnames(my_data)  <- c("c_square", nametype, "mid_lat", "mid_lon")

        my_data[,nametype] <- replace (my_data[,nametype],
                                                 my_data[,nametype]>the_breaks[length(the_breaks)],
                                                 the_breaks[length(the_breaks)])

        my_data            <- my_data[!is.na(my_data$mid_lat),] # remove failure if any
     
  
        # export a raster
       if(export_raster && in_relative==FALSE){
     
        require(raster) 
        r           <- raster(xmn=xlims[1], xmx=xlims[2], ymn=ylims[1], ymx=ylims[2], res=c(grid_degrees, grid_degrees),
                             crs=CRS("+proj=longlat +datum=WGS84"))
        some_coords <- SpatialPoints(cbind(lon=my_data$mid_lon, lat=my_data$mid_lat))
        rstr        <- rasterize(x=some_coords, y=r, field=my_data[,nametype], fun=sum, na.rm=TRUE) 
   
        crs(rstr) <- "+proj=longlat +datum=WGS84"                
        a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
        rstr_proj       <- projectRaster(rstr, crs=a_crs)  # e.g. European EEA projection
        #rstr_proj[is.na(rstr_proj)] <- -999  # arbitrary code, to get rid of true 0s in GIS
        # classify:
        #rstr_proj[rstr_proj< the_breaks[1]] <- the_breaks[1]
        #for(int in 1: length(the_breaks[-1])) {
        #    rstr_proj[rstr_proj>the_breaks[int] & rstr_proj<the_breaks[int+1]]  <- the_breaks[int+1]
        #}
        namefile_gtiff= file.path(outPath, paste0("map_averaged_",nametype,"_", plotid,"_", sce))
        writeRaster(rstr_proj, namefile_gtiff, format = "GTiff", overwrite=TRUE)
       }    
 
 
      # ...and map
      if(export_plot){
        spdata           <- unique(my_data[,c("c_square", "mid_lat", "mid_lon")])  # make sure to remove duplicates...
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

        # create rectangle from xlim and ylim to fix bad limits
        bb <- as(extent(c(xlims,ylims)), "SpatialPolygons")
        proj4string(bb) <- proj4string(out)
    
        library(rgeos)
        out2  <- gIntersection(out, bb, byid=TRUE)  # clip by the bb...
        pid   <- sapply(slot(out2, "polygons"), function(x) slot(x, "ID")) # ...and coerce the SpatialPolygons back to a SpatialPolygonDataframe!!
        p.df  <- data.frame( ID=1:length(out2), row.names = pid) 
        out2  <- SpatialPolygonsDataFrame(out2, p.df)
        rownames(my_data) <-  paste(my_data$c_square, "1")
        out2$data  <- my_data[pid, nametype]
        out2$color <- Satellite.Palette(length(the_breaks[-1])) [cut(out2$data, the_breaks)]
        out2$color[is.na(out2$color)] <- Satellite.Palette(1)[1]
        plot(out2, add=FALSE, col =  out2$color, border=NA, asp=1.5) #... finally!

        out_sce <<- out2    


       library(maps)
       bb <- as(extent(c(xlims,ylims)), "SpatialPolygons")
       if (!is.null(gis_shape)) for (i in 1:length(gis_shape[[sce]])) {
           shp <- gis_shape[[sce]][[i]]
           proj4string(bb) <- proj4string(shp)
           shp <- gIntersection(shp, bb, byid=TRUE)
           if(i==1) plot(shp, add=TRUE, col=grey(0.8), lwd=0.5, border=TRUE, asp=1.5)
           if(i>1) plot(shp, add=TRUE, density=30, lwd=0.5, border=TRUE, asp=1.5)
       }
  

       mtext(side=3, namesce[count], cex=1.2, line=0.5)
       axis(1, cex.axis=1.2)
       axis(2, las=2, cex.axis=1.2)


       # topleft
       x = c(xlims[1]+0.2, xlims[1]+0.4, xlims[1]+0.4, xlims[1]+0.2)
       #y = c(ylims[2]-7, ylims[2]-2, ylims[2]-2, ylims[2]-7)
       y = c(ylims[2]-6, ylims[2]-1, ylims[2]-1, ylims[2]-6)

       if(in_relative) a_title_leg <- substitute( expression(paste("% difference \n per cell")))
       if(!in_relative) a_title_leg <- substitute( expression(paste(legend_text1, km^2, sep="")), list(legend_text1=legend_text1))
       if(func %in% c("rate", "no_density")) a_title_leg <- legend_text1  # overwrite

       the_breaks_leg <-NULL
       for(i in 1: length(the_breaks[-1])){ if(the_breaks[i]>1) {the_breaks_leg[i] <- round(the_breaks[i])} else{the_breaks_leg[i]<- the_breaks[i]}}
       legend.gradient2 (cbind(x = x , y = y ), cols=Satellite.Palette(length(the_breaks[-1])),
       limits="", title=eval(a_title_leg),
       legend= the_breaks_leg,
       cex=1.0, col="black")
       } # endif export plot
   

     } # endif selected sce for plot
    } # endif  Baseline
 } # endfor sce

  mtext("Latitude", 1, line=2, cex=1.5, outer=TRUE)
  mtext(side=2,"Longitude", line=2, cex=1.5, outer=TRUE)

  dev.off()



 return()
}



##-------------------------------------------------------------------##
##-------------------CALLS-------------------------------------------##

 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 polPath     <- file.path(mainPath, "wk_WKTRADE2", "WKTRADE2_Data","ShapeFiles")
 dataPath    <- file.path(mainPath, "wk_WKTRADE2", "WKTRADE2_Data","ShapeFiles","DISPLACE_outputs", "BalticSea")
 outPath     <- file.path(mainPath, "wk_WKTRADE2", "WKTRADE2_Data","ShapeFiles","DISPLACE_outputs", "BalticSea")

 
 dir.create(file.path(outPath))

                                                                                     
 sces  <- c(
                                 "scebaseline", 
                                # "scerestrictionsonnets",
                                 "scerestrictionontrawling5eez",
                                 "scerestrictionontrawling10eez",
                                 "scerestrictionontrawling15eez",
                                 "scerestrictionontrawling20eez",
                                 "scerestrictionontrawling5hab",
                                 "scerestrictionontrawling10hab",
                                 "scerestrictionontrawling15hab",
                                 "scerestrictionontrawling20hab"
                                # "scerestrictionsonnetsandtrawl20hab",
                                # "scerestrictionsonnetsandtrawl20eez"                                
         )
   library(maptools)
   sh_coastlines               <- readShapePoly(file.path(polPath,"handmade","EUcoastlines"), 
                                             proj4string=CRS("+proj=longlat +ellps=WGS84"))

   
   the_baseline <- "scebaseline"
   library(raster)
   mapNodeAverageLayerFiles (dataPath, outPath, in_relative=FALSE, grid_degrees=0.1, a_type="cumftime", a_type2="", 
                             export_raster=TRUE, export_plot=TRUE, field_pos=4, the_baseline= the_baseline,
                            selected_scenarios= sces,
                            namesce=sces,
                            the_breaks_baseline=  c(0, round(exp(seq(-1.5, 3.5, by=0.3)),1), 10000), 
                            the_breaks= c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                            gis_shape=list(
                                 scebaseline = list(sh_coastlines), 
                                 scerestrictionontrawling5eez= list(sh_coastlines),
                                 scerestrictionontrawling10eez= list(sh_coastlines),
                                 scerestrictionontrawling15eez= list(sh_coastlines),
                                 scerestrictionontrawling20eez= list(sh_coastlines),
                                 scerestrictionontrawling5hab= list(sh_coastlines),
                                 scerestrictionontrawling10hab= list(sh_coastlines),
                                 scerestrictionontrawling15hab= list(sh_coastlines),
                                 scerestrictionontrawling20hab= list(sh_coastlines)
                                            ),
                            a_width= 7000, a_height =7200, xlims = c(7, 25), ylims=c(54,66), 
                            legend_text1="Fish. hours per "
                         )


  # in relative
   mapNodeAverageLayerFiles (dataPath, outPath, in_relative=TRUE, grid_degrees=0.1, a_type="cumftime", a_type2="", 
                             field_pos=4, the_baseline= the_baseline,
                            export_raster=TRUE, export_plot=TRUE, 
                            selected_scenarios= sces,
                            namesce=sces,
                            the_breaks_baseline=  c(0, round(exp(seq(-1.5, 3.5, by=0.3)),1), 10000), 
                            the_breaks= c(rev(c(-round(exp(seq(0, 4, by=2))), -100)),  0, round(exp(seq(0, 7, by=2)))),
                            gis_shape=list(
                                 scebaseline = list(sh_coastlines), 
                                 scerestrictionontrawling5eez= list(sh_coastlines),
                                 scerestrictionontrawling10eez= list(sh_coastlines),
                                 scerestrictionontrawling15eez= list(sh_coastlines),
                                 scerestrictionontrawling20eez= list(sh_coastlines),
                                 scerestrictionontrawling5hab= list(sh_coastlines),
                                 scerestrictionontrawling10hab= list(sh_coastlines),
                                 scerestrictionontrawling15hab= list(sh_coastlines),
                                 scerestrictionontrawling20hab= list(sh_coastlines)
                                            ),
                            a_width= 7000, a_height =7200, xlims = c(7, 25), ylims=c(54,66), 
                            legend_text1="Fish. hours per "
                         )



 