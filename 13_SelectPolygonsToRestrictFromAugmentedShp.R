##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## Select areas to restrict to fishing based on various criteria and % thresholds
## e.g. thresholds on historical catch volume, effort or lpues

## Developed for WKTRADE2, Aug 2019
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 dataPath    <- file.path(repoPath, "WKTRADE2_Data","ShapeFiles","WKTRADE2")
 outPath     <- file.path(dataPath, "WKTRADE2_Outputs")

 dir.create(file.path(outPath))



 #!!!!!!!!!!!!!!!!!!!!!!!!!#
 shape_file_name   <- "HELCOM_intensity_Otter_2016_02" 
 yfield            <- 'MidLat'
 xfield            <- 'MidLon'
 threshold         <- 20
 structuring_var   <- "dist2Coast" # or "feffort", or "VPUE", or the composite "probaField", etc.
 structuring_var2  <- "hab_code" # or "" or "EEZ", etc.
 cut_away_the      <- "largest" # e.g. lowest if feffort, largest if dist2Coast etc.
 cut_away_on       <- "nb_of_cells"  # or "values"
 shape_file_out    <- paste0(shape_file_name,"_",threshold,"_",structuring_var) 
 #!!!!!!!!!!!!!!!!!!!!!!!!!#


 library(maptools)
 library(rgdal)
 library(raster)


 # read shape file
 shp  <- readOGR(file.path(dataPath, paste0(shape_file_name,".shp") ))
 if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

 head(shp@data)
 
 
 # naming
 shp@data <- cbind.data.frame(shp@data, structuring_var=shp@data[,structuring_var])  
 if(structuring_var2!="") shp@data <- cbind.data.frame(shp@data, structuring_var2=shp@data[,structuring_var2])  

 # fill in NAs if any
 shp@data[is.na(shp@data$structuring_var), "structuring_var"] <- 0
 shp@data[is.na(shp@data$structuring_var2), "structuring_var2"] <- 0
  
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!CUT IN OVERALL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  
 # order cells by increasing (decreasing) order for the structuring variable 1 of interest
 library(doBy)
 if(cut_away_the=="lowest")  my_df <- orderBy(~ +structuring_var, data=shp@data)
 if(cut_away_the=="largest") my_df <- orderBy(~ -structuring_var, data=shp@data)
    
 my_df$cumsumvar           <- cumsum (my_df$structuring_var)
 my_df$cumsumvarpercent    <- cumsum (my_df$structuring_var) /sum (sum (my_df$structuring_var)) *100

     
 if(cut_away_on=="values"){
    idx  <- rownames( my_df[my_df$cumsumvarpercent< threshold,] )   # cutting away cells with lowest (largest) values representing xx% in total
    idx2 <- rownames( my_df[my_df$cumsumvarpercent>= threshold,] )   # xx% dual
 }

 if(cut_away_on=="nb_of_cells"){
    idx  <- rownames( my_df[1:(nrow(my_df)*threshold/100),] )   # cutting away xx% of cells with lowest (largest) values
    idx2 <- rownames( my_df[(nrow(my_df)*threshold/100):nrow(my_df),] )   # xx% dual
 }
 
 # export a plot on the fly
 nameobj <- paste0(shape_file_out, "_curve.tiff")
 tiff(file=file.path(outPath, nameobj), width = 380, height = 380, units = "px", pointsize = 12,
           compression = c("lzw"), type="cairo")
 par(mar=c(4,4,1,1))
 plot(my_df[nrow(my_df):1, structuring_var], type="l", lwd=3, xlab="Ordered grid cells (c-square)", ylab=structuring_var)
 abline(v=nrow(my_df)-length(idx))
 #text(length(idx2)+200, 200, label=paste0( threshold, "%"))
 dev.off()
 
 # a dirty visual check 
 #plot(shp)
 #plot(shp[idx,], add=TRUE, col=2)   # check
 #plot(shp[idx2,], add=TRUE, col=3)   # check
    
       
 # export GeoTiff
 spdf_closed_cells <- shp[idx,]
 spdf_opened_cells <- shp[idx2,]
    
 # dissolve...
 spdf_closed_cells_diss <- unionSpatialPolygons(spdf_closed_cells, ID=rep(1, length(spdf_closed_cells)))
 projection(spdf_closed_cells_diss) <- CRS("+proj=longlat +datum=WGS84")
 spdf_opened_cells_diss <- unionSpatialPolygons(spdf_opened_cells, ID=rep(1, length(spdf_opened_cells)))
 projection(spdf_opened_cells_diss) <- CRS("+proj=longlat +datum=WGS84")
 
 plot(shp)
 plot(spdf_closed_cells_diss, add=TRUE, col=2, border="red")   # check
 plot(spdf_opened_cells_diss, add=TRUE, col=3, border="green")   # check
 library(maps)
 map(add=TRUE, fill=TRUE, col=grey(0.5))
 box()
 axis(1) ; axis(2, las=2)
 
 # export a plot on the fly
 namefile <- paste0("cut_per_overall_", threshold, "_and_dual")
 savePlot(file=file.path(outPath, shape_file_out), type = "tiff")
    
 library(maptools)
 writePolyShape( SpatialPolygonsDataFrame(spdf_closed_cells_diss,  data.frame( ID=1, row.names = 1) ), 
                  file.path(outPath, paste0(shape_file_out,".shp")))
    
 writePolyShape( SpatialPolygonsDataFrame(spdf_opened_cells_diss,  data.frame( ID=1, row.names = 1) ),
                 file.path(outPath, paste0(shape_file_out,"_dual.shp")))
    
    
    
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!CUT ALONG A SECONDARY STRUCTURING VARIABLE!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 if(structuring_var2!=""){

    my_df <- shp@data
    
    # split and apply the ordering per landscape
    my_df_split <- split(my_df, f=my_df$structuring_var2)
 
    closed_cells <- list()
    opened_cells <- list()
    for (i in 1: length(my_df_split)){

       if(cut_away_the=="lowest")  my_df <- orderBy(~ +structuring_var, data=my_df_split[[i]])
       if(cut_away_the=="largest") my_df <- orderBy(~ -structuring_var, data=my_df_split[[i]])
    
       my_df$cumsumvar           <- cumsum (my_df$structuring_var)
       my_df$cumsumvarpercent    <- cumsum (my_df$structuring_var) /sum (sum (my_df$structuring_var)) *100

     
       if(cut_away_on=="values"){
          idx  <- rownames( my_df[my_df$cumsumvarpercent< threshold,] )   # cutting away cells with lowest (largest) values representing xx% in total
          idx2 <- rownames( my_df[my_df$cumsumvarpercent>= threshold,] )   # xx% dual
       }

       if(cut_away_on=="nb_of_cells"){
           idx  <- rownames( my_df[1:(nrow(my_df)*threshold/100),] )   # cutting away xx% of cells with lowest (largest) values
           idx2 <- rownames( my_df[(nrow(my_df)*threshold/100):nrow(my_df),] )   # xx% dual
       }
 
     closed_cells[[i]] <-  shp[idx,]
     opened_cells[[i]] <-  shp[idx2,]
    }


    # export GIS shape files
    spdf_closed_cells <- do.call("rbind", closed_cells) 
    spdf_opened_cells <- do.call("rbind", opened_cells) 
    plot(spdf_closed_cells, add=TRUE, col=4)   # check
    plot(spdf_opened_cells, add=TRUE, col=5)   # check

    # dissolve...
    spdf_closed_cells_diss <- unionSpatialPolygons(spdf_closed_cells, ID=rep(1, length(spdf_closed_cells)))
    projection(spdf_closed_cells_diss) <- CRS("+proj=longlat +datum=WGS84")
    spdf_opened_cells_diss <- unionSpatialPolygons(spdf_opened_cells, ID=rep(1, length(spdf_opened_cells)))
    projection(spdf_opened_cells_diss) <- CRS("+proj=longlat +datum=WGS84")
    plot(spdf_closed_cells_diss, add=TRUE, col=2, border="red")   # check
    plot(spdf_opened_cells_diss, add=TRUE, col=3, border="green")   # check
    map(add=TRUE, fill=TRUE, col=grey(0.5))
    box()
    axis(1) ; axis(2, las=2)


 # export a plot on the fly
 namefile <- paste0("cut_along_var2_", threshold, "_and_dual")
 savePlot(file=file.path(outPath, shape_file_out), type = "tiff")
    
 library(maptools)
 writePolyShape( SpatialPolygonsDataFrame(spdf_closed_cells_diss,  data.frame( ID=1, row.names = 1) ), 
                  file.path(outPath, paste0(shape_file_out,"_",structuring_var2,".shp")))
    
 writePolyShape( SpatialPolygonsDataFrame(spdf_opened_cells_diss,  data.frame( ID=1, row.names = 1) ),
                 file.path(outPath, paste0(shape_file_out,"_",structuring_var2,"_dual.shp")))
    
  
 } # end if structuring_var2


    
    