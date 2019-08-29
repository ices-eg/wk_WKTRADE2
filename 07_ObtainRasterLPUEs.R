 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Use of JRC raster file
 
 ## Developed for WKTRADE2, Aug 2019

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 dataPath    <- file.path(repoPath, "WKTRADE2_Data")
 outPath     <- file.path(dataPath, "ShapeFiles", "WKTRADE2")

 dir.create(file.path(outPath))


 #!!!!!!!!!!!!!!!!!!!!!!!!!#
 shape_file_name   <- "HELCOM_intensity_Otter_2016" 
 yfield            <- 'MidLat'
 xfield            <- 'MidLon'
 grid_degrees      <- 0.05
 shape_file_out    <- "HELCOM_intensity_Otter_2016_04"
 #!!!!!!!!!!!!!!!!!!!!!!!!!#


 
 library(maptools)
 library(rgdal)
 library(raster)


 # read shape file
 shp  <- readOGR(file.path(dataPath,  "ShapeFiles", "WGSFD", paste0(shape_file_name,".shp") ))
 if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

 head(shp@data)

 # just compute and add lpue and vpue
 shp@data$lpue <- shp@data$totweight/shp@data$feffort 
 shp@data$vpue <- shp@data$totvalue/shp@data$feffort 
 
 
 # convert to raster
 r           <- raster(xmn=bbox(shp)[1,1], xmx=bbox(shp)[1,2], ymn=bbox(shp)[2,1], ymx=bbox(shp)[2,2], res=c(grid_degrees, grid_degrees),
                             crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(lon=shp@data$MidLon, lat=shp@data$MidLat))
 rstr        <- rasterize(x=some_coords, y=r, field=shp@data$lpue, fun=sum) 
    
 crs(rstr) <- "+proj=longlat +datum=WGS84"                

 plot(rstr)

 # export the raster in outPath
 writeRaster(rstr, file = file.path(outPath, 
                                paste0("07_rstr_lpue.tif")), format = "GTiff", overwrite = TRUE)

 

 # convert to raster
 r           <- raster(xmn=bbox(shp)[1,1], xmx=bbox(shp)[1,2], ymn=bbox(shp)[2,1], ymx=bbox(shp)[2,2], res=c(grid_degrees, grid_degrees),
                             crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(lon=shp@data$MidLon, lat=shp@data$MidLat))
 rstr        <- rasterize(x=some_coords, y=r, field=shp@data$vpue, fun=sum) 
    
 crs(rstr) <- "+proj=longlat +datum=WGS84"                

 plot(rstr)

 # export the raster in outPath
 writeRaster(rstr, file = file.path(outPath, 
                                paste0("07_rstr_vpue.tif")), format = "GTiff", overwrite = TRUE)
 