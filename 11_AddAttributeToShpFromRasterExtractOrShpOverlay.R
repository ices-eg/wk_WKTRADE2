##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## Extract a raster value or do a shape overlay
## to add a raster or a shape file attribute to the data.frame of a shape.
## We then obtain an "augmented" shape.

## Developed for WKTRADE2, Aug 2019
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 dataPath    <- file.path(repoPath, "WKTRADE2_Data")
 outPath     <- file.path(dataPath, "ShapeFiles", "WKTRADE2")

 dir.create(file.path(outPath))


 library(maptools)
 library(rgdal)
 library(raster)

 
 
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 

 AddAttributeToShpFromRasterExtract <- function(shape_file_name     = file.path(dataPath, "ShapeFiles", "WGSFD", "HELCOM_intensity_Otter_2016.shp"), 
                                                yfield              = 'MidLat',
                                                xfield              = 'MidLon',
                                                raster_file_name    = file.path(dataPath, "ShapeFiles","WKTRADE2", "rstr_dist_to_coast_BalticSeaWide.tif"),
                                                namevar             = "dist2Coast",
                                                shape_file_name_out = file.path(outPath, "HELCOM_intensity_Otter_2016_01.shp"))
 ){
  
 # read shape file
 shp  <- readOGR(shape_file_name)
 if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

 head(shp@data)

 # read raster with data to be extracted
 rstr <- raster(file.path(raster_file_name))
 rstr[1:2]
 rstr[1:3,1:3, drop=FALSE]
  
 # clip to the area of interest
 #clip <- raster(file.path(dataPath, "ShapeFiles",  "simple_NorthSeaWide_polygon.tif"))
 #rstr <- crop(rstr, clip)

 # extract
 xy <- shp@data[, c(xfield,yfield) ] 
 library(rgdal)
 spp <- SpatialPoints(cbind(as.numeric(as.character(xy[,1])), as.numeric(as.character(xy[,2]))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
 xy <-  spTransform(spp, CRS(paste(attributes(rstr)$crs)))       
 assign(namevar, raster::extract(rstr, xy))
 

 # add the new field to the shp
 shp@data <- cbind.data.frame(shp@data, get(namevar))
 colnames(shp@data) [ncol(shp@data)] <- namevar

 # a quick check
 plot(shp, col=cut(shp@data[,namevar],7))

 # export augmented shp 
 writeOGR(shp, shape_file_name_out, "SHP", driver="ESRI Shapefile")
 
 #pblm of abbreviating the field names?
 #shp  <- readOGR(file.path(outPath, "HELCOM_intensity_Otter_2016_01.shp" ))
 # head(shp@data)
 
 return()
 }

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 
 
 AddAttributeToShpFromShpOverlay <- function(shape_file_name1     = file.path(dataPath, "ShapeFiles", "WGSFD", "HELCOM_intensity_Otter_2016.shp"), 
                                             yfield              = 'MidLat',
                                             xfield              = 'MidLon',
                                             shape_file_name2    = file.path(dataPath, "ShapeFiles","EMODNET", "EUNIS_codes_Combined_ICES_FAO9_clipped.shp"),
                                             namevar             = "hab_code",
                                             shape_file_name_out = "HELCOM_intensity_Otter_2016_01.shp"
 ){

 
 # read shape file 1
 shp1  <- readOGR(shape_file_name1)
 if(is.na( projection(shp1))) projection(shp1) <- CRS("+proj=longlat +datum=WGS84")   # a guess!
 head(shp1@data)

 # read shape file 2                                 
 shp2  <- readOGR(shape_file_name2)
 if(is.na( projection(shp2))) projection(shp2) <- CRS("+proj=longlat +datum=WGS84")   # a guess!
 head(shp2@data)

 xy <- shp1@data[, c(xfield,yfield) ] 
 library(sp)
 library(rgdal)
 spp <- SpatialPoints(cbind(as.numeric(as.character(xy[,1])), as.numeric(as.character(xy[,2]))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
 xy <-  spTransform(spp,  projection(shp2))       
 

 # extract
 df_query                      <- over(xy, shp2)
 shp1@data                     <- cbind.data.frame(shp1@data, as.character(df_query[,namevar]))
 colnames(shp1@data) [ncol(shp1@data)] <- namevar

 # a quick dirty check
 plot(shp1, col=as.numeric(as.character(shp1@data[,namevar])))

 # export augmented shp 
 writeOGR(shp1, shape_file_name_out, "SHP", driver="ESRI Shapefile")

  #pblm of abbreviating the field names?
 #shp  <- readOGR(file.path(outPath, "HELCOM_intensity_Otter_2016_03.shp" ))
 # head(shp@data)

 return()
 }

 
 
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!CALLS!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

 
 AddAttributeToShpFromRasterExtract (shape_file_name     = file.path(dataPath, "ShapeFiles", "WGSFD", "HELCOM_intensity_Otter_2016.shp"), 
                                     yfield              = 'MidLat',
                                     xfield              = 'MidLon',
                                     raster_file_name    = file.path(dataPath, "ShapeFiles","WKTRADE2", "01_raster_effort_forecast.tif"),
                                     namevar             = "effcast",
                                     shape_file_name_out = file.path(outPath, "HELCOM_intensity_Otter_2016_02.shp"))


 # tool chain on previous...
 AddAttributeToShpFromRasterExtract (shape_file_name     = file.path(dataPath, "ShapeFiles", "WGSFD", "HELCOM_intensity_Otter_2016_01.shp"), 
                                     yfield              = 'MidLat',
                                     xfield              = 'MidLon',
                                     raster_file_name    = file.path(dataPath, "ShapeFiles","WKTRADE2", "02_rstr_dist_to_coast_BalticSeaWide.tif"),
                                     namevar             = "dist2Coast",
                                     shape_file_name_out = file.path(outPath, "HELCOM_intensity_Otter_2016_02.shp"))
                                     
 
  # tool chain on previous...
 AddAttributeToShpFromRasterExtract (shape_file_name     = file.path(outPath, "HELCOM_intensity_Otter_2016_02.shp"), 
                                     yfield              = 'MidLat',
                                     xfield              = 'MidLon',
                                     raster_file_name    = file.path(dataPath, "ShapeFiles","WKTRADE2", "03_rstr_spatialdependencyindex1.tif"),
                                     namevar             = "Depend1",
                                     shape_file_name_out = file.path(outPath, "HELCOM_intensity_Otter_2016_03.shp"))
 

 # tool chain on previous...
 AddAttributeToShpFromRasterExtract (shape_file_name     = file.path(outPath, "HELCOM_intensity_Otter_2016_03.shp"), 
                                     yfield              = 'MidLat',
                                     xfield              = 'MidLon',
                                     raster_file_name    = file.path(dataPath, "ShapeFiles","WKTRADE2", "04_rstr_expectedprofit.tif"),
                                     namevar             = "profit",
                                     shape_file_name_out = file.path(outPath, "HELCOM_intensity_Otter_2016_04.shp"))
 
 
 # tool chain on previous...
 AddAttributeToShpFromRasterExtract (shape_file_name     = file.path(outPath, "HELCOM_intensity_Otter_2016_04.shp"), 
                                     yfield              = 'MidLat',
                                     xfield              = 'MidLon',
                                     raster_file_name    = file.path(dataPath, "ShapeFiles","WKTRADE2", "06_rstr_oceanproductivityfish.tif"),
                                     namevar             = "OP2016",
                                     shape_file_name_out = file.path(outPath, "HELCOM_intensity_Otter_2016_05.shp"))
 
 
 # tool chain on previous...
 AddAttributeToShpFromRasterExtract (shape_file_name     = file.path(outPath, "HELCOM_intensity_Otter_2016_05.shp"), 
                                     yfield              = 'MidLat',
                                     xfield              = 'MidLon',
                                     raster_file_name    = file.path(dataPath, "ShapeFiles","WKTRADE2", "06_rstr_oceanproductivityfish.tif"),
                                     namevar             = "OP2016",
                                     shape_file_name_out = file.path(outPath, "HELCOM_intensity_Otter_2016_06.shp"))
 
 # tool chain on previous...
 AddAttributeToShpFromShpOverlay (shape_file_name1     = file.path(outPath, "HELCOM_intensity_Otter_2016_06.shp"), 
                                             yfield              = 'MidLat',
                                             xfield              = 'MidLon',
                                             shape_file_name2    = file.path(dataPath, "ShapeFiles","EMODNET", "EUNIS_codes_Combined_ICES_FAO9_clipped.shp"),
                                             namevar             = "hab_code",
                                             shape_file_name_out = file.path(outPath, "HELCOM_intensity_Otter_2016_07.shp"))


