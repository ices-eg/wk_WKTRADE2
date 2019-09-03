##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

## Developed for WKTRADE2, Aug 2019
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 polPath     <- file.path(repoPath, "WKTRADE2_Data","ShapeFiles")
 dataPath1   <- file.path(repoPath, "WKTRADE2_Data") # path to current spatial effort allocation 
 dataPath2   <- file.path(repoPath, "WKTRADE2_Data","ShapeFiles","DISPLACE_outputs", "BalticSea") # path to future spatial effort allocation
 outPath     <- file.path(repoPath, "WKTRADE2_Data","ShapeFiles", "WKTRADE2")

 dir.create(file.path(outPath))

 # import fishing effort at t+1 predicted from DISPLACE or SMART
 # and find the reciprocal to identify what cells to restrict......

 # remember the use of the dynamic modelling approach (e.g. DISPLACE or SMART) is two-fold:
 # 1. Generating an average probability field from likely effort allocation after some time running the fish-fisheries system ahead  
 # 2. Alternatively, used as a platform for conducting an impact assessement on the suggested spatial restriction
 # only point 1 is demonstrated here.
 
 #!!!!!!!!!!!!!!!!!!!!!!!!!#
 shape_file_name        <- "HELCOM_intensity_Otter_2016" 
 yfield                 <- 'MidLat'
 xfield                 <- 'MidLon'
 grid_degrees           <- 0.1 
 raster_file_name       <- "map_averaged_cumftime_2_scerestrictionontrawling5eez" # ...one sce
 raster_file_name_out   <- "01_raster_effort_forecast" 
 #!!!!!!!!!!!!!!!!!!!!!!!!!#


 library(maptools)
 library(rgdal)
 library(raster)

 
 # Step A - CURRENT EFFORT
 #read shape file for the current fishing effort spatial allocation
 shape_file_name     <- file.path(dataPath1, "ShapeFiles", "WGSFD", paste0(shape_file_name, ".shp"))
 shp  <- readOGR(shape_file_name)
 if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

 # convert to raster
 r           <- raster(xmn=bbox(shp)[1,1], xmx=bbox(shp)[1,2], ymn=bbox(shp)[2,1], ymx=bbox(shp)[2,2], res=c(grid_degrees/2, grid_degrees/2),
                             crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(lon=shp@data$MidLon, lat=shp@data$MidLat))
 rstr        <- rasterize(x=some_coords, y=r, field=shp@data$feffort, fun=sum) 
    
 crs(rstr) <- "+proj=longlat +datum=WGS84"                
 a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
 rstr_current       <- projectRaster(rstr, crs=a_crs)  # e.g. European EEA projection
 
 plot(rstr_current)  
 
 
 # Step B - FUTURE EFFORT
 # DISPLACE_outputs giving per scenario the average cumulated fishing effort distribution after 5y runs
 # see the CRS in mapNodeAverageLayerFiles.r
 # but likely to be:  a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
       
 # read raster with data to be extracted
 rstr_future <- raster(file.path(dataPath2, paste0(raster_file_name, ".tif") ))
 rstr_future[1:2]
 rstr_future[1:3,1:3, drop=FALSE]

 plot(rstr_future)
 plot(rstr_future, breaks=c(-1,1,2,3,4,6,8,11,15,20,27,10000), col=terrain.colors(13))
 plot(rstr_future[rstr_future>1 & rstr_future<summary(rstr_future)["Max.",], drop=FALSE], breaks=c(-1,1,2,3,4,6,8,11,15,20,27,10000), col=terrain.colors(13))

 # get rid of irrelevant cells
 rstr_future <- rstr_future[rstr_future>1 & rstr_future<summary(rstr_future)["Max.",], drop=FALSE]

 
 # Step C -  Raster algebra
 # first harmonize raster resolutions
 res(rstr_current)
 extent(rstr_current)
 res(rstr_future)
 extent(rstr_future)
 rstr_current <-resample(rstr_current, rstr_future, method="bilinear")
 res(rstr_current)==res(rstr_future)
 
 
 # then normalize to similar values
 normalized_rstr_current <- rstr_current/cellStats(rstr_current, 'sum')
 normalized_rstr_future  <- rstr_future/cellStats(rstr_future, 'sum')
 par(mfrow=c(1,2))
 plot(normalized_rstr_current, breaks=c(0,0.001,0.002,0.004,0.008,0.016,1), col=terrain.colors(8))
 plot(normalized_rstr_future, breaks=c(0,0.001,0.002,0.004,0.008,0.016,1), col=terrain.colors(8))
 
 # clip the current with the future
 normalized_rstr_current <- crop(normalized_rstr_current, normalized_rstr_future)

 # finally, do the algebra
 areas_to_go_in_future       <- normalized_rstr_current * normalized_rstr_future 
 areas_to_go_in_future       <- areas_to_go_in_future[areas_to_go_in_future>0, drop=FALSE]
 
 # normalize to 1 and therefore get a allocation key for total effort if necessary
 areas_to_go_in_future       <- areas_to_go_in_future/cellStats(areas_to_go_in_future, 'sum') 
 
 
# export the raster in outPath
 writeRaster(areas_to_go_in_future, file = file.path(outPath, 
                                paste0(raster_file_name_out,".tif")), format = "GTiff", overwrite = TRUE)

 

