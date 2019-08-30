##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Retrieve an expected profit on spatial zones based on various assumptions
  ## then get raster files
 
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
 shape_file_out    <- "HELCOM_intensity_Otter_2016_04"
 grid_degrees      <- 0.1
 #!!!!!!!!!!!!!!!!!!!!!!!!!#


 
 library(maptools)
 library(rgdal)
 library(raster)


 # read shape file
 shp  <- readOGR(file.path(dataPath,  "ShapeFiles", "WGSFD", paste0(shape_file_name,".shp") ))
 if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

 head(shp@data)


 ##TODO:
 
 # read the WGSFD layers for spatial value
 

 # build a spatial layer "operating cost for fishing" 
 # e.g., fishCost= raster dist2Coast * fuel price * fuel consumption for a typical vessel per fleet-segment
 
 
 # deduce a "expected profit" in grid cell per fleet-segment (ignoring home harbours....)
 # e.g., (sum over (CPUE_stk/sum(CPUE_stk)* typical vessel carrying capacity  * average fish prices)  - 2*FishCost
 # CPUE is the historical CPUE obtained e.g. from WGSFD layers Landings/Effort
 # but landings in WGSFD data call not given by stk?
 
 
 # deduce a probability field for effort at t+1 as expected profit normilized to 1 
 
 
 # FAKE EXPORT FOR NOW:
 
 # convert to raster
 r           <- raster(xmn=bbox(shp)[1,1], xmx=bbox(shp)[1,2], ymn=bbox(shp)[2,1], ymx=bbox(shp)[2,2], res=c(grid_degrees, grid_degrees),
                             crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(lon=shp@data$MidLon, lat=shp@data$MidLat))
 rstr        <- rasterize(x=some_coords, y=r, field=shp@data$FishingH, fun=sum) 
    
 crs(rstr) <- "+proj=longlat +datum=WGS84"                

 plot(rstr)

 
 # export the layer and use in AddAttributeToShpFromRasterExtractorFromShpOverlay.R
 writeRaster(rstr, file = file.path(outPath, 
                                paste0("04_rstr_expectedprofit.tif")), format = "GTiff", overwrite = TRUE)




