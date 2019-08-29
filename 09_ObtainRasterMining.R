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
 rstr_name_out     <- "08_rstr_windmills"
 #!!!!!!!!!!!!!!!!!!!!!!!!!#


 
 library(maptools)
 library(rgdal)
 library(raster)



 ##TODO
 # find a data source for sediment or gravel extraction e.g. from WKBEDPRESS
 


 # export the raster in outPath
 writeRaster(rstr, file = file.path(outPath, 
                                paste0("09_rstr_mining.tif")), format = "GTiff", overwrite = TRUE)
  