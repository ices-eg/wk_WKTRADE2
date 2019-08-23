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


 
 library(maptools)
 library(rgdal)
 library(raster)

 # JRC data available for 2009-2016
 raster_file_name <- "OPFish_2016_europe"


 # read raster with data to be extracted
 rstr <- raster(file.path(dataPath, "ShapeFiles", "JRC", "OPFish_LPUE_LA_EF_Ratio_2009-2016_NEAtlantic_0417deg_GeoTif", 
                "OUTPUT_GIS", paste0(raster_file_name, ".tif") ))
 rstr[1:2]
 rstr[1:3,1:3, drop=FALSE]

 plot(rstr)

 #see OPFish_Workshop_05-2019_v4_final.pdf for description 
 