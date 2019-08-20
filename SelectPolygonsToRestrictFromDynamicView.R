##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

## Developed for WKTRADE2, Aug 2019
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 mainPath    <- file.path("D:","FBA","ADVICES") 
 dataPath    <- file.path(mainPath, "ICES_WKTRADE2","WKTRADE2_Data","ShapeFiles","WKTRADE2")
 outPath     <- file.path(dataPath, "ICES_WKTRADE2","WKTRADE2_Outputs")

 dir.create(file.path(outPath))
 dir.create(file.path(outPath, "ShapeFiles"))


# import fishing effort at t+1 predicted from DISPLACE or SMART
# and find the reciprocal to identify what cells to restrict......
