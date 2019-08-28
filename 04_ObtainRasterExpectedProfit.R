 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Retrieve GVA per fleet-segment from STECF AER data
 ## and assign to fleet-segment-specific VMS shape layers (from WGSFD or finer), 
 ## then get raster files
 
 ## Developed for WKTRADE2, Aug 2019

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 

 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 dataPath    <- file.path(repoPath, "WKTRADE2_Data")
 outPath     <- file.path(dataPath, "ShapeFiles", "WKTRADE2")

 dir.create(file.path(outPath))


 # read the WGSFD layers for spatial value
 

 # build a spatial layer "operating cost for fishing" 
 # e.g., fishCost= raster dist2Coast * fuel price * fuel consumption for a typical vessel per fleet-segment
 
 
 # deduce a "expected profit" in grid cell per fleet-segment (ignoring home harbours....)
 # e.g., (sum over (CPUE_stk/sum(CPUE_stk)* typical vessel carrying capacity  * average fish prices)  - 2*FishCost
 # CPUE is the historical CPUE obtained e.g. from WGSFD layers Landings/Effort
 # but landings in WGSFD data call not given by stk?
 
 
 # deduce a probability field for effort at t+1 as expected profit normilized to 1 
 
 
 # export the layer and use in AddAttributeToShpFromRasterExtractorFromShpOverlay.R


 # export the raster in outPath
 writeRaster(rstr, file = file.path(outPath, 
                                paste0("04_rstr_expectedprofit.tif")), format = "GTiff", overwrite = TRUE)




