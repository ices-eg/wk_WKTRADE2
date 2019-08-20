 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Build a "Spatial dependencies index" raster file
 
 ## Developed for WKTRADE2, Aug 2019

 # Bastardie et al 2014
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 

 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 dataPath    <- file.path(repoPath, "WKTRADE2_Data")
 outPath     <- file.path(dataPath, "ShapeFiles", "WKTRADE2")

 dir.create(file.path(outPath))
 
 library(raster)
 library(rgdal)
 
 #!!!!!!!!!!!!!!!!!!!!!!!!!#
 UTMzone           <- 32
 load(file=file.path(dataPath, "fake_grid_cell_data.RData"))  # get xxx
 #=> required: individual vessel data and per grid cell, e.g. like the one from vmstools tacsatp
 #!!!!!!!!!!!!!!!!!!!!!!!!!#



 #tacsatp$totfield <- apply(tacsatp[,grep('EURO',colnames(tacsatp))], 1, sum, na.rm=TRUE) )
 #x <- tacsatp[tacsatp$SI_STATE==1, c("VE_REF","LE_GEAR","grID", "CELL_LONG", "CELL_LATI", "totfield")]
 #xx <- x[x$CELL_LATI>54.5 & x$CELL_LATI <55.5 & x$CELL_LONG>10 & x$CELL_LONG<16,]
 #plot(xx[,c("CELL_LONG", "CELL_LATI")])
 #dd <- sample(1:nrow(xx))[1:1000]
 #xxx <- xx[dd,]
 #table(table(xxx$VE_REF))
 #table(table(xxx$grID))
 #xxx$VE_REF <- factor(xxx$VE_REF)
 #save(xxx, file=file.path(dataPath, "fake_grid_cell_data.RData"))

            
            
 # index 1
 revenue_per_vessel                                  <- tapply(xxx$totfield, list(xxx$VE_REF), sum, na.rm=TRUE)
 names_vessels_per_cell                              <- tapply(xxx$VE_REF, list(xxx$grID), function(x) as.character(unique(x)))
 tot_revenue_of_the_vessels_visiting_this_cell       <- lapply(names_vessels_per_cell, function(x) sum(revenue_per_vessel[x], na.rm=TRUE))
 index1_importance_of_vessels                        <- unlist(tot_revenue_of_the_vessels_visiting_this_cell) / sum(revenue_per_vessel, na.rm=TRUE)  # index runs from 0 to 1
   
 # index 2
 revenue_per_vessel                                  <- tapply(xxx$totfield, list(xxx$VE_REF), sum, na.rm=TRUE)
 revenue_per_cell                                    <- tapply(xxx$totfield, list(xxx$grID), sum, na.rm=TRUE)
 index2_contribution_of_the_cell                     <- unlist(revenue_per_cell) / sum(revenue_per_vessel, na.rm=TRUE) 

 # index 3
 names_vessels_per_cell                              <- tapply(xxx$VE_REF, list(xxx$grID), function(x) as.character(unique(x)))
 tot_revenue_of_the_vessels_visiting_this_cell       <- lapply(names_vessels_per_cell, function(x) sum(revenue_per_vessel[x], na.rm=TRUE))
 index3_dependency_to_the_cell                       <- unlist(revenue_per_cell) /  unlist(tot_revenue_of_the_vessels_visiting_this_cell)  # index runs from 0 to 1

 # note that index2 = index1 * index3

 # proj in UTM
 library(rgdal)   
 spp <- SpatialPoints(cbind(as.numeric(as.character(xxx[,"CELL_LONG"])), as.numeric(as.character(xxx[,"CELL_LATI"]))),
                       proj4string=CRS("+proj=longlat +datum=WGS84"))
 spp <- spTransform(spp, CRS(paste("+proj=utm +zone=",UTMzone," +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))    
 xxx <- cbind.data.frame(xxx, spp@coords)
 
 
 # export a raster for each
 ext <- extent(as(spp, "Spatial"))

 r <- raster(resolution = 5000, ext = ext,
            crs = paste0("+proj=utm +zone=",UTMzone,
                                      " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )
 somecells  <- xxx[!duplicated(xxx$grID), c("grID","coords.x1","coords.x2")]
 rownames(somecells) <- somecells$grID                                     
 idx1       <- cbind.data.frame(index1_importance_of_vessels, somecells[names(index1_importance_of_vessels),])                                     
 rstr1      <- rasterize(idx1[,c("coords.x1","coords.x2")], r, field=idx1$index1_importance_of_vessels, fun = mean)
 idx2       <- cbind.data.frame(index2_contribution_of_the_cell, somecells[names(index2_contribution_of_the_cell),])                                     
 rstr2      <- rasterize(idx2[,c("coords.x1","coords.x2")], r, field=idx2$index2_contribution_of_the_cell, fun = mean)
 idx3       <- cbind.data.frame(index3_dependency_to_the_cell, somecells[names(index3_dependency_to_the_cell),])                                     
 rstr3      <- rasterize(idx3[,c("coords.x1","coords.x2")], r, field=idx3$index3_dependency_to_the_cell, fun = mean)

 # check raster
 par(mfrow=c(1,3))
 plot(rstr1)
 plot(rstr2)
 plot(rstr3)

 # export the raster in outPath
 writeRaster(rstr1, file = file.path(outPath, 
                                paste0("rstr_spatialdependencyindex1.tif")), format = "GTiff", overwrite = TRUE)
 writeRaster(rstr2, file = file.path(outPath, 
                                paste0("rstr_spatialdependencyindex2.tif")), format = "GTiff", overwrite = TRUE)
 writeRaster(rstr3, file = file.path(outPath, 
                                paste0("rstr_spatialdependencyindex3.tif")), format = "GTiff", overwrite = TRUE)
 
         
           
