 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Build a "Probability field" for allocating feffort from a weighting average
 ## over some relevant factors
 
 ## Developed for WKTRADE2, Aug 2019

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 

 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 dataPath    <- file.path(repoPath, "WKTRADE2_Data")
 outPath     <- file.path(dataPath, "WKTRADE2_Outputs", "ShapeFiles")

 
 dir.create(file.path(outPath))

 library(maptools)
 library(rgdal)
 library(raster)


 #!!!!!!!!!!!!!!!!!!!!!!!!!#
 shape_file_name     <- "HELCOM_intensity_Otter_2016_02" 
 yfield              <- 'MidLat'
 xfield              <- 'MidLon'
 shape_file_name_out <- "HELCOM_intensity_Otter_2016_With_Proba.shp"
 vars                <- c("dist2Coast", "totvalue") # just an example here...
 inverted            <- c(1,0) #  code 0/1 (1 if e.g. dist2coast, 0 if e.g. totvalue)
 #!!!!!!!!!!!!!!!!!!!!!!!!!#

  
 # read shape file
 shp  <- readOGR(file.path(outPath, paste0(shape_file_name,".shp") ))
 if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

 head(shp@data)


 # do a raster computation (or better, compute directly on augmented shp@data...)
 # and vars among e.g. LPUE, dist2coast, OP/LPUE, GVA, spaceDepdency, etc.
 temp        <- shp@data
 if(length(vars)!=length(inverted)) stop()
 for (i in 1:length(vars)) { if(inverted[i]) shp@data[,vars[i]] <- 1/shp@data[,vars[i]] }
  
 w_cell      <- as.numeric(as.character(shp@data$feffort))
 var_cell    <- sapply(shp@data[,vars], function(x) as.numeric(as.character(x)))
 if(length(vars)==1) shp@data    <- cbind.data.frame(shp@data, proba_cell  = (w_cell*var_cell)/sum(w_cell*var_cell, na.rm=TRUE) )
 if(length(vars)>1)  shp@data    <- cbind.data.frame(shp@data, proba_cell  = (apply(w_cell*var_cell, 1, sum)/length(vars))/sum(apply(w_cell*var_cell, 1, sum)/length(vars), na.rm=TRUE) )
 # or, alternatively:
 if(length(vars)>1)  {
            # w_cell*var1_cell/sum(w_cell*var1_cell)  *  w_cell*var2_cell/sum(w_cell*var2_cell) etc.  assuming independence
            shp@data    <- cbind.data.frame(shp@data, proba_cell  = apply((w_cell*var_cell) / apply(w_cell*var_cell, 2, sum, na.rm=TRUE), 1, prod) )
            shp@data$proba_cell <-  shp@data$proba_cell /sum(shp@data$proba_cell, na.rm=TRUE) # then, normalize
            }

 # just for info:
 shp@data    <- cbind.data.frame(shp@data, feffort_tplus1  =  shp@data$proba_cell * sum(shp@data$feffort, na.rm=TRUE))

 # checks
 sum(shp@data$proba_cell, na.rm=TRUE) # should return 1
 sum(shp@data$feffort, na.rm=TRUE)
 sum(shp@data$feffort_tplus1, na.rm=TRUE)
 
 
 # restore when necessary
 shp@data[, vars] <- temp[, vars]

 # quick & dirty visual check for before/after
 par(mfrow=c(1,2))
 somecolors <- rainbow(7)
 # plot1
 somedata   <- cut(shp@data[,"feffort"],7)
 brks  <- gsub("\\(","",x=unlist( sapply(levels(somedata), function (x) strsplit(x, ","))))
 brks  <- gsub("]","",x=brks)
 brks  <- as.numeric(unique(brks[]))
 levels(somedata) <- somecolors
 plot(shp, col=as.character(somedata), border=FALSE)
 # plot2
 somedata   <- cut(shp@data[,"feffort_tplus1"],breaks=brks)
 levels(somedata) <- somecolors
 plot(shp, col=as.character(somedata), border=FALSE)
 


 # finally, export the proba field for future use 
 # in SelectPolygonsToRestrictFromStaticView.R as the input variable
  # export augmented shp 
 writeOGR(shp, file.path(outPath, shape_file_name_out), "SHP", driver="ESRI Shapefile")

 #pblm of abbreviating the field names
 # shp2  <- readOGR(file.path(outPath, shape_file_name_out))
 # head(shp2@data)






