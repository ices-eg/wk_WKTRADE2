 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Build a "Probability field" for allocating feffort from a weighting average
 ## over some relevant factors
 
 ## Developed for WKTRADE2, Aug 2019
 ## Francois Bastardie

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 

 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 dataPath    <- file.path(repoPath, "WKTRADE2_Data", "ShapeFiles", "WKTRADE2")
 outPath     <- file.path(repoPath, "WKTRADE2_Outputs", "ShapeFiles")

 
 dir.create(file.path(outPath))

 library(maptools)
 library(rgdal)
 library(raster)


 #!!!!!!!!!!!!!!!!!!!!!!!!!#
 shape_file_name     <- "HELCOM_intensity_Otter_2016_10" 
 yfield              <- 'MidLat'
 xfield              <- 'MidLon'
 shape_file_name_out <- "HELCOM_intensity_Otter_2016_With_Proba.shp"
 vars                <- c('totvalue', 'effcast','dist2Coast','Depend1','profit','gva','OP2016','lpue','vpue','windmills','mining') # just an example here...
 inverted            <- c(0,           0,            1,          0,         0,     0,     0,      0,     0,      0,          0) #  code 0/1 (1 if e.g. dist2coast, 0 if e.g. totvalue)
 if(length(inverted)!=length(vars)) stop()
 #!!!!!!!!!!!!!!!!!!!!!!!!!#

  
 # read shape file
 shp  <- readOGR(file.path(dataPath, paste0(shape_file_name,".shp") ))
 if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

 head(shp@data)

 # remove if already existing fields
 shp@data <- shp@data[, !colnames(shp@data) %in% c("proba_cell", "feffort_tplus1")] 

 # replace NAs with 0s
 for (a_var in vars) shp@data[,a_var] <- replace(shp@data[,a_var], is.na(shp@data[,a_var]), 0)


 # compute a probability field to allocate the total effort spatially from a weigthed mean over 
 # identified as relevant driving variables among e.g. LPUE, dist2coast, OP/LPUE, GVA, spaceDepdency, etc.
 temp        <- shp@data
 for (i in 1:length(vars)) { if(inverted[i]) shp@data[,vars[i]] <- 1/shp@data[,vars[i]] }
  
 w_cell      <- as.numeric(as.character(shp@data$feffort))
 var_cell    <- sapply(shp@data[,vars], function(x) as.numeric(as.character(x)))
 if(length(vars)==1) shp@data    <- cbind.data.frame(shp@data, proba_cell  = (w_cell*var_cell)/sum(w_cell*var_cell, na.rm=TRUE) )
 #if(length(vars)>1)  shp@data    <- cbind.data.frame(shp@data, proba_cell  = (apply(w_cell*var_cell, 1, sum)/length(vars))/sum(apply(w_cell*var_cell, 1, sum)/length(vars), na.rm=TRUE) )
 # or, alternatively:
 if(length(vars)>1)  {
            # w_cell*var1_cell/sum(w_cell*var1_cell)  *  w_cell*var2_cell/sum(w_cell*var2_cell) etc.  assuming proba independence
            shp@data    <- cbind.data.frame(shp@data, proba_cell  = apply((w_cell*var_cell) / apply(w_cell*var_cell, 2, sum, na.rm=TRUE), 1, prod) )
            shp@data$proba_cell <-  shp@data$proba_cell /sum(shp@data$proba_cell, na.rm=TRUE) # then, normalize
            }

 
 ## TODO: alternatively some more integrated rules 
 ## could be used here to deduce the probability 
 ## of visiting cells along the relevant variables......
 
 # remember that the curent problem with WGFBIT is that the effort displacement effect (if any) is assuming no change in catch rates for areas receiving extra effort...
 # Hence in the weighting to deduce a probability for future effort allocation we might put more weight from the info coming from the dynamic modelling approach 
 # (here we named "EffortForecast") which is internalizing this issue by dynamically simulating
 # the change in catch rates along the long-term fish population dynamics and the redistribution of effort
  
 
 
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
 # in SelectPolygonsToRestrictFromAugmentedShp.R as the input variable
  # export augmented shp 
 writeOGR(shp, file.path(outPath, shape_file_name_out), "SHP", driver="ESRI Shapefile")
 writeOGR(shp, file.path(dataPath, shape_file_name_out), "SHP", driver="ESRI Shapefile") # caution: write also back to dataPath

 #pblm of abbreviating the field names
 # shp2  <- readOGR(file.path(outPath, shape_file_name_out))
 # head(shp2@data)






