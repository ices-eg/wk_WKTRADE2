##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# Compare a simple rule for static effort displacement vs. 
# effort displacement predicted by the dynamic modelling approach,
# then run a WGFBIT assessment from both sources 

## Developed for WKTRADE2, Aug 2019
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") # just adapt that path to point to repo path...
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 polPath     <- file.path(repoPath, "WKTRADE2_Data","ShapeFiles")
 dataPath1   <- file.path(repoPath, "WKTRADE2_Data") # path to current spatial effort allocation 
 dataPath2   <- file.path(repoPath, "WKTRADE2_Data","ShapeFiles","DISPLACE_outputs", "BalticSea") # path to future spatial effort allocation
 outPath     <- file.path(repoPath, "WKTRADE2_Data","ShapeFiles", "WKTRADE2")

 dir.create(file.path(outPath))

 
  
 
 #!!!!!!!!!!!!!!!!!!!!!!!!!#
 shape_file_name            <- "HELCOM_intensity_Otter_2016" 
 yfield                     <- 'MidLat'
 xfield                     <- 'MidLon'
 grid_degrees               <- 0.1 
 shp_file_name_closed_pol   <- "feffort_cut_per_eez_25" # ...one sce
 raster_future_effort_alloc <- "map_averaged_cumftime_5_scerestrictionontrawling20eez" # ...one sce
 #raster_future_effort_alloc <- "map_averaged_cumsweptarea_5_scerestrictionontrawling20eez" # ...one sce
 raster_file_name_out       <- "diff_static_vs_dynamic" 
 #!!!!!!!!!!!!!!!!!!!!!!!!!#


 library(maptools)
 library(rgdal)
 library(raster)


 #------------------#
 #------------------#
 # POLYGON CLOSED
 #------------------#
 #------------------#
 # read shape file
 shp_closed  <- readOGR(file.path(dataPath2, paste0(shp_file_name_closed_pol, ".shp") ))
 if(is.na( projection(shp_closed))) projection(shp_closed) <- CRS("+proj=longlat +datum=WGS84")   # a guess!
 a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
 shp_closed <- spTransform(x = shp_closed, CRSobj = a_crs)

 # convert to raster
 #r           <- raster(xmn=bbox(shp_closed)[1,1], xmx=bbox(shp_closed)[1,2], ymn=bbox(shp_closed)[2,1], ymx=bbox(shp_closed)[2,2], res=c(grid_degrees/2, grid_degrees/2),
 #                            crs=CRS("+proj=longlat +datum=WGS84"))
 #vals <- rep(1e6, ncell(r)) # assign a fake high value
 #r <- setValues(r, vals) 
 #crs(r) <- "+proj=longlat +datum=WGS84"                
 #a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
 #rstr_closed       <- projectRaster(r, crs=a_crs)  # e.g. European EEA projection
 

 
 #------------------#
 #------------------#
 # CURRENT EFFORT
 #------------------#
 #------------------#
 #read shape file for the current fishing effort spatial allocation
 a_shape_file_name     <- file.path(dataPath1, "ShapeFiles", "WGSFD", paste0(shape_file_name, ".shp"))
 shp  <- readOGR(a_shape_file_name)
 if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

 # convert to raster
 a_var <- "feffort"
 #a_var <- "SAR"
 r           <- raster(xmn=bbox(shp)[1,1], xmx=bbox(shp)[1,2], ymn=bbox(shp)[2,1], ymx=bbox(shp)[2,2], res=c(grid_degrees/2, grid_degrees/2),
                             crs=CRS("+proj=longlat +datum=WGS84"))
 some_coords <- SpatialPoints(cbind(lon=shp@data$MidLon, lat=shp@data$MidLat))
 rstr        <- rasterize(x=some_coords, y=r, field=shp@data[,a_var], fun=sum) 
    
 crs(rstr) <- "+proj=longlat +datum=WGS84"                
 a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
 rstr_current       <- projectRaster(rstr, crs=a_crs)  # e.g. European EEA projection
 
  
 
 # DISPLACE EFFORT FROM A STATIC VIEW 
 rstr_closed       <- mask(x = rstr_current, mask = shp_closed)
 rstr_closed[is.na(rstr_closed)]   <- 0
 rstr_current[is.na(rstr_current)] <- 0
 sum_effort_init   <- cellStats(rstr_current, 'sum')
 sum_effort_cut    <- cellStats(rstr_closed, 'sum')
 rstr_opened       <- overlay(rstr_current, rstr_closed, fun=function(r1, r2){return(r1-r2)})
 sum_effort_opened <- cellStats(rstr_opened, 'sum')
 sum_effort_init -  sum_effort_cut == sum_effort_opened # check
 rstr_opened_static<- rstr_opened +  (sum_effort_cut /  ncell(rstr_opened)) # evenly dispatched
 rstr_opened_static<- rstr_opened +  (sum_effort_cut* rstr_opened / sum_effort_opened) # or dispatched proportional to feffort
 # TO CHECK:  not sure about which alternative WGFIT scenario-testing has used so far....


 # convert to shape file for use in WGFBIT and add c-sqaure info to make the link
 rstr <- projectRaster(rstr_opened_static, crs="+proj=longlat +datum=WGS84")  # e.g. European EEA projection
 shp_opened_static <- rasterToPolygons(rstr, dissolve=TRUE)
 head(shp_opened_static@data)
 shp_opened_static@data <- cbind.data.frame(shp_opened_static@data, 
                       vmstools::CSquare(coordinates(shp_opened_static)[,1], coordinates(shp_opened_static)[,2],
                        degrees=0.1)) 
 
 

 # quick check
 par(mfrow=c(1,2))
 plot(rstr_current)
 plot(rstr_opened_static)
 
 
 
 #------------------#
 #------------------#
 # FUTURE EFFORT FROM THE DYNAMIC MODELLING APPROACH
 #------------------#
 #------------------#

 # DISPLACE_outputs giving per scenario the average cumulated fishing effort distribution after 5y runs
 # see the CRS in mapNodeAverageLayerFiles.r
 # but likely to be:  a_crs="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
       
 # read raster with data to be extracted
 rstr_future <- raster(file.path(dataPath2, paste0(raster_future_effort_alloc, ".tif") ))   # should be feffort or SAR (caution: the WGFBIT assessment use SAR)
 rstr_future[1:2]
 rstr_future[1:3,1:3, drop=FALSE]

 plot(rstr_future)
 plot(rstr_future, breaks=c(-1,1,2,3,4,6,8,11,15,20,27,10000), col=terrain.colors(13))
 plot(rstr_future[rstr_future>1 & rstr_future<summary(rstr_future)["Max.",], drop=FALSE], breaks=c(-1,1,2,3,4,6,8,11,15,20,27,10000), col=terrain.colors(13))

 # get rid of irrelevant cells
 rstr_opened_dynamic <- rstr_future[rstr_future>1 & rstr_future<summary(rstr_future)["Max.",], drop=FALSE]

 # convert to shape file for use in WGFBIT
 shp_opened_dynamic <- rasterToPolygons(rstr_opened_dynamic, dissolve=TRUE)
 head(shp_opened_dynamic@data)
 
  # convert to shape file for use in WGFBIT and add c-sqaure info to make the link
 rstr <- projectRaster(rstr_opened_dynamic, crs="+proj=longlat +datum=WGS84")  # e.g. European EEA projection
 shp_opened_dynamic <- rasterToPolygons(rstr, dissolve=TRUE)
 head(shp_opened_dynamic@data)
 shp_opened_dynamic@data <- cbind.data.frame(shp_opened_dynamic@data, 
                       vmstools::CSquare(coordinates(shp_opened_dynamic)[,1], coordinates(shp_opened_dynamic)[,2],
                        degrees=0.1)) 
 
 
 
 
 #------------------#
 #------------------#
 #------------------#
 # DO THE DIFFERENCE
 #------------------#
 #------------------#
 
 # crop the larger dynamic rstr with the static rstr
 rstr_opened_dynamic <- crop(rstr_opened_dynamic, rstr_opened_static)

 # and harmonize raster resolutions
 res(rstr_opened_static)
 extent(rstr_opened_static)
 res(rstr_opened_dynamic)
 extent(rstr_opened_dynamic)
 rstr_opened_static <-resample(rstr_current, rstr_opened_dynamic, method="bilinear")
 res(rstr_opened_static)==res(rstr_opened_dynamic)


 
 # then normalize to similar values
 normalized_rstr_opened_static    <- rstr_opened_static/cellStats(rstr_opened_static, 'sum')
 normalized_rstr_opened_dynamic   <- rstr_opened_dynamic/cellStats(rstr_opened_dynamic, 'sum')
 par(mfrow=c(1,2))
 plot(normalized_rstr_opened_static, breaks=c(0,0.001,0.002,0.004,0.008,0.016,1), col=terrain.colors(8))
 plot(normalized_rstr_opened_dynamic, breaks=c(0,0.001,0.002,0.004,0.008,0.016,1), col=terrain.colors(8))
 
 # finally, do the algebra
 diff_static_minus_dynamic[is.na(diff_static_minus_dynamic)]   <- 0
 normalized_rstr_opened_dynamic[is.na(normalized_rstr_opened_dynamic)] <- 0

 diff_static_minus_dynamic <- log(rstr_opened_static/rstr_opened_dynamic)
 
 
 # a plot
 tiff(filename=file.path(outPath, paste0("diff_static_dynamic",".tif")),   width = 4000, height = 3000,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 par(mfrow=c(1,1))
 diverging_cols <- c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30')
 plot(diff_static_minus_dynamic, asp=1.6, col=diverging_cols)
 library(maptools)
 sh_coastlines               <- readShapePoly(file.path(polPath,"handmade","EUcoastlines"), 
                                             proj4string=CRS("+proj=longlat +ellps=WGS84"))
 sh_coastlines_proj <- spTransform(x = sh_coastlines, CRSobj = a_crs)
 plot(sh_coastlines_proj, add=TRUE, col=grey(0.8), lwd=0.5, border=TRUE, asp=1.5)
 dev.off()
 

 
# export the raster in outPath
 writeRaster(diff_static_minus_dynamic, file = file.path(outPath, 
                                paste0(raster_file_name_out,".tif")), format = "GTiff", overwrite = TRUE)

 
 
 
 #------------------#
 #------------------#
 # ...AND EVENTUALLY RUN THE WGBIT ASSESSEMENT:
 #------------------#
 #------------------#

 ## TODO
 # then use the rstr_opened_static or the rstr_opened_dynamic as input to (equilibrium) WGFBIT assessment, 
 # https://github.com/ices-eg/FBIT/blob/master/Utilities/Impact_continuous_longevity.R
 # https://github.com/ices-eg/FBIT/blob/master/Utilities/Processing_assessment.R
 # knowing that the required metric for the data should be Swept Area Ratio (SAR):
 # and knowing the depletion factor is currently given per gear type but not per habitat type (for now). 
 # Fishing event depletion proportions are derived by data from Hiddink et al. PNAS 2017 Table S4 
 # for TBB, OT, TD, Seine as 0.14*SAR, 0.06, 0.20 and 0.06 repectively
 
 
# e.g. adapt the following extracted from https://github.com/ices-eg/FBIT/blob/master/Utilities/Processing_assessment.R

################
# Figure A.12
################

  Region  <-  # TODO: TO BE RETRIEVED (ASK DANIEL)....
  year <- AssYear
  

  # impact is not calculated from the continuous longevity (takes a lot of time to do it for all the different scenarios)
  A12dat <- subset(Region@data,Region@data$Depth >= -200 & Region@data$Depth < 0)
  
  
  SAR_static     <- shp_opened_static   ## Prblm...we need SAR, not feffort
  SAR_dynamic    <- shp_opened_dynamic
  
  A12dat <- cbind(A12dat,SAR_static@data[match(A12dat$squares,SAR_static@data$c_square),c("SurfaceSAR")])
  colnames(A12dat)[ncol(A12dat)]<-"static_SSAR"
  A12dat <- cbind(A12dat,SAR_dynamic@data[match(A12dat$squares,SAR_dynamic@data$c_square),c("SurfaceSAR")])
  colnames(A12dat)[ncol(A12dat)]<-"dynamic_SSAR"
  
  state_ampl_static <- c()
  ccname_static <- c()
  state_ampl_dynamic <- c()
  ccname_dynamic <- c()
  amplifier <- c(4, 2, 1.25, 1, 0.75, 0.50, 0.25)
  
  # recovery following Hiddink et al. J Applied Ecol. 2018
  rzerone <- 5.31/1
  ronethree <- 5.31/2
  rthreeten <- 5.31/6.5
  rten <- 5.31/10
  Recov <- c(rzerone,ronethree,rthreeten,rten)
  
  for (j in 1: length(amplifier)){
    
    ### calculate fractions of biomass per longevity class 
    zerone<-A12dat$Lone                      #### fraction of biomass with longevity 0-1
    onethree<-A12dat$Lthree-zerone           #### fraction of biomass with longevity 1-3
    threeten<-A12dat$Lten-(onethree+zerone)  #### fraction of biomass with longevity 3-10
    tenmore<- 1-(threeten+onethree+zerone)   #### fraction of biomass with longevity > 10
    
    ### calculate depletion per gear group following Hiddink et al. and include the amplifier
    Depl_static   <- 0.06 * A12dat$static_SSAR * amplifier[j]     ### data from Hiddink et al. PNAS 2017 Table S4  - HERE FOR OTB
    Depl_dynamic   <- 0.06 * A12dat$dynamic_SSAR * amplifier[j]     ### data from Hiddink et al. PNAS 2017 Table S4   - HERE FOR OTB
    
    
    # STATIC - calculate state for the whole community
    frac_bio <- cbind(zerone,onethree,threeten,tenmore)

    dat <-as.data.frame(matrix(data=NA,nrow=nrow(A12dat)))
    for(i in 1:4){
      dat[,i]<-(frac_bio[,i]*(1-Depl_static/Recov[i]))
    }  
    dat[dat<0] <- 0
    state_static <- rowSums(dat)
    state_ampl_static <- cbind(state_ampl,state_static)
    ccname_static <- c(ccname_static,paste("state_static",amplifier[j],sep="_"))
  
    # DYNAMIC -calculate state for the whole community
    frac_bio <- cbind(zerone,onethree,threeten,tenmore)

    dat <-as.data.frame(matrix(data=NA,nrow=nrow(A12dat)))
    for(i in 1:4){
      dat[,i]<-(frac_bio[,i]*(1-Depl_dynamic/Recov[i]))
    }  
    dat[dat<0] <- 0
    state_dynamic <- rowSums(dat)
    state_ampl_dynamic <- cbind(state_ampl,state_dynamic)
    ccname_dynamic <- c(ccname_dynamic,paste("state_dynamic_",amplifier[j],sep="_"))
  
  
  }
  
  
  
  state_ampl_static[,1:7][is.na(state_ampl_static[,1:7]) & A12dat$Depth >= -200 & A12dat$Depth < 0] <- 1
  test <- apply(state_ampl_static,2,sort,decreasing=F)
  state_ampl_static<-as.data.frame(test)
  colnames(state_ampl_static) <- ccname_static 

  state_ampl_static$footprop <- 1-(1:nrow(state_ampl_static))/nrow(state_ampl_static)
  
  A12fig <- state_ampl_static
  
  save(A12fig, file="FigureA12.RData")




