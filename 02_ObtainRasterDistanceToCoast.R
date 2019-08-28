 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## Build a "distance to coast" raster file
 
 ## Developed for WKTRADE2, Aug 2019

 # inspired by https://dominicroye.github.io/en/2019/calculating-the-distance-to-the-sea-in-r/
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 

 mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
 repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
 dataPath    <- file.path(repoPath, "WKTRADE2_Data")
 outPath     <- file.path(dataPath, "ShapeFiles", "WKTRADE2")

 dir.create(file.path(outPath))


 

 # based on sf
 library(rnaturalearth)
 library(sf) # https://r-spatial.github.io/sf/articles/sf1.html
 library(raster)
 library(tidyverse)
 library(RColorBrewer)



 a_region <- "BalticSeaWide" ; UTMzone <- 32
 if(a_region=="BalticSeaWide")   some_countries <- c("Denmark", "Germany", "Sweden", "Poland", "Norway", "Estonia", "Lituania", "Latvia", "Finland")
 if(a_region=="NorthSeaWide") some_countries <- c("Denmark", "Germany", "United Kingdom", "Faeroe Islands", "Netherlands", "Norway", "France")
 
 world <- ne_countries(scale = 50) #world map with 50m resolution
 unique(world@data$name_long)
 plot(world) #sp class by default

 #import the limits of countries
 countries <- ne_countries(scale = 10, 
              country = some_countries,
               returnclass = "sp")
 st_countries <-  st_as_sf(countries)

 #info of our spatial vector object
 st_countries

 plot(st_countries, max.plot=1)

 # transform to UTM
 region <- st_transform(st_countries, paste("+proj=utm +zone=",UTMzone,
                                      " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep=''))

 # create the fishnet
 grid <- st_make_grid(region, cellsize = 5000, what = "centers")
 

 # filter out the grid
 surrounding_land_shp     <- readShapePoly(
                              file.path(dataPath, "ShapeFiles", "handmade", paste0("simple_",a_region,"_polygon.shp")),
                              proj4string= CRS("+proj=longlat +ellps=WGS84")
                                )
 surrounding_land_shp_utm <- spTransform(surrounding_land_shp, 
                                      CRS(paste0("+proj=utm +zone=",UTMzone,
                                      " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
                                      ) 
 surrounding_land_shp_utm <-  st_as_sf(surrounding_land_shp_utm)
 grid <- st_intersection(grid, surrounding_land_shp_utm)



 # our fishnet with the extension of countries
 plot(grid)

 # only extract the points outside the limits of countries
 st_erase <- function(x, y) st_difference(x, st_union(st_combine(y)))
 grid     <- st_erase(grid, region)   

 # our fishnet now
 plot(grid)



 # combine subobjects before distance computation
 region <- st_union(st_combine(region))


 # transform region from polygon shape to line
 region <- st_cast(region, "MULTILINESTRING")

 # calculation of the distance between the coast and our points
 dist <- st_distance(region, grid)

 # distance with unit in meters
 head(dist)

 # create a data.frame with the distance and the coordinates of the points
 df <- data.frame(dist = as.vector(dist)/1000,
                    st_coordinates(grid))

 # structure
 str(df)

 # colors 
 col_dist <- brewer.pal(11, "RdGy")


 ggplot(df, aes(X, Y, fill = dist))+ #variables
         geom_tile()+ #geometry
           scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
             labs(fill = "Distance (km)")+ #legend name
             theme_void()+ #map theme
              theme(legend.position = "bottom") #legend position
              
              
 # get the extension
 ext <- extent(as(grid, "Spatial"))

 # extent object
 ext

 # raster destination
 r <- raster(resolution = 5000, ext = ext,
            crs = paste0("+proj=utm +zone=",UTMzone,
                                      " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )

 # convert the points to a spatial object class sf
 dist_sf <- st_as_sf(df, coords = c("X", "Y")) %>%
                      st_set_crs(paste0("+proj=utm +zone=",UTMzone,
                                      " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") )

 # create the distance raster
 dist_raster <- rasterize(dist_sf, r, "dist", fun = mean)

 # raster
 plot(dist_raster)

 # export the raster in outPath
 writeRaster(dist_raster, file = file.path(outPath, 
                                paste0("02_raster_dist_to_coast_",a_region,".tif")), format = "GTiff", overwrite = TRUE)


 











