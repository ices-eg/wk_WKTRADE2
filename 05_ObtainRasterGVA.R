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


 #!!!!!!!!!!!!!!!!!!!!!!!!!#
 shape_file_name   <- "HELCOM_intensity_Otter_2016" 
 yfield            <- 'MidLat'
 xfield            <- 'MidLon'
 shape_file_out    <- "HELCOM_intensity_Otter_2016_05"
 #!!!!!!!!!!!!!!!!!!!!!!!!!#


 
 library(maptools)
 library(rgdal)
 library(raster)


 # read shape file
 shp  <- readOGR(file.path(dataPath,  "ShapeFiles", "WGSFD", paste0(shape_file_name,".shp") ))
 if(is.na( projection(shp))) projection(shp) <- CRS("+proj=longlat +datum=WGS84")   # a guess!

 head(shp@data)


 ##TODO
 
 

 ## maybe start from helpful piece of code dealing with STECF AER data: 
library(readxl)
library(dplyr)
IRL <- read_excel(file.path(dataPath, "STECF", "2018-07_STECF 18-07 - EU Fleet Economic and Transversal data_fs level_final", "IRL.xlsx"))   # extracted from pivot_economic  IRL
IRL <- subset(IRL, select=c(variable_name, `2014`, `2015`, `2016`))
IRL %>% mutate(average = (`2014`+ `2015`+ `2016`)/3) -> IRL
IRL <- subset(IRL, select= -c(`2014`, `2015`, `2016`))
IRL <- as.data.frame(t(IRL))
names(IRL) <- lapply(IRL[1, ], as.character)
IRL <- IRL[-1,]
IRL <- cbind(IRL, Code="IRL")
IRL <- IRL[,c(35,1:34)]


#Select value of interest
IRL <- subset(IRL, select = c(Code, `Total number of vessels`, `Total employed`, `Other income`, `Wages and salaries of crew`, `Unpaid labour value`, `Other variable costs`, `Other non-variable costs`, `Tangible asset value (replacement)`, `Value of landings`, `Income from landings`, `Fishing days`, `Full-time equivalent (harmonised)`))
IRL$FleetSeg   <- IRL$Code
IRL$Nb_Vessels <- IRL$`Total number of vessels`
IRL$Nb_crew <- IRL$`Total employed`
IRL$Annual_other_income <- IRL$`Other income`
IRL$`Value of landings` <- as.numeric(as.character(IRL$`Value of landings`))
IRL$`Income from landings` <- as.numeric(as.character(IRL$`Income from landings`))
IRL  %>% mutate(`Landing_costs_percent` =(`Value of landings`- `Income from landings`)/`Value of landings`) -> IRL 
IRL$`Wages and salaries of crew` <- as.numeric(as.character(IRL$`Wages and salaries of crew`))
IRL$`Unpaid labour value` <- as.numeric(as.character(IRL$`Unpaid labour value`))
IRL  %>% mutate(`Crewshare_and_unpaid_labour_costs_percent` =(`Wages and salaries of crew`+`Unpaid labour value`)/`Income from landings`) -> IRL #crew and unpaid labour costs related to the income from landings
IRL$`Other variable costs` <- as.numeric(as.character(IRL$`Other variable costs`))
IRL$`Fishing days` <- as.numeric(as.character(IRL$`Fishing days`))
IRL  %>% mutate(`Other_variable_costs_per_unit_effort` =`Other variable costs`/((`Fishing days`)*24)) -> IRL #Better to use kW or Gt fishing days?
IRL <- cbind(IRL, Annual_insurance_costs_per_crew=0) #Source?
IRL <- cbind(IRL, Standard_labour_hour_opportunity_costs=34.09)#https://ec.europa.eu/eurostat/statistics-explained/index.php/Hourly_labour_costs
IRL$Standard_annual_full_time_employment_hours <- IRL$`Full-time equivalent (harmonised)`
IRL$Other_annual_fixed_costs <- IRL$`Other non-variable costs`
IRL$Vessel_value <- IRL$`Tangible asset value (replacement)`#??
IRL <- cbind(IRL, Annual_depreciation_rate=4)#Source?
IRL <- cbind(IRL, Opportunity_interest_rate=4)#Source?
IRL <- cbind(IRL, Annual_discount_rate=4)#Source?
IRL <- subset(IRL, select = -c(Code, `Total number of vessels`, `Total employed`, `Other income`, `Value of landings`, `Income from landings`, `Wages and salaries of crew`, `Unpaid labour value`, `Other variable costs`, `Fishing days`, `Other non-variable costs`, `Tangible asset value (replacement)`,`Full-time equivalent (harmonised)`))




Economics_Country <- rbind(IRL) #, DNK, EST, FIN, LAT, LIT, POL, SWE)

write.table(Economics_Country, file = file.path(general$main_path_gis, "FISHERIES", "STECF",
                                             "Economics_Country.csv"),sep=";",row.names=F,col.names=T, quote=FALSE)

####################################################################################################################



library(tidyr)
library(reshape2)
IRL_fleet <- read_excel(file.path(general$main_path_gis, 
                                  "FISHERIES", "STECF", "2018-07_STECF 18-07 - EU Fleet Economic and Transversal data_fs level_final",
                                  "IRL-Fleet.xlsx"))   # extracted from pivot_economic IRL, then MORE TABLE, then ticking fs_name, then complete flat table blanks with the variable names
IRL_fleet <- subset(IRL_fleet, select=c(variable_name, fs_name, `2014`, `2015`, `2016`))
IRL_fleet %>% mutate(average = (`2014`+ `2015`+ `2016`)/3) -> IRL_fleet
IRL_fleet <- subset(IRL_fleet, select= -c(`2014`, `2015`, `2016`))
IRL_fleet %>% mutate(country = (sapply(strsplit(IRL_fleet$fs_name, split=" "), function(x) x[1]))) -> IRL_fleet
IRL_fleet %>% mutate(area = (sapply(strsplit(IRL_fleet$fs_name, split=" "), function(x) x[2]))) -> IRL_fleet
IRL_fleet %>% mutate(gear_length = (sapply(strsplit(IRL_fleet$fs_name, split=" "), function(x) x[3]))) -> IRL_fleet
IRL_fleet %>% separate(gear_length, into = c("gear", "length"), sep = "(?<=[A-Za-z])(?=[0-9])") -> IRL_fleet
unique(IRL_fleet$gear)
#DFN = Drift and/or fixed netters
# DTS = Demersal trawlers and/or demersal seiners
# PG = Passive gears 
#TBB = Beam trawlers
# TM = Pelagic trawlers
unique(IRL_fleet$length)
IRL_fleet$length[IRL_fleet$length %in% "40XX"]<-"4000"
IRL_fleet$length[IRL_fleet$length %in%"1012"]<-NA
IRL_fleet$length[IRL_fleet$length %in%"0010"]<-NA
IRL_fleet <- na.omit(IRL_fleet)
IRL_fleet <- cbind(IRL_fleet, code=c(NA))
  IRL_fleet$country <- as.factor(IRL_fleet$country)
  IRL_fleet$gear <- as.factor(IRL_fleet$gear)
  IRL_fleet$length <- as.factor(IRL_fleet$length)
  IRL_fleet$code[IRL_fleet$code %in% NA]<- paste(IRL_fleet$country, IRL_fleet$gear, IRL_fleet$length, sep = "")
  unique(IRL_fleet$code)
IRL_fleet <- subset(IRL_fleet, select = c(code, variable_name, average))
IRL_fleet <- as.data.frame(dcast(IRL_fleet, code~variable_name, mean))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#Select value of interest
IRL_fleet <- subset(IRL_fleet, select = c(code, `Total number of vessels`, `Total employed`, `Other income`, `Wages and salaries of crew`, `Unpaid labour value`, `Other variable costs`, `Other non-variable costs`, `Tangible asset value (replacement)`, `Value of landings`, `Income from landings`, `Fishing days`, `Full-time equivalent (harmonised)`))
IRL_fleet$FleetSeg <- IRL_fleet$code
IRL_fleet$Nb_Vessels <- IRL_fleet$`Total number of vessels`
IRL_fleet$Nb_crew <- IRL_fleet$`Total employed`
IRL_fleet$Annual_other_income <- IRL_fleet$`Other income`
IRL_fleet$`Value of landings` <- as.numeric(as.character(IRL_fleet$`Value of landings`))
IRL_fleet$`Income from landings` <- as.numeric(as.character(IRL_fleet$`Income from landings`))
IRL_fleet  %>% mutate(`Landing_costs_percent` =(`Value of landings`- `Income from landings`)/`Value of landings`) -> IRL_fleet 
IRL_fleet$`Wages and salaries of crew` <- as.numeric(as.character(IRL_fleet$`Wages and salaries of crew`))
IRL_fleet$`Unpaid labour value` <- as.numeric(as.character(IRL_fleet$`Unpaid labour value`))
IRL_fleet  %>% mutate(`Crewshare_and_unpaid_labour_costs_percent` =(`Wages and salaries of crew`+`Unpaid labour value`)/`Income from landings`) -> IRL_fleet #crew and unpaid labour costs related to the income from landings
IRL_fleet$`Other variable costs` <- as.numeric(as.character(IRL_fleet$`Other variable costs`))
IRL_fleet$`Fishing days` <- as.numeric(as.character(IRL_fleet$`Fishing days`))
IRL_fleet  %>%  mutate(`Other_variable_costs_per_unit_effort` =`Other variable costs`/((`Fishing days`)*24)) -> IRL_fleet #Better to use kW or Gt fishing days?
IRL_fleet <- cbind(IRL_fleet, Annual_insurance_costs_per_crew=0) # guess
IRL_fleet <- cbind(IRL_fleet, Standard_labour_hour_opportunity_costs=32.01)#https://ec.europa.eu/eurostat/statistics-explained/index.php/Hourly_labour_costs
IRL_fleet$Standard_annual_full_time_employment_hours <- IRL_fleet$`Full-time equivalent (harmonised)`
IRL_fleet$Other_annual_fixed_costs <- IRL_fleet$`Other non-variable costs`
IRL_fleet$Vessel_value <- IRL_fleet$`Tangible asset value (replacement)`
IRL_fleet <- cbind(IRL_fleet, Annual_depreciation_rate=4)# guess
IRL_fleet <- cbind(IRL_fleet, Opportunity_interest_rate=4) # guess
IRL_fleet <- cbind(IRL_fleet, Annual_discount_rate=4) # guess
IRL_fleet <- subset(IRL_fleet, select = -c(code, `Total number of vessels`, `Total employed`, `Other income`, `Value of landings`, `Income from landings`, `Wages and salaries of crew`, `Unpaid labour value`, `Other variable costs`, `Fishing days`, `Other non-variable costs`, `Tangible asset value (replacement)`,`Full-time equivalent (harmonised)`))


Economics_fs <- rbind(IRL_fleet) #, DNK_fleet, EST_fleet, FIN_fleet, LAT_fleet, LIT_fleet, POL_fleet, SWE_fleet)

write.table(Economics_fs, file =file.path(dataPath, "STECF", "Economics_fs.csv"), sep=";",row.names=F,col.names=T, quote=FALSE)





 # export the raster in outPath
 writeRaster(rstr, file = file.path(outPath, 
                                paste0("05_rstr_gva.tif")), format = "GTiff", overwrite = TRUE)



