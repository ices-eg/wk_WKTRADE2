##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ## GOAL: COST SPATIAL DISAGREGATION coupling publically available STECF database with ICES VMS data
 ## to ultimately be able to retrieve contribution margin as income from landings - minus operating costs
 ## as raster files
 
 ## Developed for WKTRADE2, Aug 2019
 ## Erik Sulanke, Oisin Callery & Francois Bastardie
 
 ## Target: the AER cost structure disaggregated per fleet-segment (or métier) per c-square   

 ## STECF INPUT DATA: AER Cost ratio data per fleet-segment 
 ## STECF INPUT DATA: AER Effort per fleet-segment per FAO subregion 
 ## COMPUTED HERE: Effort share per fs per subregion (obtained from total AER effort per fs * fs_share_effort from prop of effort in the subregion)
 ## COMPUTED HERE: AER cost ratio data per fs per subregion
 ## TODO: (the missing link for now: FDI effort per fs (or best per métier) per ICES rectangle…so what is exactly needed is a share over ICES rectangles of total fs effort per subarea…Cannot this table be directly deduced from VMS data) 
 ## ICES INPUT DATA: VMS effort per métier level 6 per c-square
 ## COMPUTED HERE: coupling to obtain VMS cost structure per métier level 6/fs per c-square as VMS effort times cost ratios
 

 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
 ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


### Reading and exploring raw data ####
library("dplyr")
library("ggplot2")
library("scales")
library("ggpubr")
library("stringr")
library("lme4")
library("scales")

my_path_to_vms_data <- file.path("D:","FBA","ADVICES","ICES_WKTRADE2","WGSFD","wktrade2_vms") # ICES vms data cannot be shared without permission.
my_path_to_oth_data <- file.path("D:","FBA","ADVICES","ICES_WKTRADE2","wk_WKTRADE2", "WKTRADE2_Data", "STECF")

#### Reading and wrangling VMS Data ####
rawdata <- read.csv(file.path(my_path_to_vms_data,"vmsagg.txt"), header = T,sep = ",", stringsAsFactors = F)
str(rawdata)

#> head(rawdata)
#  year       c_square       LE_MET_level6 vessel_length_category    lat   lon  ecoregion mw_fishinghours subsurface    surface   totvalue   totweight
#1 2016 1500:249:384:1      DRB_MOL_>0_0_0                    o15 54.825 9.425 Baltic Sea        2.438367 0.51382804 0.51382804  7826.1745  25351.7903
#2 2017 1500:249:384:1      DRB_MOL_>0_0_0                    o15 54.825 9.425 Baltic Sea        3.094000 0.64346693 0.64346693 14198.1584  58766.8187
#3 2016 1500:249:384:1       DRB_MOL_0_0_0                    o15 54.825 9.425 Baltic Sea        0.739200 0.07376808 0.07376808  7008.0000  51908.0000
#4 2017 1500:249:384:2      DRB_MOL_>0_0_0                    o15 54.825 9.475 Baltic Sea        8.412733 1.76435085 1.76435085 28731.5047 160272.0083
#5 2015 1500:459:104:2      DRB_MOL_>0_0_0                    o15 55.025 9.475 Baltic Sea        0.129000 0.04044649 0.04044649   933.9597   7100.2292
#6 2014 1500:459:104:2 OTB_DEF_>=105_1_120                  12-15 55.025 9.475 Baltic Sea        0.162000 0.05041708 0.64637283   111.5187    118.8298


rawdata$lat <- as.numeric(rawdata$lat) #Converting lattitude to numeric variable
rawdata <- rawdata[!is.na(rawdata$lat),] #and removing NA

unique(rawdata$LE_MET_level6) # checking metier levels
unique(rawdata$vessel_length_category) # checking vessel lengths 

# redefining variables as factors and filtering df for North Sea Data of 2016
rawdata$vessel_length_category <- as.factor(rawdata$vessel_length_category)
rawdata$LE_MET_level6 <- as.factor(rawdata$LE_MET_level6)
rawdata$year <- as.factor(rawdata$year)
levels(rawdata$vessel_length_category)
rawdata$ecoregion <- as.factor(rawdata$ecoregion)
rawdataNS2016 <- filter(rawdata, year == "2016" & ecoregion == "Greater North Sea")
rawdataNS2016$vessel_length_category <- as.character(rawdataNS2016$vessel_length_category)
rawdataNS2016$vessel_length_category <- as.factor(rawdataNS2016$vessel_length_category)
levels(rawdataNS2016$vessel_length_category)

#Reordering levels of vessel length category in ascending order
rawdataNS2016$vessel_length_category <- factor(rawdataNS2016$vessel_length_category, levels = c("<8","8-10","10-12","12-15","o15"))

#Splitting the metier data using a temp dataframe
temp <- as.data.frame(str_split_fixed(rawdataNS2016$LE_MET_level6,"_",3))  
rawdataNS2016$gear <- temp[,1]
rawdataNS2016$assemblage <- temp[,2]
rawdataNS2016$meshsize <- temp[,3]

# redefining variables as factors
rawdataNS2016$gear <- as.factor(rawdataNS2016$gear)
rawdataNS2016$c_square <- as.factor(rawdataNS2016$c_square)

####  Checking gear table from joerg (VMS Data - DCF Data gears) ####
dcf_gears <- read.csv(file=file.path(my_path_to_oth_data, "codetransl_fish_tech08xx.csv"), sep = ";")
names(dcf_gears)[1]<-"gear"

#Joining gear table with VMS rawdata
rawdataNS2016_withgears <- left_join(rawdataNS2016,dcf_gears, by="gear")

# redefining variables as factors
sum(is.na(rawdataNS2016_withgears$FISHING_TECH)) #239 NAs - negligible
rawdataNS2016_withgears$vessel_length_category <- as.character(rawdataNS2016_withgears$vessel_length_category)
rawdataNS2016_withgears$vessel_length_category <- as.factor(rawdataNS2016_withgears$vessel_length_category)

#### Applying ICES-subregions to c-squares using Oisins Data ####
csquare_region_data <- read.csv(file=file.path(my_path_to_oth_data, "C_SQS.csv"))
csquare_region_data <- csquare_region_data[!is.na(csquare_region_data$EcoRegion),]
names(csquare_region_data)[2] <- "ecoregion"

#Joining geo information to vms data
rawdataNS2016_withgearsandregions <- left_join(rawdataNS2016_withgears, csquare_region_data, by=c("c_square","ecoregion"))

#Adjusting column names
names(rawdataNS2016_withgearsandregions)[16]<-"fishing_tech"
names(rawdataNS2016_withgearsandregions)[21]<-"sub_reg"

#### Calculating total value of landings per c-square and gear ###
#Removing NAs first
rawdataNS2016_withgearsandregions <- rawdataNS2016_withgearsandregions[!is.na(rawdataNS2016_withgearsandregions$fishing_tech),]
rawdataNS2016_withgearsandregions <- rawdataNS2016_withgearsandregions[!is.na(rawdataNS2016_withgearsandregions$sub_reg),]

#grouping and calculating
value_per_csquare_NS_2016 <- rawdataNS2016_withgearsandregions %>%
  group_by(c_square, fishing_tech) %>% #grouping
  mutate(csquare_sum_fishinghours = sum(mw_fishinghours)) %>% # sum of effort
  mutate(csquare_sum_value = sum(totvalue)) %>% # sum of value
  mutate(csquare_sum_weight = sum(totweight)) # sum of weight
value_per_csquare_NS_2016 <- ungroup(value_per_csquare_NS_2016) # ungroup to avoid warning messages

head(as.data.frame(value_per_csquare_NS_2016))

#Refine dataframe to all variables necessary for c-square x gear level
value_per_csquare_NS_2016 <- value_per_csquare_NS_2016[,c(1:2,16,20:24)]
value_per_csquare_NS_2016 <- unique(value_per_csquare_NS_2016)
value_per_csquare_NS_2016$c_square <- as.character(value_per_csquare_NS_2016$c_square)
value_per_csquare_NS_2016$c_square <- as.factor(value_per_csquare_NS_2016$c_square)

#### Fixing diverging subarea-levels
value_per_csquare_NS_2016$sub_reg <- as.character(value_per_csquare_NS_2016$sub_reg)
value_per_csquare_NS_2016$sub_reg[value_per_csquare_NS_2016$sub_reg == "27.3.a.20"] <- "27.3.a"
value_per_csquare_NS_2016$sub_reg[value_per_csquare_NS_2016$sub_reg == "27.3.a.21"] <- "27.3.a"
value_per_csquare_NS_2016$sub_reg <- as.factor(value_per_csquare_NS_2016$sub_reg)

#### Calculating subarea-sums of fishing techs
value_per_csquareandsubarea_NS_2016 <- value_per_csquare_NS_2016 %>%
  group_by(fishing_tech,sub_reg)%>%
  mutate(subreg_sum_fishinghours = sum(csquare_sum_fishinghours))%>% # subarea sum of effort
  mutate(subreg_sum_fishingvalue = sum(csquare_sum_value))%>% # subarae sum of weight
  mutate(subreg_sum_fishingweight = sum(csquare_sum_weight))# subarae sum of weight
value_per_csquareandsubarea_NS_2016 <- ungroup(value_per_csquareandsubarea_NS_2016)

head(as.data.frame(value_per_csquareandsubarea_NS_2016))

###Reading AER value data ####
#stecf_fleetdata <- read.csv(file=file.path(my_path_to_oth_data, "STECF_EU_Fleet_Landings_FS-Level.csv"), sep = ";", dec = ",") ## TOO BIG FOR GITHUB!
stecf_fleetdata <- read.csv(file=file.path("D:","FBA","ADVICES","ICES_WKTRADE2", "STECF_EU_Fleet_Landings_FS-Level","STECF_EU_Fleet_Landings_FS-Level.csv"), sep = ";", dec = ",")
stecf_fleetdata <- stecf_fleetdata[,c(2,4:7,9:14,16:20,27)]
str(stecf_fleetdata)
stecf_fleetdata$year <- as.character(stecf_fleetdata$year)
stecf_fleetdata$year <- as.factor(stecf_fleetdata$year)
# setting -1 values to 0
stecf_fleetdata$value[stecf_fleetdata$value < 0] <- 0

# filtering for 2016 data and area 27, calculating value of landings ####
stecf_fleetdata_2016 <- filter(stecf_fleetdata,year==2016)
stecf_fleetdata_2016 <- filter(stecf_fleetdata_2016,supra_reg=="AREA27",variable_name=="Value of landings")
stecf_fleetdata_2016 <- stecf_fleetdata_2016[,c(1:5,7,11:17)]

#filtering for 2016 data and area 27, calculating kilo of landings ####
stecf_fleetdata_2016_kilo <- filter(stecf_fleetdata,year==2016)
stecf_fleetdata_2016_kilo <- filter(stecf_fleetdata_2016_kilo,supra_reg=="AREA27",variable_name=="Live weight of landings")
names(stecf_fleetdata_2016_kilo)[11]<-"weight"
stecf_fleetdata_2016_kilo <- stecf_fleetdata_2016_kilo[,c(1:5,7,11:17)]

#joining AER value and weight data
stecf_fleetdata_2016_with_kilo <- left_join(stecf_fleetdata_2016, stecf_fleetdata_2016_kilo, by=c("year","supra_reg","fishing_tech","vessel_length","fs_name","sub_reg","gear_type","country_code","fishing_activity","species_namesc","species_name","species_code"))
#removing rows containing weight NAs
stecf_fleetdata_2016_with_kilo <- stecf_fleetdata_2016_with_kilo[!is.na(stecf_fleetdata_2016_with_kilo$weight),]

#Refine dataframe to all variables necessary
stecf_fleetdata_2016_with_kilo <- stecf_fleetdata_2016_with_kilo[,c(1:6,10:12,8,9,13,7,14)]

#### ### Reading and exploring STECF effort Data ####
stecf_fleetdata_effort <- read.csv(file=file.path(my_path_to_oth_data, "STECF_EU_Fleet_Effort_FS-Level.csv"), sep = ";", dec = ",")
### Neeed to calculate MW_fishing hours - this is not in the AER and thus has to be constructed.
# Aproximation: (Fishing days/days at sea * kW hours at Sea)/1000 
stecf_fleetdata_effort_daysatsea <- filter(stecf_fleetdata_effort, variable_name=="Days at sea")
stecf_fleetdata_effort_fishingdays <- filter(stecf_fleetdata_effort, variable_name=="Fishing days")
stecf_fleetdata_effort_kwHoursSea <- filter(stecf_fleetdata_effort, variable_name=="kW hours at sea")
stecf_fleetdata_effort_daysatsea <- stecf_fleetdata_effort_daysatsea[,c(2,4:7,10,14:15)]
stecf_fleetdata_effort_fishingdays <- stecf_fleetdata_effort_fishingdays[,c(2,4:7,10,14:15)]
stecf_fleetdata_effort_kwHoursSea <- stecf_fleetdata_effort_kwHoursSea[,c(2,4:7,10,14:15)]
names(stecf_fleetdata_effort_daysatsea)[7] <- "daysatsea"
names(stecf_fleetdata_effort_fishingdays)[7] <- "fishingdays"
names(stecf_fleetdata_effort_kwHoursSea)[7] <- "kwHoursSea"

#Joining days at sea and fishing days df
stecf_fleetdata_effort_kwHoursfishing <- left_join(stecf_fleetdata_effort_kwHoursSea,stecf_fleetdata_effort_fishingdays, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name","sub_reg"))
#joining days at sea/fishing days dataframe with kwHoursSea dataframe
stecf_fleetdata_effort_kwHoursfishing <- left_join(stecf_fleetdata_effort_kwHoursfishing,stecf_fleetdata_effort_daysatsea, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name","sub_reg"))
### Neeed to calculate MW_fishing hours - this is not in the AER and thus has to be constructed.
# Aproximation: (Fishing days/days at sea * kW hours at Sea)/1000 
stecf_fleetdata_effort_kwHoursfishing$mw_fishinghours <- (stecf_fleetdata_effort_kwHoursfishing$fishingdays/stecf_fleetdata_effort_kwHoursfishing$daysatsea*
                                                           stecf_fleetdata_effort_kwHoursfishing$kwHoursSea)/1000                       
table(is.na(stecf_fleetdata_effort_kwHoursfishing$mw_fishinghours))

## !!!! CAUTION !!!!!
## !!!! CAUTION !!!!!
## !!!! CAUTION !!!!!
## TRYING TO WORKAROUND THE BAD FISHING DAYS FIELD HAVING A LOT OF NAs
#stecf_fleetdata_effort_kwHoursfishing$mw_fishinghours <- stecf_fleetdata_effort_kwHoursfishing$daysatsea/24*0.5                    
## !!!! CAUTION !!!!!
## !!!! CAUTION !!!!!
## !!!! CAUTION !!!!!


#Refine dataframe to relevant variables
stecf_fleetdata_effort_kwHoursfishing <- stecf_fleetdata_effort_kwHoursfishing[,c(1,2,4,5,8,11)]
stecf_fleetdata_effort_kwHoursfishing$year <- as.factor(stecf_fleetdata_effort_kwHoursfishing$year)

#Joining dataframe with value and weight with the effort-dataframe
stecf_fleetdata_2016_with_kiloandeffort <- left_join(stecf_fleetdata_2016_with_kilo,stecf_fleetdata_effort_kwHoursfishing, by=c("country_code","year","fishing_tech","vessel_length","sub_reg"))

#grouping for subregion and fleet segment(=country, gear and vessel length), calculating sum value 
stecf_fleetdata_2016_with_kiloandeffort_fslevel <- stecf_fleetdata_2016_with_kiloandeffort %>%
  group_by(country_code,fishing_tech,vessel_length,sub_reg)%>%
  mutate(AER_sumvalue_subreg = sum(value))%>% # sum of value
  mutate(AER_sumweight_subreg = sum(weight))%>% #sum of weight
  mutate(AER_sumeffort_subreg = sum(mw_fishinghours, na.rm = FALSE)) # sum of effort (no NA remove to avoid underestimation of effort sum)




  ## CAUTION: SOM NAs because fishingdays not informed for all records!!!!





#refine dataframe to relevant information
stecf_fleetdata_2016_with_kiloandeffort_fslevel <- stecf_fleetdata_2016_with_kiloandeffort_fslevel[,c(1,4,5,7,16:18)]
stecf_fleetdata_2016_with_kiloandeffort_fslevel <- unique(stecf_fleetdata_2016_with_kiloandeffort_fslevel)

#calculate share of fleet segments on value and catch
stecf_fleetdata_2016_with_kiloandeffort_fslevel <- stecf_fleetdata_2016_with_kiloandeffort_fslevel %>%
  group_by(fishing_tech, sub_reg)%>%
  mutate(fs_share_value = AER_sumvalue_subreg/sum(AER_sumvalue_subreg))%>% #share of fleet segment on total value landed by fishing tech
  mutate(fs_share_weight = AER_sumweight_subreg/sum(AER_sumweight_subreg))%>% #share of fleet segment on total weight by fishing tech
  mutate(fs_share_effort = AER_sumeffort_subreg/sum(AER_sumeffort_subreg, na.rm=TRUE))#share of fleet segment on total effort by fishing tech

### reading AER costratios ####
AER_costratios <- read.csv(file=file.path(my_path_to_oth_data, "AER_Costratios.csv"),sep = ";",dec = ",")
AER_costratios$year <- as.character(AER_costratios$year)
#filtering for 2016 data and redefining columns
AER_costratios <- filter(AER_costratios, year== "2016")
AER_costratios <- AER_costratios[,c(1,3:4,7,10,12,14)]

#joining AER dataframe with value, weight and effort with costratio-dataframe
stecf_fleetdata_2016_with_kiloandeffort_fslevel_andcostratios <- left_join(stecf_fleetdata_2016_with_kiloandeffort_fslevel,AER_costratios,
                                                                           by= c("country_code","fishing_tech","vessel_length"))


#### Joining AER to VMS Data on c-square level ####
value_per_csquareandsubarea_NS_2016_withAER <- left_join(value_per_csquareandsubarea_NS_2016, stecf_fleetdata_2016_with_kiloandeffort_fslevel_andcostratios, by=c("fishing_tech","sub_reg"))

#filtering for subregions contained in the vms data
value_per_csquareandsubarea_NS_2016_withAER <- filter(value_per_csquareandsubarea_NS_2016_withAER, sub_reg %in% c("27.3.a","27.3.b.23","27.4.a","27.4.b","27.4.c","27.7.d","27.7.e"))
value_per_csquareandsubarea_NS_2016_withAER$sub_reg <- as.factor(value_per_csquareandsubarea_NS_2016_withAER$sub_reg)

#### Interlude for creating bar plot of subreg totals###
####Build remodeled dataframe for barplotting containing only subreg totals ####
### VMS
value_per_subarea_NS_2016_VMS <- value_per_csquareandsubarea_NS_2016_withAER[,c(3,5,9:11)]
value_per_subarea_NS_2016_VMS <- unique(value_per_subarea_NS_2016_VMS)
#Calculating subreg totals
value_per_subarea_NS_2016_VMS <- value_per_subarea_NS_2016_VMS %>%
  group_by(sub_reg)%>%
  mutate(subreg_total_fishinghours = sum(subreg_sum_fishinghours, na.rm = T))%>%
  mutate(subreg_total_fishingvalue = sum(subreg_sum_fishingvalue, na.rm = T))%>%
  mutate(subreg_total_fishingweight = sum(subreg_sum_fishingweight, na.rm = T))
value_per_subarea_NS_2016_VMS <- ungroup(value_per_subarea_NS_2016_VMS)

value_per_subarea_NS_2016_VMS <- value_per_subarea_NS_2016_VMS[,c(2,6:8)]
value_per_subarea_NS_2016_VMS <- unique(value_per_subarea_NS_2016_VMS)

###AER
value_per_subarea_NS_2016_AER <- stecf_fleetdata_2016_with_kiloandeffort_fslevel_andcostratios[,c(2,4:6)]

value_per_subarea_NS_2016_AER <- value_per_subarea_NS_2016_AER %>%
  group_by(sub_reg)%>%
  mutate(AER_totalvalue_subreg = sum(AER_sumvalue_subreg))%>%
  mutate(AER_totalkilo_subreg = sum(AER_sumweight_subreg))
value_per_subarea_NS_2016_AER <- ungroup(value_per_subarea_NS_2016_AER)

value_per_subarea_NS_2016_AER <- value_per_subarea_NS_2016_AER[,c(2,5,6)]
value_per_subarea_NS_2016_AER <- unique(value_per_subarea_NS_2016_AER)

####Joining VMS and AER subreg total dfs ####
value_per_subarea_NS_2016_withAER <- left_join(value_per_subarea_NS_2016_VMS,value_per_subarea_NS_2016_AER, by="sub_reg")

Value_persubarea_VMS <- value_per_subarea_NS_2016_withAER[,c(1,3,4)]
Value_persubarea_VMS <- unique(Value_persubarea_VMS)
names(Value_persubarea_VMS)[2] <- "value"
names(Value_persubarea_VMS)[3] <- "weight"
Value_persubarea_VMS$datasource <- "VMS"

value_persubarea_AER <- value_per_subarea_NS_2016_withAER[,c(1,5,6)]
value_persubarea_AER <- unique(value_persubarea_AER)
names(value_persubarea_AER)[2] <- "value"
names(value_persubarea_AER)[3] <- "weight"
value_persubarea_AER$datasource <- "AER"

Values_perRegion_longformat <- bind_rows(Value_persubarea_VMS,value_persubarea_AER)
Values_perRegion_longformat$sub_reg <- as.factor(Values_perRegion_longformat$sub_reg)
Values_perRegion_longformat$datasource <- as.factor(Values_perRegion_longformat$datasource)

### Plots ####
barplot1 <- ggplot(Values_perRegion_longformat, aes(sub_reg, value, fill=datasource))+geom_col(position = "dodge")+scale_y_continuous(labels = comma)+labs(x="subregion", y="total catch value [â‚¬]", title = " catch value")+theme(axis.title.x = element_blank(),axis.text.x = element_blank())
barplot2 <- ggplot(Values_perRegion_longformat, aes(sub_reg, weight, fill=datasource))+geom_col(position = "dodge")+scale_y_continuous(labels = comma)+labs(x="subregion", y="total catch weight [kg]", title = "catch weight")

ggarrange(barplot1,barplot2, common.legend = T, legend = "right", nrow = 2, ncol = 1)


#### Continue working on joined VMS and AER data - applying fleet segment shares from AES data to VMS ####
fleetsegment_valueandweight_per_csquareandsubarea_NS_2016 <- value_per_csquareandsubarea_NS_2016_withAER
fleetsegment_valueandweight_per_csquareandsubarea_NS_2016 <- fleetsegment_valueandweight_per_csquareandsubarea_NS_2016[,c(2:8,12:23)]

### Calculating c-square value, weight, effort and costs with the AER share calculations
#calculating effort
fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$value_recalculated <- fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$csquare_sum_value*
  fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$fs_share_value

#calculating weight
fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$weight_recalculated <- fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$csquare_sum_weight*
  fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$fs_share_weight

#calculating effort
fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$effort_recalculated <- fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$csquare_sum_fishinghours*
  fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$fs_share_effort

#calculating energy cost (effort by ratio)
fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$calculated_energycost <- (fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$effort_recalculated*1000/24)*
  fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$enerbykwfishdy

#calculating wages (value by ratio)
fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$calculated_wages <- (fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$value_recalculated)*
  fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$wagebyinc

#calculating repair (effort by ratio)
fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$calculated_repaircost <- (fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$effort_recalculated*1000/24)*
  fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$repbykwfishday

#calculating variable costs (effort by ratio)
fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$calculated_variabelcost <- (fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$effort_recalculated*1000/24)*
  fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$varbykwfishday


#check
head(as.data.frame(fleetsegment_valueandweight_per_csquareandsubarea_NS_2016))
table(is.na(fleetsegment_valueandweight_per_csquareandsubarea_NS_2016$effort_recalculated))   # are there NAs?


####Creating the rasterframe and reducing to relevant information
rasterframe <- fleetsegment_valueandweight_per_csquareandsubarea_NS_2016[,c(1:4,8:9,20:26)]
rasterframe <- rasterframe[,c(1,3,4,2,5:13)]

#calculating total costs (adding up all costs)
rasterframe$calculated_totalcost <- rasterframe$calculated_energycost +
  rasterframe$calculated_wages +
  rasterframe$calculated_repaircost +
  rasterframe$calculated_variabelcost

str(rasterframe)
### This is the rasterframe --- It contains info on calculated catch value, catch weight and costs per fleet segment ####
### Fleet segment is the combination of country, fishing tech and vessel length
### It contains a lot of NAs --- Unfortunately this is the best we can get from the given data!



# convert to raster

mainPath    <- file.path("D:","FBA","ADVICES", "ICES_WKTRADE2") 
repoPath    <- file.path(mainPath, "wk_WKTRADE2") # github repo
dataPath    <- file.path(repoPath, "WKTRADE2_Data")
outPath     <- file.path(dataPath, "ShapeFiles", "WKTRADE2")



library(raster)
library(vmstools)
rasterframe <- cbind.data.frame( rasterframe,
                      vmstools::CSquare2LonLat (as.character(rasterframe$c_square), degrees=0.05 )
                                                  )
rangex <- range(rasterframe$SI_LONG)                                                  
rangey <- range(rasterframe$SI_LATI)                                                  
 
 
r           <- raster(xmn=rangex[1], xmx=rangex[2], ymn=rangey[1], ymx=rangey[2], res=c(0.0501, 0.0501),
                             crs=CRS("+proj=longlat +datum=WGS84"))
some_coords <- SpatialPoints(cbind(lon=rasterframe$SI_LONG, lat=rasterframe$SI_LATI))
rstr_totcost        <- rasterize(x=some_coords, y=r, field=quantile(rasterframe$calculated_totalcost, prob=seq(0,1,by=0.1), na.rm=TRUE), fun=sum) 
crs(rstr_totcost) <- "+proj=longlat +datum=WGS84"                
rstr_energycost        <- rasterize(x=some_coords, y=r, field=quantile(rasterframe$calculated_energycost, prob=seq(0,1,by=0.1), na.rm=TRUE), fun=sum) 
crs(rstr_energycost) <- "+proj=longlat +datum=WGS84"                
#rstr_effort        <- rasterize(x=some_coords, y=r, field=quantile(rasterframe$effort_recalculated, prob=seq(0,1,by=0.1), na.rm=TRUE), fun=sum) 
#crs(rstr_effort) <- "+proj=longlat +datum=WGS84"                
rstr_effort        <- rasterize(x=some_coords, y=r, field=quantile(value_per_csquareandsubarea_NS_2016$csquare_sum_fishinghours, prob=seq(0,1,by=0.1), na.rm=TRUE), fun=sum) 
crs(rstr_effort) <- "+proj=longlat +datum=WGS84"                
rstr_landingvalue        <- rasterize(x=some_coords, y=r, field=quantile(rasterframe$value_recalculated, prob=seq(0,1,by=0.1), na.rm=TRUE), fun=sum) 
crs(rstr_landingvalue) <- "+proj=longlat +datum=WGS84"                


# compute a margin contribution
rstr_margincontribution <- rstr_landingvalue - rstr_energycost

# check
plot((rstr_energycost/cellStats(rstr_energycost, "max"))-((rstr_effort/cellStats(rstr_effort, "max"))))

# PLOTS
par(mfrow=c(2,2))
#plot(rstr_effort/cellStats(rstr_effort, "max"))
#title("Effort")
plot(rstr_landingvalue/cellStats(rstr_landingvalue, "max"))
title("Value")
plot(rstr_energycost/cellStats(rstr_energycost, "max"))
title("Energy cost")
plot(rstr_totcost/cellStats(rstr_totcost, "max"))
title("Total cost")
plot(rstr_margincontribution/cellStats(rstr_margincontribution, "max"))
title("Margin Contribution")
savePlot(file.path(outPath, paste0("10_rstrs.png")), type="png")


par(mfrow=c(2,2))
rst_eff_contr <- rstr_effort/cellStats(rstr_effort, "sum")
plot(rst_eff_contr/cellStats(rst_eff_contr, "max"))
title("Effort")
rst_landv_contr <- rstr_landingvalue/cellStats(rstr_landingvalue, "sum")
plot(rst_landv_contr/cellStats(rst_landv_contr, "max"))
title("Landing Value")
rst_energyc_contr <- rstr_energycost/cellStats(rstr_energycost, "sum")
plot(rst_energyc_contr/cellStats(rst_energyc_contr, "max"))
title("Energy cost")
rst_totcost_contr <- rstr_totcost/cellStats(rstr_totcost, "sum")
plot(rst_totcost_contr/cellStats(rst_totcost_contr, "max"))
title("Total cost")

# check
plot((rst_energyc_contr/cellStats(rst_energyc_contr, "max"))-((rst_eff_contr/cellStats(rst_eff_contr, "max"))))

 
# export the layer and use in AddAttributeToShpFromRasterExtractorFromShpOverlay.R
writeRaster(rstr_energycost, file = file.path(outPath, 
                                paste0("10_rstr_energycost.tif")), format = "GTiff", overwrite = TRUE)
# export the layer and use in AddAttributeToShpFromRasterExtractorFromShpOverlay.R
writeRaster(rstr_totcost, file = file.path(outPath, 
                                paste0("10_rstr_totcost.tif")), format = "GTiff", overwrite = TRUE)
# export the layer and use in AddAttributeToShpFromRasterExtractorFromShpOverlay.R
writeRaster(rstr_effort, file = file.path(outPath, 
                                paste0("10_rstr_effort.tif")), format = "GTiff", overwrite = TRUE)



head(as.data.frame(rasterframe))

