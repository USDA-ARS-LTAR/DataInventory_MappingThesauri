##Script to pull in data inventories from the LTAR NEtwork, bind and anlyze them
##Assign Network categories and keywords, as well as attributes to AgCROS tables
##Next version shall assign only one category, yet allow multiple terms comma-delimited to be listed under keywords
##Nicole Kaplan, Bryan Carlson 3/7/2019

get.inventory <- function() {
  library(tidyverse)
  #library(fuzzyjoin)
  
  #dir <- "//10.177.16.112/cdo/TEAM/inventories"
  #setwd(dir)
  
  
  #### 1. appending all of the inventories into one ####
  #setwd("Y:/inventories/csv_Inventories")
  
  filename<-as.data.frame(dir("input/csv_inventories")) 
  colnames(filename)[1]<-'filename'
  filename$filename<-as.character(filename$filename)
  filename<-filename %>%  
    filter(str_detect(filename, paste(c("txt")))) 
  
  
  
  l<-NROW(filename$filename)
  
  inventory.1 <- data.frame()
  
  
  for (i in 1:l) {
    #i=2
    fn=paste("input","csv_inventories",filename[i,], sep = "/")
    filedata <- read.delim(fn, header = T, sep = "\t", stringsAsFactors = FALSE)
    inventory.1 <- rbind(inventory.1, filedata)
  }
  
  #### 2. Adding additional columns to further categorize data (still as general typology vs attribute definition - next step) ####
  
  inventory.1$Network.Category <- ""  #Assigned in section 3.1
  inventory.1$AgCROS.Table <-""       #Assigned in section 3.2
  inventory.1$AgCROS.VarName <- ""    # Assigned by site and work through individual categories first, perhaps as google sheet with lookup, section 4.1?
  inventory.1$Repo.Name <- "" #(eg AmeriFlux, ) #Assigned in section 3.2
  inventory.1$subsite <- ""           #Needs to be addessed by each site, see MN as an example 3.3
  inventory.1$NALT.Keywords <- ""     #3.4
  #inventory.1$Model.Name <- ""  #3.5 Ask Bryan if there is a list of models that may have included simulated data to be included as a lookup
  
  #### 3. Working with intgrated data frame from all sites ####
  
  alldata<- inventory.1
  
  #### 3.1 assign ONE network category to each site category ####
  alldata$Network.Category[alldata$Category%in%c("Animal", "Animals", "ANIMAL", "Rangeland")]<-"Animals"
  alldata$Network.Category[alldata$Category%in%c("Manure")]<-"Manure"
  alldata$Network.Category[alldata$Category%in%c("Wildlife", "Biodiversity", "Species List", "Habitat")]<-"Biodiversity"
  alldata$Network.Category[alldata$Category%in%c("Geospatial", "Landbase")]<-"Geospatial Data"
  alldata$Network.Category[alldata$Category%in%c("Gass Fraction", "Gas Fraction", "GREENHOUSE GAS FLUX", "Greenhouse Gas Flux", "greenhouse gas flux", "Greenhouse Gases")]<-"Greenhouse Gas Emissions"
  alldata$Network.Category[alldata$Category%in%c("Water Flow", "Hydrology", "Ecohydrology", "RUNOFF", "Runoff")]<-"Hydrology"
  alldata$Network.Category[alldata$Category%in%c("Evapotranspiration")]<-"Evapotranspiration"
  alldata$Network.Category[alldata$Category%in%c("Land Management")]<-"Land Management"
  alldata$Network.Category[alldata$Category%in%c("Farm Management", "FarmManagement")]<-"Land Management"
  alldata$Network.Category[alldata$Category%in%c("Land Management") & alldata$Site.VariableName%in%c("harvest")]<-"Land Management"
  alldata$Network.Category[alldata$Category%in%c("LAND USE", "Land Use")]<-"Land Management"
  alldata$Network.Category[alldata$Category%in%c("Meteorological", "Meteorology", "Meterology", "Precipitation")]<-"Meteorology"
  alldata$Network.Category[alldata$Category%in%c("EC Flux Towers", "Ecosystem Flux", "Eddy Flux", "Eddy flux", "METEOROLOGY (Eddy covariance)", "EDDY COVARIANCE", "Covariance Easy Flux Continous", "LAND-ATMOSPHERE FLUXES (EC)", "METEOROLOGY + EC", "METEOROLOGY (EC)")]<-"Eddy Covariance"
  alldata$Network.Category[alldata$Category%in%c("Micrometeorology")]<-"Meteorology"
  alldata$Network.Category[alldata$Category%in%c("Modeling", "MODELING")]<-"Models"
  alldata$Network.Category[alldata$Category%in%c("pesticide")]<-"Pesticides"
  alldata$Network.Category[alldata$Category%in%c("RemoteSensing, Remote Sensing", "Remote sensing", "AERIAL IMAGERY", "DEM")]<-"Remote Sensing"
  alldata$Network.Category[alldata$Category%in%c("Phenology", "Phenocam Continuous", "Phenocam", "PHENOLOGY")]<-"Phenology"
  alldata$Network.Category[alldata$Category%in%c("Projects Data")]<-"Research Projects"
  alldata$Network.Category[alldata$Category%in%c("Reservior", "RESERVIOR", "Sediment", "SEDIMENT")]<-"Sediments"
  alldata$Network.Category[alldata$Category%in%c("Soil", "Soils", "SOIL", "Soil characteristics", "SOIL CHARACTERISTICS", "SOIL WATER", "SOIL COVER", "soils")]<-"Soil"
  alldata$Network.Category[alldata$Category%in%c("SOIL PHYSICS")]<-"Soil"
  alldata$Network.Category[alldata$Category%in%c("SOIL BIOLOGY", "SOIL MICROBIAL")]<-"Soil"
  alldata$Network.Category[alldata$Category%in%c("Soil Properties")]<-"Soil"
  alldata$Network.Category[alldata$Category%in%c("SOIL CHEMISTRY", "Soil Sampling Mineral Analysis 2016", "Soil Sampling Mineral Analysis 2017","Soil Sampling Mineral Analysis 2018", "Soil Sampling Physical Analysis 2016", "Soil Sampling Physical Analysis 2017","Soil Sampling Physical Analysis 2018")]<-"Soil"
  alldata$Network.Category[alldata$Category%in%c("Soil-Atmosphere Gas Flux", "SOIL ECOSYSTEM EXCHANGES")]<-"Soil"
  alldata$Network.Category[alldata$Category%in%c("Soil/Plant")]<-"Soil Plant Interactions"
  alldata$Network.Category[alldata$Category%in%c("Plant", "PLANT FRACTION", "Vegetation", "VEGETATION", "Primary Production", "Vegetation Composition")]<-"Vegetation"
  alldata$Network.Category[alldata$Category%in%c("BIOMASS CARBOHYDRATE ANALYSIS", "BIOMASS MINERAL ANALYSIS", "BIOMASS ENERGY")]<-"Vegetation"
  alldata$Network.Category[alldata$Category%in%c("Crop", "CROP")]<-"Vegetation"
  alldata$Network.Category[alldata$Category%in%c("HARVEST REMOVAL")]<-"Vegetation"
  alldata$Network.Category[alldata$Category%in%c("Forage", "GRAZING PLANTS")]<-"Vegetation"
  alldata$Network.Category[alldata$Category%in%c("Forage")]<-"Vegetation"
  alldata$Network.Category[alldata$Category%in%c("Weed")]<-"Vegetation"
  alldata$Network.Category[alldata$Category%in%c("Water Quality", "WATER QUALITY", "water/soil chemistry")]<-"Water Quality"
  alldata$Network.Category[alldata$Category%in%c("STREAM CROSS-SECTION SURVEYS", "STREAM THALWEG SURVEYS")]<-"Hydrology"
  alldata$Network.Category[alldata$Category%in%c("Lake")]<-"Water Quality"
  alldata$Network.Category[alldata$Category%in%c("Water Quality") & alldata$Site.VariableName%in%c("pp' -DDD", "pp'DDE", "pp'-DDT")]<-"Water Quality"
  alldata$Network.Category[alldata$Category%in%c("Wind Erosion")]<-"Wind Erosion"
  alldata$Network.Category[alldata$Category%in%c("Wind Erosion") & alldata$Site.VariableName%in%c("time lapse camera")]<-"Wind Erosion"
  
  
  
  
  
  #### 3.2 assign AgCROS table names from enterprise data system or Repo.Name to Network.Categories  ####
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Animals, Animal Science, Rangeland")] <-"MgtAnimal, MeasAnimal"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Biodiversity")] <-"New AgCROS table TBD"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Vegetation, Biomass")] <-"MeasBiomassCHO, MeasBiomassEnergy, MeasBiomassMinAn"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Vegetation, Crop, Harvesting")] <-"MgtAmendments, MgtPlanting, MgtTillage, MgtGrowthStages, MgtResidue, MgtGrazing, MeasResidueMgt, MeasHarvestFraction"
  alldata$Repo.Name[alldata$Network.Category%in%c("Meteorology, Eddy Covariance")] <-"AmeriFlux"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Hydrology, Evapotranspiration")] <-"STEWARDS Water Quality Cemistry tables that will be added to DET"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Land Management", "Land Management, Farm Management")] <-"MgtAmendments, MgtPlanting, MgtTillage, MgtGrowthStages, MgtResidue, MgtGrazing"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Geospatial Data")] <-"Imagery, Shapefiles, metadata tables DET"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Vegetation, Grazing, Biomass", "Vegetation, Grazing")] <-"MgtGrazing, MeasGrazingPlants, MgtAnimal, MeasAnimal"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Greenhouse Gas Emissions", "Soil, Vegetation, Soil Respiration, Greenhouse Gas Emissions")] <-"MeasGHGFlux"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Hydrology")] <-"STEWARDS Water Quality, Discharge, and Runoff tables that will be added to DET"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Vegetation, Invasive Species")] <-"MeasSoilCover or New AgCROS table TBD"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Water Quality, Lake")] <-"STEWARDS Water Quality table that will be added to DET"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Land Use, Geospatial Data")] <-"GIS Layer in AgCROS"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Animals, Soil, Manure", "Pesticides")] <-"MgtAmendents"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Meteorology")] <-"WeatherDaily, WeatherStation"
  alldata$Repo.Name[alldata$Network.Category%in%c("Meteorology")] <-"NAL Met Data Dashboard"
  alldata$Model.Name[alldata$Network.Category%in%c("Models")] <-"Look up model name"  
  alldata$Repo.Name[alldata$Network.Category%in%c("Remote Sensing", "Drone")] <-"ARS Imagery Server"
  alldata$Repo.Name[alldata$Network.Category%in%c("Remote Sensing, Phenology", "Phenocam, Drone")] <-"PhenoCam Network"
  alldata$Repo.Name[alldata$SpatialExtentDescription%in%c("Drone")] <-"ARS Imagery Server"
  alldata$Repo.Name[alldata$SpatialExtentDescription%in%c("Phenocam, Drone")] <-"PhenoCam Network"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Research Projects")] <-"AgCROS overview tables"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Sediments")] <-" Soil, Soil Biology, Soil Chemistry, or Soil Physical"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Soil", "Soil, Soil Biology, Soil Physics, Soil Chemistry")] <-"MeasSoilCover, MeasSoilChem, MeasSoilPhys, MeasSoilBiol"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Soil, Soil Chemistry")] <-"MeasSoilChem"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Soil Physics")] <-"MeasSoilPhys"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Soil, Soil Biology")] <-"MeasSoilBiol"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Soil, Soil Respiration" )] <-"MeasGHGFlux"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Vegetation" )] <-"MeasGrazingPlants, MeasResidueMgt, MeasHarvestFraction"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Water Quality")] <-"STEWARDS Water Quality table that will be added to AgCROS"
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Wind Erosion" )] <-"MeasWindErosionArea, MeasWindErosionConc"
  
  
  #### 3.3 assign NALT keywords ####
  
  alldata$NALT.Keywords[alldata$Category%in%c("Animal", "Animals", "ANIMAL", "Rangeland")]<-"Animals, Animal Science, Rangeland"
  alldata$NALT.Keywords[alldata$Category%in%c("Manure")]<-"Animals, Soil, Manure"
  alldata$NALT.Keywords[alldata$Category%in%c("Wildlife", "Biodiversity", "Species List", "Habitat")]<-"Biodiversity"
  alldata$NALT.Keywords[alldata$Category%in%c("Geospatial", "Landbase")]<-"Geospatial Data"
  alldata$NALT.Keywords[alldata$Category%in%c("Gass Fraction", "Gas Fraction", "GREENHOUSE GAS FLUX", "Greenhouse Gas Flux", "greenhouse gas flux", "Greenhouse Gases")]<-"Greenhouse Gas Emissions"
  alldata$NALT.Keywords[alldata$Category%in%c("Water Flow", "Hydrology", "Ecohydrology", "RUNOFF", "Runoff")]<-"Hydrology"
  alldata$NALT.Keywords[alldata$Category%in%c("Evapotranspiration")]<-"Hydrology, Evapotranspiration"
  alldata$NALT.Keywords[alldata$Category%in%c("Land Management")]<-"Land Management"
  alldata$NALT.Keywords[alldata$Category%in%c("Farm Management", "FarmManagement")]<-"Land Management, Farm Management"
  alldata$NALT.Keywords[alldata$Category%in%c("Land Management") & alldata$Site.VariableName%in%c("harvest")]<-"Land Management, Harvesting"
  alldata$NALT.Keywords[alldata$Category%in%c("LAND USE", "Land Use")]<-"Land Use, Geospatial Data"
  alldata$NALT.Keywords[alldata$Category%in%c("Meteorological", "Meteorology", "Meterology", "Precipitation")]<-"Meteorology"
  alldata$NALT.Keywords[alldata$Category%in%c("EC Flux Towers", "Ecosystem Flux", "Eddy Flux", "Eddy flux", "METEOROLOGY (Eddy covariance)", "EDDY COVARIANCE", "Covariance Easy Flux Continous", "LAND-ATMOSPHERE FLUXES (EC)", "METEOROLOGY + EC", "METEOROLOGY (EC)")]<-"Meteorology, Eddy Covariance"
  alldata$NALT.Keywords[alldata$Category%in%c("Micrometeorology")]<-"Meteorology"
  alldata$NALT.Keywords[alldata$Category%in%c("Modeling", "MODELING")]<-"Models"
  alldata$NALT.Keywords[alldata$Category%in%c("pesticide")]<-"Pesticides"
  alldata$NALT.Keywords[alldata$Category%in%c("RemoteSensing, Remote Sensing", "Remote sensing", "AERIAL IMAGERY", "DEM")]<-"Remote Sensing"
  alldata$NALT.Keywords[alldata$Category%in%c("Phenology", "Phenocam Continuous", "Phenocam", "PHENOLOGY")]<-"Remote Sensing, Phenology"
  alldata$NALT.Keywords[alldata$Category%in%c("Projects Data")]<-"Research Projects"
  alldata$NALT.Keywords[alldata$Category%in%c("Reservior", "RESERVIOR", "Sediment", "SEDIMENT")]<-"Sediments"
  alldata$NALT.Keywords[alldata$Category%in%c("Soil", "Soils", "SOIL", "Soil characteristics", "SOIL CHARACTERISTICS", "SOIL WATER", "SOIL COVER", "soils")]<-"Soil"
  alldata$NALT.Keywords[alldata$Category%in%c("SOIL PHYSICS")]<-"Soil Physics"
  alldata$NALT.Keywords[alldata$Category%in%c("SOIL BIOLOGY", "SOIL MICROBIAL")]<-"Soil, Soil Biology"
  alldata$NALT.Keywords[alldata$Category%in%c("Soil Properties")]<-"Soil, Soil Biology, Soil Physics, Soil Chemistry"
  alldata$NALT.Keywords[alldata$Category%in%c("SOIL CHEMISTRY", "Soil Sampling Mineral Analysis 2016", "Soil Sampling Mineral Analysis 2017","Soil Sampling Mineral Analysis 2018", "Soil Sampling Physical Analysis 2016", "Soil Sampling Physical Analysis 2017","Soil Sampling Physical Analysis 2018")]<-"Soil, Soil Chemistry"
  alldata$NALT.Keywords[alldata$Category%in%c("Soil-Atmosphere Gas Flux", "SOIL ECOSYSTEM EXCHANGES")]<-"Soil, Soil Respiration"
  alldata$NALT.Keywords[alldata$Category%in%c("Soil/Plant")]<-"Soil, Vegetation, Soil Respiration, Greenhouse Gas Emissions"
  alldata$NALT.Keywords[alldata$Category%in%c("Plant", "PLANT FRACTION", "Vegetation", "VEGETATION", "Primary Production", "Vegetation Composition")]<-"Vegetation"
  alldata$NALT.Keywords[alldata$Category%in%c("BIOMASS CARBOHYDRATE ANALYSIS", "BIOMASS MINERAL ANALYSIS", "BIOMASS ENERGY")]<-"Vegetation, Biomass"
  alldata$NALT.Keywords[alldata$Category%in%c("Crop", "CROP")]<-"Vegetation, Crop"
  alldata$NALT.Keywords[alldata$Category%in%c("HARVEST REMOVAL")]<-"Vegetation, Crop, Harvesting"
  alldata$NALT.Keywords[alldata$Category%in%c("Forage", "GRAZING PLANTS")]<-"Vegetation, Grazing"
  alldata$NALT.Keywords[alldata$Category%in%c("Forage")]<-"Vegetation, Grazing, Biomass"
  alldata$NALT.Keywords[alldata$Category%in%c("Weed")]<-"Vegetation, Invasive Species"
  alldata$NALT.Keywords[alldata$Category%in%c("Water Quality", "WATER QUALITY", "water/soil chemistry")]<-"Water Quality"
  alldata$NALT.Keywords[alldata$Category%in%c("STREAM CROSS-SECTION SURVEYS", "STREAM THALWEG SURVEYS")]<-"Water Quality, Hydrology"
  alldata$NALT.Keywords[alldata$Category%in%c("Lake")]<-"Water Quality, Lake"
  alldata$NALT.Keywords[alldata$Category%in%c("Water Quality") & alldata$Site.VariableName%in%c("pp' -DDD", "pp'DDE", "pp'-DDT")]<-"Water Quality, Pesticides"
  alldata$NALT.Keywords[alldata$Category%in%c("Wind Erosion")]<-"Wind Erosion"
  alldata$NALT.Keywords[alldata$Category%in%c("Wind Erosion") & alldata$Site.VariableName%in%c("time lapse camera")]<-"Water Quality, Phenology"
  
  
  #### 3.4 #unique(alldata$LTARSite.Code)  # need to split out St Paul and Morris as subsites ####
    
  #### 4. Work on attribute names as mapped to AgCROS, by site within a specific category ####
  ## bring in AgCROS value domains ##
#  var.names <- read.delim("input/var.names.txt", header = T, sep = "\t", stringsAsFactors = FALSE, na.strings = "")
#  var.names <- var.names %>%
#    select(TYPE, TABLE, COLUMN, ParameterDescription, Units) %>%
#    filter(COLUMN != "---") %>%
#    arrange(COLUMN) 
#  names(var.names)[3] <-"AgCROS.VarName"
  
  return(alldata)
}

get.AgCROS.domains <- function() {
  var.names <- read.delim("input/var.names.txt", header = T, sep = "\t", stringsAsFactors = FALSE, na.strings = "")
  var.names <- var.names %>%
    select(TYPE, TABLE, COLUMN, ParameterDescription, Units) %>%
    filter(COLUMN != "---") %>%
    arrange(COLUMN) 
  names(var.names)[3] <-"AgCROS.VarName"
  
  return(var.names)
}

write.csv.agcross <- function(df, table.name = NULL) {
  write.path <- "output/agcross-"
  df.out <- df
  
  if(!is.null(table.name)) {
    df.out <- df %>% 
      filter(TABLE == table.name)
    write.path <- paste(write.path, table.name, sep = "")
  }
  else {
    write.path <- paste(write.path, "all", sep = "")
  }
  
  dateToday <- format(Sys.Date(), "%Y%m%d")
  write.path <- paste(write.path, "_", dateToday, ".csv", sep = "")
  write.csv(df.out, file = write.path, na = "", row.names = FALSE)
}

write.csv.inventory <- function(df, site.code = NULL, network.category = NULL) {

  write.path <- "output/inventory-"
  df.out <- df
  
  if(!is.null(site.code) & !is.null(network.category)) {
    df.out <- df %>% 
      filter(LTARSite.Code == site.code,
             Network.Category == network.category)
    write.path <- paste(write.path, site.code, "-", network.category, sep = "")
  }else if(!is.null(site.code) & is.null(network.category)) {
    df.out <- df %>% 
      filter(LTARSite.Code == site.code)
    write.path <- paste(write.path, site.code, sep = "")
  }else if(is.null(site.code) & !is.null(network.category)) {
    df.out <- df %>% 
      filter(Network.Category == network.category)
    write.path <- paste(write.path, network.category, sep = "")
  } else {
    write.path <- paste(write.path, "alldata_", sep = "")
  }
  
  dateToday <- format(Sys.Date(), "%Y%m%d")
  write.path <- paste(write.path, "_", dateToday, ".csv", sep = "")
  write.csv(df.out, file = write.path, na = "", row.names = FALSE)
}

#### 5. Filter by Site and Category ####

#plants.CPER.data <- filter(alldata, alldata$LTARSite.Code == "CPER" & alldata$Network.Category == "Vegetation")


#plants.var.name <- filter(var.names, var.names$TABLE == "MeasGrazingPlants")



#write.table(plants.CPER.data, paste("output/plants.CPER.data_", dateToday, ".txt", sep = ""), col.names = TRUE, sep = "\t")

#write.table(plants.var.name, paste("output/plants.var.name_", dateToday, ".txt", sep = ""), col.names = TRUE, sep = "\t")