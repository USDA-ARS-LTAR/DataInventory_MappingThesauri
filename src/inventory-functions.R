############################
### Script to pull in data inventories from the LTAR NEtwork, bind and anlyze them
### Assign Network categories and keywords, as well as attributes to AgCROS tables
### Next version shall assign only one category, yet allow multiple terms comma-delimited to be listed under keywords
#   Nicole Kaplan, Bryan Carlson, Gerardo Armendariz (4/9/2019)


#' Bind inventories
#' 
#' Reads all tab delim text files in the "input/csv_inventories" directory and 
#' merges them into a single DataFrame.
#' 
#' @return A dplyr dataframe containing inventory information from all LTAR sites
#' @seealso [get.inventory()] which calls this function and does further 
#' processing
#' @author Nicole Kaplan
bind.inventories <- function() {
  require(tidyverse)
  
  #### 1. appending all of the inventories into one ####
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
    inventory.1 <- filter(inventory.1, inventory.1$LTARSite.Code != "")
  }
  
  return(inventory.1)
}

#' Get inventory
#' 
#' Reads all tab delim text files in the "input/csv_inventories" directory and 
#' merges them into a single DataFrame, then adds additional columns.
#' 
#' @return A dplyr dataframe containing inventory information from all LTAR
#' sites along with additional columns
#' @seealso [bind.inventories()] which this function calls  
#' @author Nicole Kaplan
get.inventory <- function() {
  library(tidyverse)

  #### 1. appending all of the inventories into one ####
  inventory.1 <- bind.inventories()
  
  #### 2. Adding additional columns to further categorize data (still as general typology vs attribute definition - next step) ####
  
  inventory.1$Network.Category <- ""  #Assigned one in section 3.1
  inventory.1$AgCROS.Table <-"NA"       #Assigned in section 3.2 from Bruce's mapping
  inventory.1$AgCROS.VarName <- ""    # To be assigned by site and work through individual categories first, perhaps as google sheet with lookup
  inventory.1$NALT.Keywords <- ""     #3.3
  inventory.1$subsite <- ""           #Needs to be addessed by each site, see MN as an example 3.4
  inventory.1$Repo.Name <- "" #(eg AmeriFlux, ) #Assigned in section 3.5
  inventory.1$Model.Name <- ""  #3.6 Ask Bryan if there is a list of models that may have included simulated data to be included as a lookup
  
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
  alldata$Network.Category[alldata$Category%in%c("Water Quality", "WATER QUALITY", "water/soil chemistry", "water quality")]<-"Water Quality"
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
  alldata$AgCROS.Table[alldata$Network.Category%in%c("Sediments")] <-" MeasSoilCover, MeasSoilChem, MeasSoilPhys, MeasSoilBiol"
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
  return(alldata)
}

############################
#### 4. Filter AgCROS variable names for mapping from network to AgCROS ####
## bring in AgCROS value domains ##

get.AgCROS.domains <- function() {
  var.names <- read.delim("input/var.names.txt", header = T, sep = "\t", stringsAsFactors = FALSE, na.strings = "", check.names = FALSE)
  
  var.names <- select(var.names, TYPE, TABLE, 'Parameter Description Name (long)','Parameter Description Name (short)', 'Units (short)', TYPE) 
  
  names(var.names)[3] <-"Variable Description"
  names(var.names)[4] <-"AgCROS.VarName"
  names(var.names)[5] <-"AgCROS.Unit"
  
  var.names <- var.names %>%
    filter(TYPE %in% c("Measurement", "Management", "Characterization")) %>%
    filter(!str_detect(AgCROS.VarName, paste(c("STD")))) %>%
    na.omit(var.names$AgCROS.Unit)
  
  
  return(var.names)
}

#### 4.1 Filter the AgCROS variable associated with various tables to create systematic approach to mapping ####

filter.agcros.variables <- function(){
  var.names.soil <- var.names %>% filter(TABLE %in% c("MeasSoilCover", "MeasSoilChem", "MeasSoilPhys", "MeasSoilBiol"))  
  var.names.ghg <- var.names %>% filter(TABLE %in% c("MeasGHGFlux"))
  var.names.plants <- var.names %>% filter(TABLE %in% c("MeasGrazingPlants", "MeasResidueMgt", "MeasHarvestFraction"))
  var.names.wind <- var.names %>% filter(TABLE %in% c("MeasWindErosionArea", "MeasWindErosionConc"))
  var.names.mgmt <- var.names %>%
    filter(TABLE %in% c("MgtAmendments", "MgtPlanting", "MgtTillage", "MgtGrowthStages", "MgtResidue", "MgtGrazing"))
  var.names.weather <- var.names %>%
    filter(TABLE %in% c("WeatherDaily", "WeatherStation"))
  var.names.stewards.waterQual <- var.names %>%
    filter(TABLE %in% c("STEWARDS Water Quality table that will be added to AgCROS"))
  var.names.stewards.waterDisc <- var.names %>%
    filter(TABLE %in% c("STEWARDS Water Quality, Discharge, and Runoff tables that will be added to DET"))
}

#### 4.1.1 Output the AgCROS variable associated with various tables to create systematic approach to mapping ####

write.filtered.agcros.variables <- function(){
  write.table(var.names.soil, "output/var.names.soil.txt", sep = "\t") 
  write.table(var.names.ghg, "output/var.names.ghg.txt", sep = "\t") 
  write.table(var.names.plants, "output/var.names.plants.txt", sep = "\t") 
  write.table(var.names.wind, "output/var.names.wind.txt", sep = "\t") 
  write.table(var.names.mgmt, "output/var.names.mgmt.txt", sep = "\t") 
  write.table(var.names.weather, "output/var.names.weather.txt", sep = "\t") 
  write.table(var.names.stewards.waterQual, "output/var.names.stewards.waterQual.txt", sep = "\t") 
  write.table(var.names.stewards.waterDisc, "output/var.names.stewards.waterDisc.txt", sep = "\t") 
}

#### 4.2 Write the AgCROS variable names to a CSV file ####

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
  write.path <- paste(write.path, "_", dateToday, ".csv", sep = "\t")
  write.csv(df.out, file = write.path, na = "", row.names = FALSE)
}


#### 4.3 Write the full inventory to a CSV ####

#' Write inventory to CSV
#' 
#' Optionally filters dataframe then writes dataframe to a comma delim file in
#' the output directory with current date appended to name
#' 
#' @param df A dplyr::data_frame to be written as CSV
#' @param site.code A string LTAR site code, will be used to filter df
#' @param network.category A string network category, will be used to filter df
#' @return NA
#' @author Bryan Carlson
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
    write.path <- paste(write.path, "alldata", sep = "")
  }
  
  dateToday <- format(Sys.Date(), "%Y%m%d")
  write.path <- paste(write.path, "_", dateToday, ".csv", sep = "")
  write.csv(df.out, file = write.path, na = "")
}


############################
### 5. Get data from Google to work with in R ####
##  Purpose: this function will query Google Sheets for the complete LTAR inventory and build a data frame
#   Code added by Gerardo
#          In order for this code to work, the Google Sheet has to be copied to your Google account.

get.inventory.from.GoolgeSheets <- function(sheetname){
  library(googlesheets)
  
  # list all of the users Google Sheets
  my_sheets <- gs_ls()
  
  # read Google Sheet and create data frame
  GS <- gs_title("LTAR_Inventory_alldata_2019")
  
  # list worksheets
  gs_ws_ls(GS)
  
  alldata.GoogleSheets <- gs_read(ss=GS, ws = sheetname, skip=0)
  
  return(alldata.GoogleSheets)
}

#### 5.1 Update Network Category, add NAL subject, and add ISO topic ####
## Added as next step by Nicole to re-assign categories per discussions with NAL, AgCROS and project members ##

update.inventory.with.NALSubject.and.ISOTopic <- function(df) {
  
#### 5.1.1 Update Meteorology category to include evapotranspiration ###
  df$Network.Category[df$Category%in%c("Evapotranspiration")]<-"Meteorology"
  
#### 5.1.2 Add NALT Subject area mapped from network categories ####
  
  df$NALT.Subject<-""
  
  df$NALT.Subject[df$Network.Category%in%c("Animals")]<-"Animal Science and Animal Products"
  df$NALT.Subject[df$Network.Category%in%c("Biodiversity")]<-"Natural Resources, Earth and Environmental Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Eddy Covariance")]<-"Research, Technology and Engineering"
  df$NALT.Subject[df$Network.Category%in%c("Geospatial Data")]<-"Research, Technology and Engineering"
  df$NALT.Subject[df$Network.Category%in%c("Greenhouse Gas Emissions")]<-"Physical and Chemical Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Hydrology")]<-"Natural Resources, Earth and Environmental Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Land Management")]<-"Farms and Farming Systems"
  df$NALT.Subject[df$Network.Category%in%c("Manure")]<-"Farms and Farming Systems"
  df$NALT.Subject[df$Network.Category%in%c("Meteorology")]<-"Natural Resources, Earth and Environmental Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Models")]<-"Research, Technology and Engineering"
  df$NALT.Subject[df$Network.Category%in%c("Phenology")]<-"Natural Resources, Earth and Environmental Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Remote Sensing")]<-"Research, Technology and Engineering"
  df$NALT.Subject[df$Network.Category%in%c("Sediments")]<-"Natural Resources, Earth and Environmental Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Soil")]<-"Natural Resources, Earth and Environmental Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Soil Plant Interactions")]<-"Natural Resources, Earth and Environmental Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Vegetation")]<-"Natural Resources, Earth and Environmental Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Water Quality")]<-"Natural Resources, Earth and Environmental Sciences"
  df$NALT.Subject[df$Network.Category%in%c("Wind Erosion")]<-"Natural Resources, Earth and Environmental Sciences"
  
####5.1.3 Add ISO topics mapped from network categories ####
  df$ISO.Topic<-""
  
  df$ISO.Topic[df$Network.Category%in%c("Animals")]<-"farming"
  df$ISO.Topic[df$Network.Category%in%c("Biodiversity")]<-"climatologyMeteorologyAtmosphere"
  df$ISO.Topic[df$Network.Category%in%c("Eddy Covariance")]<-"climatologyMeteorologyAtmosphere"
  df$ISO.Topic[df$Network.Category%in%c("Geospatial Data")]<-"planningCadastre"
  df$ISO.Topic[df$Network.Category%in%c("Greenhouse Gas Emissions")]<-"climatologyMeteorologyAtmosphere"
  df$ISO.Topic[df$Network.Category%in%c("Hydrology")]<-"inlandWaters"
  df$ISO.Topic[df$Network.Category%in%c("Land Management")]<-"environment"
  df$ISO.Topic[df$Network.Category%in%c("Manure")]<-"farming"
  df$ISO.Topic[df$Network.Category%in%c("Meteorology")]<-"climatologyMeteorologyAtmosphere"
  df$ISO.Topic[df$Network.Category%in%c("Models")]<-""
  df$ISO.Topic[df$Network.Category%in%c("Phenology")]<-"biota"
  df$ISO.Topic[df$Network.Category%in%c("Remote Sensing")]<-"imageryBaseMapsEarthCover"
  df$ISO.Topic[df$Network.Category%in%c("Sediments")]<-"geoscientificInformation"
  df$ISO.Topic[df$Network.Category%in%c("Soil")]<-"geoscientificInformation"
  df$ISO.Topic[df$Network.Category%in%c("Soil Plant Interactions")]<-"biota"
  df$ISO.Topic[df$Network.Category%in%c("Vegetation")]<-"biota"
  df$ISO.Topic[df$Network.Category%in%c("Water Quality")]<-"inlandWaters"
  df$ISO.Topic[df$Network.Category%in%c("Wind Erosion")]<-"environment"
  
  return(df)
}


############################
#### 6.0 saving revised data inventory back out to Google sheet workbook ####

upload.inventory.to.googlesheets <- function(df) {
  require(googlesheets)
  suppressMessages(library(dplyr))
  #first get the existing spreadsheet
  LTAR_Inventory_alldata_2019 <- gs_title("LTAR_Inventory_alldata_2019")  
  #Then add the new worksheet to the existing sheet 
  LTAR_Inventory_alldata_2019 %>% gs_ws_new(ws_title = "alldata.GoogleSheets_NUEVO"  #make sure it doesn't exist already
            , row_extent = 5000
            , col_extent = 24
            , input = df #data.frame or data.table
            , trim = FALSE  #optional if you want your worksheet trimed
            , verbose = TRUE
  )
}


############################
#### 7. ?? More development for later .... Filter by Site and Category for site to work with data later ??####

#plants.CPER.data <- filter(alldata, alldata$LTARSite.Code == "CPER" & alldata$Network.Category == "Vegetation")

#plants.var.name <- filter(var.names, var.names$TABLE == "MeasGrazingPlants")

#write.table(plants.CPER.data, paste("output/plants.CPER.data_", dateToday, ".txt", sep = ""), col.names = TRUE, sep = "\t")

#write.table(plants.var.name, paste("output/plants.var.name_", dateToday, ".txt", sep = ""), col.names = TRUE, sep = "\t")