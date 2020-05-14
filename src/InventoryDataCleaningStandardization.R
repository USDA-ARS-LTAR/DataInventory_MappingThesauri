options(stringsAsFactors=FALSE)

.libPaths("C:/Users/lindsey.messinger/Documents/RPackages") # change (or remove) to indicate where R Packages are saved
setwd("C:/Users/lindsey.messinger/Projects/LTAR_InventoryDashboard/") # change this path to indicate where data is located and new data should be saved

library(tidyverse)
library(lubridate)

InventoryData<-read.csv("df_20200215.csv")

# Standardize various field values where multiple formats exist
InventoryData$Category[InventoryData$Category%in%c("Vegetation", "Vegatation", "VEGETATION", "vegetation" )]<-"Vegetation"
InventoryData$Category[InventoryData$Category%in%c("Soils", "Soil", "soil","Soil" )]<-"Soil"
InventoryData$Category[InventoryData$Category%in%c("biodiversity", "Biodiversity" )]<-"Biodiversity"


# assign network category based on Category in current dataset
InventoryData$Network.Category[InventoryData$Category%in%c("Flux", "METEOROLOGY", "Precipitation", "Ecosystem Flux", "GHGFlux" )]<-"Land-Atmosphere Interactions" 
InventoryData$Network.Category[InventoryData$Category%in%c("Plant", "Phenology", "Vegetation")]<-"Primary Production" 
InventoryData$Network.Category[InventoryData$Category%in%c("Modeling" )]<-"Models"
InventoryData$Network.Category[InventoryData$Category%in%c("Soil" )]<-"Soil"
InventoryData$Network.Category[InventoryData$Network.Category%in%c("Animal" )]<-"Animal Science and Behavior"


# identify and create datasets where field values are missing/NA
no_network_category<-InventoryData[is.na(InventoryData$Network.Category),]
no_common_experiment<-InventoryData[is.na(InventoryData$CommonExperiment),]

# add common domain catergory based on network category in currrent dataset - future supdates will need to make sure these assignments fit the specific data
InventoryData$CommonDomain[InventoryData$Network.Category%in%c("Primary Production", "Animal Science and Behavior")]<-"Productivity"
InventoryData$CommonDomain[InventoryData$Network.Category%in%c("Land-Atmosphere Interactions", "Landscape structure and biodiversity", "Land Management Practices",
                                                               "Soil", "Water Quality", "Gridded and Vector Data", "Eco-Aeolian-Hydro Processes",
                                                               "Manure Management", "Models")]<-"Environment"

no_common_domain<-InventoryData[is.na(InventoryData$CommonDomain),]

# standardize Start and End date formats into new year-only column to aid with visualizations
# Start date standardization to year-only
InventoryData$StartYear <- parse_date_time(InventoryData$StartDateStandard, c("%Y", "%m/%d/%Y"))
InventoryData$StartYear[is.na(InventoryData$StartYear)] <- as.POSIXct(as.Date(as.numeric(InventoryData$StartDateStandard[is.na(InventoryData$StartYear)]),
                                                                              origin = "1899-12-30"))

InventoryData$StartYear<- format(as.Date(InventoryData$StartYear), "%Y")

# end date standardization to year-only
currentyear<-format(Sys.Date(), "%Y")
InventoryData<-InventoryData%>%
  mutate(EndDateStandard=ifelse(EndDateStandard%in%c("now", "Present"), currentyear, EndDateStandard)) # code on-going projects to "end" in current year

InventoryData$EndYear <- parse_date_time(InventoryData$EndDateStandard, c("%Y", "%m/%d/%Y"))
InventoryData$EndYear[is.na(InventoryData$EndYear)] <- as.POSIXct(as.Date(as.numeric(InventoryData$EndDateStandard[is.na(InventoryData$EndYear)]),
                                                                          origin = "1899-12-30"))

InventoryData$EndYear<- format(as.Date(InventoryData$EndYear), "%Y")

InventoryData<-InventoryData%>%
  mutate(CZO.Term=ifelse(CZO.Term=="", "no CZO Term indicated", CZO.Term))%>% # provide clairification as to why no CZO term is indicated
  mutate(SpatialRepeats=ifelse(SpatialRepeats%in%c("variable", "Variable"), 1, as.numeric(SpatialRepeats)))%>% # convert "variable" value to 1 (likely conservative but no better data to use and we know there was at least 1)
  mutate(SpatialRepeats=ifelse(SpatialRepeats=="1, 4", 4, SpatialRepeats))%>% # assign spatial repeats to plot, not field level resolution for these entries
  mutate(SpatialRepeats=ifelse(is.na(SpatialRepeats), 1, SpatialRepeats))%>% # assign NA values to 1 (likely conservative but no better data to use and we know there was at least 1)
  separate(CZO.Term, into = c("CZO.Term", "CZO_check"), sep = "_")%>% # places "_check" into seperate column and seperates from CZO term for cleaner display
  select(-CZO_check)


InventoryDataRaw<-InventoryData%>%
  select(LTARSiteCode, StartYear, EndYear, StartDateStandard, EndDate, EndDateStandard, Network.Category, CZO.Term, CommonExperiment, 
         CommonDomain, SpatialRepeats, Category, TemporalResolution, TemporalResolutionStandard, PublicAccess, AgCROS.VarName, DataSetVariableName, AttributeDescription, Units,
         StorageFormat) # reduce dataset to key columns for easier manipulation

write.csv(InventoryDataRaw, "InventoryDataRaw.csv")

InventoryDataRawMap<-InventoryData
write.csv(InventoryDataRawMap, "InventoryDataRawMap.csv") # raw dataset attached to spatial layer for display in web map on AgCROS


InventoryData<-InventoryData%>%
  select(-StartDate, -StartDateStandard, -EndDate, -EndDateStandard)%>% # remove variables no longer needed for visualization
  group_by(LTARSiteCode, CommonDomain, Network.Category, CZO.Term, StartYear, EndYear, CommonExperiment,AgCROS.VarName)%>% # group by these variables to maintain in dataframe
  summarise(TotalSpatialRepeats=sum(SpatialRepeats))%>% # summarise data to represent unique variables measured and total spatial repeats
  separate(CZO.Term, into = c("CZO.Term", "CZO_check"), sep = "_")%>% # places "_check" into seperate column and seperates from CZO term for cleaner display
  select(-CZO_check)

write.csv(InventoryData, "InventoryDataSub.csv")


InventoryDataCZO<-InventoryData%>%
  select(-StartYear, -EndYear, -CommonExperiment, -AgCROS.VarName)%>%
  group_by(LTARSiteCode, CommonDomain, Network.Category, CZO.Term)%>%
  summarise(TotalSpatialRepeats=sum(TotalSpatialRepeats))

write.csv(InventoryDataCZO, "InventoryDataCZO.csv") # this will save into the folder designated as the working directory - this is a dataset uploaded to Insights


InventoryDataUniqueCZO<-InventoryDataCZO%>%
  group_by(LTARSiteCode, CommonDomain, Network.Category, CZO.Term)%>%
  distinct(CZO.Term)

InventoryDataUniqueCZO<-InventoryDataUniqueCZO[InventoryDataUniqueCZO$CZO.Term!="no CZO Term indicated",]

InventoryDataUniqueCZO<-InventoryDataUniqueCZO[!is.na(InventoryDataUniqueCZO$LTARSiteCode),]

write.csv(InventoryDataUniqueCZO, "InventoryDataUniqueCZO.csv") # this will save into the folder designated as the working directory - this is a dataset uploaded to Insights

# Temporal data for visualization
 
has_start_year<-InventoryData[!is.na(InventoryData$StartYear),]
TemporalInventoryData<-has_start_year[!is.na(has_start_year$EndYear),]
TemporalInventoryData$StartYear<-as.numeric(TemporalInventoryData$StartYear)
TemporalInventoryData$EndYear<-as.numeric(TemporalInventoryData$EndYear)

TemporalInventoryData<-TemporalInventoryData%>%
  mutate(Years_Between = (EndYear - StartYear) + 1)

TemporalInventoryData<-TemporalInventoryData[rep(seq(nrow(TemporalInventoryData)), as.numeric(TemporalInventoryData$Years_Between)),] %>%
  group_by(LTARSiteCode, CommonDomain, Network.Category, CZO.Term, StartYear, EndYear, CommonExperiment, AgCROS.VarName) %>%
  mutate(Current_Year = StartYear + row_number() - 1)%>%
  select(-Years_Between)

write.csv(TemporalInventoryData, "InventoryDataTemporal.csv") # this will save into the folder designated as the working directory - this is the dataset uploaded to Insights