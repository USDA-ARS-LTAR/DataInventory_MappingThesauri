#############
# This script will be used to subset the LTAR inventory data for WGEW
source("src/inventory-functions.R")

#############
# Get original data inventory
df <- get.inventory()
# add an ID column in order to match with Google Sheet
df <- cbind(ID = "", df)






###############
#
# Updating Subject and ISO Topics to KBS and CPER (5/3/2019)
# 
##############

#############
# Get invenotry from Google Sheets
df.GS <- get.inventory.from.GoolgeSheets("alldata_2019 (before Topics/Subjects to KBS/CPER)")

#############
# subset the KBS, and CPER inventory from the original inventory to apply subjects and ISO topics
df.sites <- subset(df.GS,LTARSite.Code == "KBS" | LTARSite.Code == "CPER")

#  dataframe with the NAL Subjects and ISO Topics as new columns
df.GS.updated <- update.inventory.with.NALSubject.and.ISOTopic(df.sites)

# write new CSV file.  This will be uploaded to the Google Sheet
write.csv.inventory(df.GS.updated)
# after creating the CSV for KBS and CPER, I copied and pasted these records to the alldata_2019 sheet







###############
#
# Updating Subject and ISO Topics to all datasets (4/9/2019)
# 
##############

#############
# Get invenotry from Google Sheets  
df.GS <- get.inventory.from.GoolgeSheets("alldata_2019_(4/9/2019)")

#############
# subset the GB inventory from the original inventory
df.site.GB <- subset(df,LTARSite.Code == "GB")

# merge rows from Grate Basin
df.GS.with.site.GB <- rbind(df.GS, df.site.GB)

#  dataframe with the NAL Subjects and ISO Topics as new columns
df.GS.updated <- update.inventory.with.NALSubject.and.ISOTopic(df.GS.with.site.GB)

# reset the ID column
df.GS.updated$ID <- seq.int(nrow(df.GS.updated))

# write new CSV file.  This will be uploaded to the Google Sheet
write.csv.inventory(df.GS.updated)




















#############
# Push inventory to new Google Sheet tab. 
# TODO: This is not currently working. Possibly will be solved in 
#       googlesheets 0.4.0
upload.inventory.to.googlesheets(df.GS.updated)







#############
# Writing CSVs
df.agcros <- get.AgCROS.domains()

write.csv.agcross(df.agcros) 
write.csv.inventory(df)
write.csv.inventory(df, "WGEW")


#############
# Compare Google Sheets vs R data frame

# local dataframe
df %>%
  group_by(LTARSite.Code) %>%
  summarise(n=n())

# remote (Google Sheets) data frame
GSdf %>%
  group_by(LTARSite.Code) %>%
  summarise(n=n())


##########
# Filter for raw datasets
df$LTARSite.Code <- rownames(df)
dplyr::filter(df, grepl('raw', DataSetVariableName))
# 421 with raw work in DataSetVariableName
dplyr::filter(df, grepl('raw', AttributeDescription))
# 39 with raw in AttributeDescription

dplyr::filter(df, grepl('Raw', DataSetVariableName))
