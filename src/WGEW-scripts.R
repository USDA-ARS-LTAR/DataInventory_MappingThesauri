# This script will be used to subset the LTAR inventory data for WGEW
source("src/inventory-functions.R")

df <- get.inventory()

df.GS <- get.inventory.from.GoolgeSheets()

df.agcros <- get.AgCROS.domains()

write.csv.agcross(df.agcros)
write.csv.inventory(df, "WGEW")
