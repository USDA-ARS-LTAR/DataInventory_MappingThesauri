source("src/inventory-functions.R")

df <- map.thesauri()

write.csv.inventory(df)
write.csv.inventory(df, "CAF")
write.csv.inventory(df, NULL, "Vegetation")
write.csv.inventory(df, "CAF", "Vegetation")