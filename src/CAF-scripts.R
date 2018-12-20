source("src/inventory-functions.R")

df <- map.thesauri()
df.agcros <- get.AgCROS.domains()

write.csv.agcross(df.agcros)
write.csv.inventory(df, "CAF")
