source("src/inventory-functions.R")

df <- get.inventory()
df.agcros <- get.AgCROS.domains()

write.csv.agcross(df.agcros)
write.csv.inventory(df, "CAF")
paste("weeee")