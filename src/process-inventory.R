############################
### Script to pull in data inventories from the LTAR NEtwork and bind them
#   Nicole Kaplan, Bryan Carlson, Gerardo Armendariz (11/21/2019)

source("src/inventory-functions.R")

df <- bind.inventories() %>% 
  set.id() %>% 
  write.csv.inventory()
