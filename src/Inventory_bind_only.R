############################
### Script to pull in data inventories from the LTAR NEtwork, bind and anlyze them ONLY

#   Nicole Kaplan, Bryan Carlson, Gerardo Armendariz (11/20/2019)

#get.inventory <- function() {
  library(tidyverse)

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
    
 # }

 #### 2.0 write all site inventory to output ####
    
    write.csv(inventory.1, "output/inventory.1.csv")
  