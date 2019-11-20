############################
### Script to pull in data inventories from the LTAR NEtwork, bind and anlyze them
### Assign Network categories and keywords, as well as attributes to AgCROS tables
### Next version shall assign only one category, yet allow multiple terms comma-delimited to be listed under keywords
#   Nicole Kaplan, Bryan Carlson, Gerardo Armendariz (4/9/2019)


############################
### Parts 1, 2, and 3:  Create initial merged inventory from individual site inventories
##  

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
 # }

 #### 2.0 write all site inventory to output ####
    
    write.csv(inventory.1, "output/inventory.1.csv")
  