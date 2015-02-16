# Load required libraries
library(shiny); library(ggmap); library(leaflet)

# Read in all files in the data directory
files <- dir("data/")

housing <-  data.frame()

for (i in files){
    print(paste("data/", i, sep=""))
    rds <- readRDS(paste("data/", i, sep=""))
    housing <- rbind(housing, rds)
}

# Check the ID variable and remove duplicate values
dup <- duplicated(housing[,2])
housing <- housing[!dup,]

rm(i, rds, files, dup)