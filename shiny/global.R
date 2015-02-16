files <- dir("data/")

housing <-  data.frame()

for (i in files){
    print(paste("data/", i, sep=""))
    rds <- readRDS(paste("data/", i, sep=""))
    housing <- rbind(housing, rds)
}

# Remove all duplicates
dup <- duplicated(housing[,2])
housing <- housing[!dup,]

rm(i, rds, files, dup)



# housing <- readRDS("2015-02-03.RDS")