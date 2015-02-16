#############
# Read data #
#############

library('jsonlite')
file_name <- "20150212"
json <- fromJSON(paste("raw_data/", file_name, ".json", sep=""))


####################################
# Extract variables from JSON file #
####################################

# ID
housing_data <- data.frame("id" = unlist(json$results$scrap_results$price$href), stringsAsFactors = FALSE)

# Price
housing_data <- cbind(housing_data, "price" = unlist(json$results$scrap_results$price$text), stringsAsFactors = FALSE)

# Size
housing_data <- cbind(housing_data, "size" = unlist(json$results$scrap_results$size$text), stringsAsFactors = FALSE)

# Street
housing_data <- cbind(housing_data, "address" = json$results$scrap_results$address$text, stringsAsFactors = FALSE)

# Description
housing_data <- cbind(housing_data, "description"= json$results$scrap_results$description$text, stringsAsFactors = FALSE)

# Type
housing_data <- cbind(housing_data, "type"= json$results$scrap_results$type$text, stringsAsFactors = FALSE)

# Agent
housing_data <- cbind(housing_data, "agent"= json$results$scrap_results$agent$alt, stringsAsFactors = FALSE)

# Date
housing_data <- cbind(housing_data, "date"= json$thisversionrun, stringsAsFactors = FALSE)

# Convert unicode to ASCII
# http://www.r-bloggers.com/unicode-in-r-packages-not/
for (i in 1:ncol(housing_data)){
  housing_data[,i] <- iconv(housing_data[,i], from="UTF-8", to='ASCII//TRANSLIT')
}

rm(i, json)


####################
# CLEAN DATA FRAME #
####################

clean_data <- data.frame(1:nrow(housing_data))


########
# DATE #
########

# Date the data was scraped
clean_data$date <- strptime(housing_data$date, "%a %b %d %Y %H:%M:%S ")


############
# CLEAN ID #
############

clean_data$id <- sapply(strsplit(housing_data$id, split = "\\?entry"), "[", 1)
clean_data$id <- as.numeric(gsub("[^0-9]", "", clean_data$id))


##############################
# DELETE DUPLICATE ID VALUES #
##############################

dup <- duplicated(clean_data[,3])
clean_data <- clean_data[!dup,]
housing_data <- housing_data[!dup,]

rm(dup)


###############
# CLEAN PRICE #
###############

# NEW VARIABLE
# Rental is paid monthly, weekly or differenlty
clean_data$payment_period <- ifelse(grepl(" EUR/kk", housing_data$price) == TRUE,
                       "month", ifelse(grepl(" EUR/vk", housing_data$price) == TRUE,
                                       "week", "other"))

table(clean_data$payment_period) # Check-up

clean_data$price <- gsub(" EUR/kk| EUR/vk", "", housing_data$price) # Delete non numeric characters
clean_data$price <- gsub(",.*| ", "", clean_data$price) # Delete decimal places
# Convert to numeric, it might force NA on rentals paid for weekly
clean_data$price <- as.numeric(clean_data$price)

# Remove the initial redundant column
clean_data <- clean_data[,-1]


##############
# CLEAN SIZE #
##############

clean_data$size <- gsub("m.*$", "", housing_data$size)
clean_data$size <- gsub(",", ".", clean_data$size)
clean_data$size <- as.numeric(clean_data$size)


##########################
# PRICE PER SQUARE METER #
##########################

clean_data$price_sqm <- clean_data$price / clean_data$size


################
# CLEAN STREET #
################

# Split the string by "\n,"
# Some of the addresses do not include the area so instead of three components will display only two
# Causing an NA in street number. Transfer the street from area and enter NA in area instead

library(stringr)
clean_data$city <- sapply(strsplit(housing_data$address, split = "\n,"), "[", 1)
clean_data$city <- str_trim(clean_data$city)

clean_data$area <- sapply(strsplit(housing_data$address, split = "\n,"), "[", 2)
clean_data$area <- str_trim(clean_data$area)

clean_data$street <- sapply(strsplit(housing_data$address, split = "\n,"), "[", 3)
clean_data$street <- str_trim(clean_data$street)

# Fix street info
fix_street <- is.na(clean_data$street)
clean_data$street <- ifelse(fix_street == TRUE, clean_data$area, clean_data$street)

# 'Fix' area info
clean_data$area <- ifelse(fix_street == TRUE, NA, clean_data$area)

rm(fix_street)


#######################
# ADD GPS COORDINATES #
#######################

# Before using the 'geocode' function:
# Fix ä
clean_data$street <- gsub("\\\"a", "a", clean_data$street)
clean_data$area <- gsub("\\\"a", "a", clean_data$area)

# Fix ö
clean_data$street <- gsub("\\\"o", "o", clean_data$street)
clean_data$area <- gsub("\\\"o", "o", clean_data$area)

library(ggmap)

# Google restricts requests to 2500 requests a day for non-business use
geo <- geocode(paste(clean_data$street, clean_data$city, sep=", "),
               output="latlona")

clean_data$lat <- geo$lat
clean_data$lon <- geo$lon


##############
# POTAL CODE #
##############

address <- as.character(geo$address)
address <- gsub("finland", "", address)
address <- gsub("helsinki,|espoo,|vantaa,", "", address)
address <- str_trim(address)

clean_data$po_code <- factor(substring(address, nchar(address)-4, nchar(address)))
rm(address)

# Check-up
table(clean_data$po_code)

# Clean erroneous, non numeric postal codes
x <- grep("[a-z]", clean_data$po_code, ignore.case = TRUE)
clean_data$po_code[x] <- NA
clean_data$po_code <- factor(clean_data$po_code) # Re-Factor

rm(geo)


###########################
# CLEAN REAL-STATE AGENCY #
###########################

clean_data$agent <- housing_data$agent

# Remove Helsinki, Espoo
clean_data$agent <- gsub("Helsinki|Espoo", "", clean_data$agent)

# Ovenia appears under several similar names
clean_data$agent <- ifelse(grepl("Ovenia", clean_data$agent) == TRUE,
                           "Ovenia", clean_data$agent)

# TA-Yhtymä == TA-Asunnot
clean_data$agent <- ifelse(grepl("TA-", clean_data$agent) == TRUE,
                           "TA", clean_data$agent)

# VVO
clean_data$agent <- ifelse(grepl("VVO-", clean_data$agent) == TRUE,
                           "VVO", clean_data$agent)

# SVK
clean_data$agent <- ifelse(grepl("Suomen Vuo", clean_data$agent) == TRUE,
                           "SVK", clean_data$agent)

# SLA
clean_data$agent <- ifelse(grepl("Suomen Laa", clean_data$agent) == TRUE,
                           "SLA", clean_data$agent)

# SATO
clean_data$agent <- ifelse(grepl("SATO", clean_data$agent) == TRUE,
                           "SATO", clean_data$agent)

# Vuokrahuone Vallila
clean_data$agent <- ifelse(grepl("Vuokrahuone Val", clean_data$agent) == TRUE,
                           "Vuokrahuone", clean_data$agent)

# Newsec
clean_data$agent <- ifelse(grepl("Newsec", clean_data$agent) == TRUE,
                           "Newsec", clean_data$agent)

# Forenom
clean_data$agent <- ifelse(grepl("Forenom", clean_data$agent) == TRUE,
                           "Forenom", clean_data$agent)

# City of Helsinki 
clean_data$agent <- ifelse(grepl("Helsingin", clean_data$agent) == TRUE,
                           "City of Helsinki", clean_data$agent)

# Asuntoverstas
clean_data$agent <- ifelse(grepl("Asuntoverstas", clean_data$agent) == TRUE,
                           "Asuntoverstas", clean_data$agent)

# Realia appears under several similar names
clean_data$agent <- ifelse(grepl("Realia", clean_data$agent) == TRUE,
                           "Realia", clean_data$agent)

# Remove Oy, LKV, Ab, AFM
clean_data$agent <- gsub(" Oy| LKV| Ab| AFM", "", clean_data$agent)

# Remove anything after punctuation, commas, parentheses, slashes, pipes...
clean_data$agent <- gsub(",.*|/.*|\\(.*|\\|.*|\\[.*", "", clean_data$agent)

# Trim spaces
clean_data$agent <- gsub("^ | $", "", clean_data$agent)

# Convert empty values to "Other"
clean_data$agent <- ifelse(grepl("^$", clean_data$agent) == TRUE,
                           "Other", clean_data$agent)

# Factor
clean_data$agent <- factor(clean_data$agent)

# Check
table(clean_data$agent)


##################
# Building types #
##################

# Translate building types

# Garage / parking spot
clean_data$type <- ifelse(housing_data$type=="autohallipaikka", "parking", housing_data$type)
clean_data$type <- ifelse(housing_data$type=="autotalli", "parking", clean_data$type)

# Apartment
clean_data$type <- ifelse(housing_data$type=="kerrostalo", "apartment", clean_data$type)
clean_data$type <- ifelse(housing_data$type=="luhtitalo", "apartment", clean_data$type)

# Single house
clean_data$type <- ifelse(housing_data$type=="omakotitalo", "single house", clean_data$type)
clean_data$type <- ifelse(housing_data$type=="erillistalo", "single house", clean_data$type)

clean_data$type <- ifelse(housing_data$type=="paritalo", "semi-detached", clean_data$type)
clean_data$type <- ifelse(housing_data$type=="rivitalo", "terrace", clean_data$type)

table(clean_data$type) # Check-up


#####################
# CLEAN DESCRIPTION #
#####################

# New variable to collect the number of rooms
clean_data$rooms <- NA

# One-room rentals
x <- grep("1 |1[hH]|1 [hH]|1[rR]|1 [rR]|1 [MmHh]|1[MmHh]|1 [Bb]edroom|[Ss]tudio|[STUDIO]", housing_data$description)
clean_data$rooms[x] <- 1

# Two-room rentals
x <- grep("2 |2[hH]|2 [hH]|2[rR]|2 [rR]|2 [MmHh]|2[MmHh]", housing_data$description)
clean_data$rooms[x] <- 2

# Three-room rentals
x <- grep("3 |3[hH]|3 [hH]|3[rR]|3 [rR]|3 [MmHh]|3[MmHh]", housing_data$description)
clean_data$rooms[x] <- 3

# Four-room rentals
x <- grep("4 |4[hH]|4 [hH]|4[rR]|4 [rR]|4+|4 [MmHh]|4[MmHh]", housing_data$description)
clean_data$rooms[x] <- 4

# Five-room rentals
x <- grep("5 |5[hH]|5 [hH]|5[rR]|5 [rR]|5 [MmHh]|5[MmHh]", housing_data$description)
clean_data$rooms[x] <- 5

# Six-room rentals
x <- grep("6 |6[hH]|6 [hH]|6[rR]|6 [rR]|6 [MmHh]|6[MmHh]", housing_data$description)
clean_data$rooms[x] <- 6

# Seven-room rentals
x <- grep("7 |7[hH]|7 [hH]|7[rR]|7 [rR]|7 [MmHh]71[MmHh]", housing_data$description)
clean_data$rooms[x] <- 7

# Eight-room rentals
x <- grep("8 |8[hH]|8 [hH]|8[rR]|8 [rR]|8 [MmHh]|8[MmHh]", housing_data$description)
clean_data$rooms[x] <- 8

# Nine-room rentals
x <- grep("9 |9[hH]|9 [hH]|9[rR]|9 [rR]|9 [MmHh]|9[MmHh]", housing_data$description)
clean_data$rooms[x] <- 9

# Ten-room rentals!!
x <- grep("10 |10[hH]|10 [hH]|10[rR]|10 [rR]|10 [MmHh]|10[MmHh]", housing_data$description)
clean_data$rooms[x] <- 10

# SHARED apartment
# it is important to have it last, otherwise the data gets erased by some of the other variables
x <- grep("Omahuone", housing_data$description)
clean_data$rooms[x] <- 0

# Check-up
table(clean_data$rooms)
x <- which(is.na(clean_data$rooms) == TRUE)
housing_data$description[x]


# BALCONY
clean_data$balcony <- 0

x <- grep("[Pp]arv|pvk", housing_data$description)
clean_data$balcony[x] <- 2

x <- grep("ransk|ransk.[Pp]arv|ransk.p", housing_data$description)
clean_data$balcony[x] <- 1

# SAUNA
clean_data$sauna <- 0

x <- grep("[Ss]auna|+[Ss]$|/s,|+ [Ss] +|+s+", housing_data$description)
clean_data$sauna[x] <- 1

# FURNISHED
clean_data$furnished <- 0

x <- grep("FURN", housing_data$description, ignore.case = TRUE)
clean_data$furnished[x] <- 1

# GARAGE / PARKING SPOT
clean_data$garage <- 0

x <- grep("aut|gar", housing_data$description, ignore.case = TRUE)
clean_data$garage[x] <- 1

# TERRACE
clean_data$terrace <- 0

x <- grep("ter", housing_data$description)
clean_data$terrace[x] <- 1

rm(x)


##########################################
# REMOVE GARAGES AND WEEKLY PAID RENTALS #
##########################################

clean_data <- subset(clean_data, garage==0 & payment_period == "month")


###########################################
# DELETE EXTREME (WRONG) GPS OBSERVATIONS #
###########################################

# Check and remove "outliers"
ggplot(clean_data, aes(lon, lat)) +
    geom_point()

clean_data <- subset(clean_data, lat < 60.4)

# When the geocode function does not recognize the address it assigns the coordinates to the
# most general level, which is Helsinki's center, exactly on the railway station tracks
# remove that range of values
geocode("helsinki")
x <- which(clean_data$lon > 24.94101 & clean_data$lon < 24.94103 &
               clean_data$lat > 60.17331 & clean_data$lat < 60.17333)
clean_data[x, 10:11]
clean_data <- clean_data[-x,]


##############
# DELETE NAS #
##############

clean_data <- na.omit(clean_data)


###################
# SAVE FINAL DATA #
###################

dir_file <- paste("clean_data/", file_name, ".RDS", sep="")
saveRDS(clean_data, dir_file)
rm(dir_file, x, file_name, clean_data, housing_data)