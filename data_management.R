# libraries
library(ggplot2)
library(raster)
library(zoo)

## load UFO-data
ufos <- read.delim("data//ufo_awesome.csv", sep = ";", header = F, na.strings = "", stringsAsFactors = F)
ufos_2 <- cbind(ufos[, c(1,2,4,5)], substr(ufos$V3, 2, 1000))
names(ufos_2) <- c("date1", "date2", "form", "dauer", "ort")


## load US-Coordinates
US_coord <- read.delim("data/US.txt", header=F, quote="", sep = "\t", na.strings = "", stringsAsFactors = F)
US_coord_2 <- US_coord[, c(3,5,9,10)]
US_coord_2 <- cbind(paste(US_coord_2[,1], US_coord_2[,2], sep = ", "), US_coord_2[, c(3,4)], stringsAsFactors = F)
names(US_coord_2) <- c("ort", "lat", "long")
temp <- US_coord_2[!duplicated(US_coord_2[,c(1)]),]

## merge ufo seeings and coordinates
data <- merge(temp, ufos_2, by.x = 'ort', by.y = 'ort')

# sightings from year 1900 on and without Alaska and Hawaii
data_2 <- data[ which( data$date1 > "19000000" & data$long > -130 ), ]
data_2 <- cbind(data_2, as.Date(as.factor(data_2$date1), "%Y%m%d"), as.factor(data_2$form))
names(data_2) <- c("ort", "lat", "long", "date1", "date2", "form", "dauer", "date", "form_neu")
# levels with few observations --> "other"
levels(data_2$form_neu)[c(1,6,7,8,10,15,18,23)] <- " other"
# NA-levels --> " unknown"
data_2$form_neu[is.na(data_2$form_neu)] <- c(" unknown")

## get US map
usa_map <- getData('GADM', country = 'USA', level = 1)
usa_map_2 <- fortify(usa_map, region = "NAME_1")
gpclibPermit()
usa_map_2 <- fortify(usa_map, region = "NAME_1")

# without Alaska and Hawaii
usa_map_3 <- usa_map_2[ which(usa_map_2$long > -130 & usa_map_2$long < 0), ]

# renaming
sightings <- data_2
map <- usa_map_3

# saving
save(sightings, map, file = "data/ufo_data.RData")
