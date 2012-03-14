setwd("C:\\Dokumente und Einstellungen\\ahschulz\\Eigene Dateien\\My Dropbox\\R\\Ufo-Sichtungen")
setwd("C:\\Users\\Arne\\Dropbox\\R\\Ufo-Sichtungen")

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

## Data manipulation

# select continent (without alaska) and seeings later that 1900
data_2 <- data[ which( data$date1 > "19000000" & data$long > -130 ), ]
data_2 <- cbind(data_2, as.Date(as.factor(data_2$date1), "%Y%m%d"), as.factor(data_2$form))
names(data_2) <- c("ort", "lat", "long", "date1", "date2", "form", "dauer", "date", "form_neu")
# levels with few observations --> "other"
levels(data_2$form_neu)[c(1,6,7,8,10,15,18,23)] <- " other"
# NA-levels --> " unknown"
data_2$form_neu[is.na(data_2$form_neu)] <- c(" unknown")
# reorder

## Amerika-Karte laden
usa_map <- getData('GADM', country = 'US', level = 1)
usa_map_2 <- fortify(usa_map, region = "NAME_1")
gpclibPermit()
usa_map_2 <- fortify(usa_map, region = "NAME_1")

# Karte mit weniger Punkten
usa_map_3 <- usa_map_2[ which(usa_map_2$long > -130 & usa_map_2$long < 0), ]


### Zeitverlauf

timeline_1 <- format(seq(as.Date("1950-01-01"), by = "year", length = 44), "%Y")
timeline_2 <- format(seq(as.Date("1994-01-01"), by = "month", length = 17*12), "%Y-%m")

#timeline <- as.Date("2010-01-01")

pb <- winProgressBar("Fortschritt Jahre", "", min = 1, max = length(timeline_1), 0)
pb2 <- 1

for (i in timeline_1) {
	temp <- data_2[which(as.numeric(format(data_2$date, "%Y")) == i),]
	
	name <- paste("grafiken/film/bild", i, sep = "_")
	
	plot <- ggplot(temp, aes(y = lat, x = long, colour = form_neu))
	plot <- plot + geom_path(data = usa_map_3, aes(x = long, y = lat, group = group), colour = "#8D8D8D", linetype = 1)
	plot <- plot + coord_fixed(ratio = 1.4) + geom_point(alpha = 0.8, shape = 16)
	plot <- plot + theme_bw() + scale_x_continuous(limits = c(-125,-65)) + scale_y_continuous(limits = c(25, 50))
	plot <- plot + opts(title = paste("UFO-Sigthings in ", i, sep = ""), axis.text.x = theme_blank(), axis.text.y = theme_blank(), panel.grid.major = theme_blank(),
			panel.grid.minor = theme_blank(), axis.ticks = theme_blank())
	plot <- plot + scale_colour_discrete(name = "Forms")
	
#	png(paste(name, "png", sep = "."), width = 720, height = 500, units = "px")
#		print(plot)
#	dev.off()

	ggsave(plot, filename = paste(name, "png", sep = "."), width = 7.2, height = 5)
	# update progress bar
	setWinProgressBar(pb, pb2)
	pb2 <- pb2 + 1
}

close(pb)


pb <- winProgressBar("Fortschritt Monate-Jahre", "", min = 1, max = length(timeline_2), 0)
pb2 <- 1

for (i in timeline_2) {
	temp <- data_2[which(as.numeric(format(data_2$date, "%Y-%m")) == i),]
	
	name <- paste("grafiken/film/bild", i, sep = "_")
	
	plot <- ggplot(temp, aes(y = lat, x = long, colour = form_neu))
	plot <- plot + geom_path(data = usa_map_3, aes(x = long, y = lat, group = group), colour = "#8D8D8D", linetype = 1)
	plot <- plot + coord_fixed(ratio = 1.4) + geom_point(alpha = 0.8, shape = 16)
	plot <- plot + scale_x_continuous(limits = c(-125,-65)) + scale_y_continuous(limits = c(25, 50))
	plot <- plot + opts(title = paste("UFO-Sigthings in ", i, sep = ""), axis.text.x = theme_blank(), axis.text.y = theme_blank(), panel.grid.major = theme_blank(),
			panel.grid.minor = theme_blank(), axis.ticks = theme_blank())
	
	png(paste(name, "png", sep = "."), width = 720, height = 500, units = "px")
	print(plot)
	dev.off()
	
	# update progress bar
	setWinProgressBar(pb, pb2)
	pb2 <- pb2 + 1
}

close(pb)