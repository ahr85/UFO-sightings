setwd("C:\\Dokumente und Einstellungen\\ahschulz\\Eigene Dateien\\My Dropbox\\R\\Ufo-Sichtungen")
setwd("C:\\Users\\Arne\\Dropbox\\R\\Ufo-Sichtungen")

# libraries
#library(ggplot2)
#library(raster)
library(zoo)
library(maps)
library(Hmisc)

## load UFO-data
ufos <- read.delim("data//ufo_awesome.csv", sep = ";", header = F, na.strings = "", stringsAsFactors = F)
ufos_2 <- cbind(ufos[, c(1,2,4,5)], substr(ufos$V3, 2, 1000))
names(ufos_2) <- c("date1", "date2", "form", "dauer", "ort")


## load US-Coordinates
#US_coord <- read.csv2("data/US.csv", header=F, quote="", na.strings = "", stringsAsFactors = F)
US_coord <- read.delim("data/US.txt", header=F, quote="", sep = "\t", na.strings = "", stringsAsFactors = F)
#US_coord <- US_coord[,1:3]
#
#  col.names = c("", "", "ort", "", "state", "", "", "", "lat", "long", ""))
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
#data_2 <- na.omit(data_2)
names(data_2) <- c("ort", "lat", "long", "date1", "date2", "form", "dauer", "date", "form_neu")
# levels with few observations --> "other"
levels(data_2$form_neu)[c(1,6,7,8,10,15,18,23)] <- " other"
# NA-levels --> " unknown"
data_2$form_neu[is.na(data_2$form_neu)] <- c(" unknown")

## plotting

# different forms
ggplot(data_2, aes(form_neu)) + geom_bar() + opts(title = "Different UFO forms")
	
# different years
ggplot(data_2, aes(date)) + geom_bar(binwidth = 0.1) + scale_x_date(format = "%Y") 

# plot with form in different colours

map(database="state")
symbols(x =data_2$long, y = data_2$lat, circles=rep(1, length(data_2$long)), inches = 0.025, add = T, bg = rainbow(data_2$form_neu))
legend("bottomright", levels(data_2$form_neu), fill = rainbow(data_2$form_neu), cex = 0.65)


# facet wrap for each form
p2 <- ggplot(data_2, aes(y = lat, x = long)) + geom_point(alpha = 0.3) + facet_wrap(~ form_neu)
p2


## Plotting for Blog

# different forms
png("grafiken/ufo_forms.png", width = 500, height = 500, units = "px")
print(ggplot(data_2, aes(form_neu)) + geom_bar()
				+ opts(title = "Different UFO forms", plot.title=theme_text(size=16), axis.text.x = theme_text(size = 11,angle = 90), axis.title.x = theme_blank()))
dev.off()

# different years
png("grafiken/ufo_years.png", width = 500, height = 500, units = "px")
print(ggplot(data_2, aes(date)) + geom_bar() + scale_x_date(format = "%Y")
				+ opts(title = "UFO seeings by year", plot.title=theme_text(size=16), axis.text.x = theme_text(size = 11,angle = 90), axis.title.x = theme_blank()))
dev.off()



### Zeitverlauf

#as.yearmon(data_2$date, "%Y %m") - 3

timeline <- format(seq(as.Date("1950-01-01"), by = "year", length = 62), "%Y")

#timeline <- as.Date("2010-01-01")

#pb <- txtProgressBar(min = 1, max = length(timeline), style = 3, char = "=")

for (i in timeline) {
	temp <- data_2[which(as.numeric(format(data_2$date, "%Y")) == i),]
	
	name <- paste("grafiken/bild_maps", i, sep = "_")
		
	png(paste(name, "png", sep = "."), width = 720, height = 500, units = "px")
		map(database="state")
		symbols(x = temp$long, y = temp$lat, circles=rep(1, length(temp$long)), inches = 0.025, add = T, bg = rainbow(temp$form_neu))
		legend("bottomright", levels(temp$form_neu), fill = rainbow(temp$form_neu), cex = 0.65)
	dev.off()
}

#close(pb)
