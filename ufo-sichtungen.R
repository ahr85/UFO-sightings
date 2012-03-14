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
# reorder
#reorder <- sort(with(data_2

## Amerika-Karte laden
usa_map <- getData('GADM', country = 'US', level = 1)
usa_map_2 <- fortify(usa_map, region = "NAME_1")
gpclibPermit()
usa_map_2 <- fortify(usa_map, region = "NAME_1")

# Ausschnitt
usa_map_3 <- usa_map_2[ which(usa_map_2$long > -130 & usa_map_2$long < 0), ]


## plotting

# different forms
ggplot(data_2, aes(factor(form_neu, levels = names(form_neu)))) + geom_bar() + opts(title = "Different UFO forms")
	
# different years
ggplot(data_2, aes(date)) + geom_bar(binwidth = 0.1) + scale_x_date(format = "%Y") 

# plot with form in different colours
p <- ggplot(data_2, aes(y = lat, x = long, colour = form_neu)) + geom_point(alpha = 0.3, shape = 17)
p <- p + theme_bw() + scale_x_continuous(limits = c(-125,-65)) + scale_y_continuous(limits = c(25, 50))
p <- p + opts(title = paste("UFO-Sigthings in ", i, sep = ""), axis.text.x = theme_blank(), axis.text.y = theme_blank(), panel.grid.major = theme_blank(),
		panel.grid.minor = theme_blank(), axis.ticks = theme_blank())
p <- p + scale_colour_discrete(name = "Forms")
p

# plot with size
p <- ggplot(data_2, aes(y = lat, x = long)) + geom_point(alpha = 0, shape = 17)
p <- p + theme_bw() + scale_x_continuous(limits = c(-125,-65)) + scale_y_continuous(limits = c(25, 50))
p <- p + opts(title = paste("UFO-Sigthings in ", i, sep = ""), axis.text.x = theme_blank(), axis.text.y = theme_blank(), panel.grid.major = theme_blank(),
		panel.grid.minor = theme_blank(), axis.ticks = theme_blank())
#p <- p + stat_density2d(aes(fill = ..level..), geom = "polygon")
p <- p + stat_density2d(aes(fill = ..density..), geom = "tile", contour = FALSE) + scale_fill_gradient(limits=c(0.0001,0.006)) 
p


# facet wrap for each form
p2 <- ggplot(data_2, aes(y = lat, x = long)) + geom_point(alpha = 0.3) + facet_wrap(~ form_neu)
p2

# plotting with Map
p <- ggplot(data_2, aes(y = lat, x = long, colour = form_neu))
p <- p + geom_path(data = usa_map_3, aes(x = long, y = lat, group = group), colour = "#8D8D8D", linetype = 1)
p <- p + coord_fixed() + geom_point(alpha = 0.3)
p



## Plotting for Blog

# different forms
pdf("grafiken/ufo_forms.pdf")
#png("grafiken/ufo_forms.png", width = 500, height = 500, units = "px")
print(ggplot(data_2, aes(factor(form_neu, levels = names(test)))) + geom_bar()
				+ opts(title = "Different UFO forms", plot.title=theme_text(size=16), axis.text.x = theme_text(size = 11,angle = 90), axis.title.x = theme_blank()))
dev.off()

# different years
pdf("grafiken/ufo_years.pdf")
	print(ggplot(data_2, aes(date)) + geom_bar(binwidth = 365) + scale_x_date(format = "%Y", major = "5 year", limits = c(as.Date("1900-01-01"), as.Date("2011-1-1")))# + theme_bw()
		+ opts(title = "UFO sightings by year", plot.title=theme_text(size=16), axis.text.x = theme_text(size = 11,angle = 90), axis.title.x = theme_blank()))	
dev.off()


