# libraries
library(ggplot2)
#library(zoo)

## load UFO data
load("data/ufo_data.RData")



## plotting

# different forms
ggplot(sightings, aes(factor(form_neu))) + geom_bar() + opts(title = "Different UFO forms")
	
# different years
ggplot(sightings, aes(date)) + geom_bar(binwidth = 0.1) + scale_x_date(format = "%Y") 

# plot with form in different colours
p <- ggplot(sightings, aes(y = lat, x = long, colour = form_neu)) + geom_point(alpha = 0.5, shape = 17)
p <- p + theme_bw() + scale_x_continuous(limits = c(-125,-65)) + scale_y_continuous(limits = c(25, 50))
p <- p + opts(title = "UFO-Sigthings in the USA", axis.text.x = theme_blank(), axis.text.y = theme_blank(), panel.grid.major = theme_blank(),
		panel.grid.minor = theme_blank(), axis.ticks = theme_blank())
p <- p + scale_colour_discrete(name = "Forms") + coord_map()
p <- p + labs(x = NULL, y = NULL)
p


# facet wrap for each form
p2 <- ggplot(sightings, aes(y = lat, x = long)) + geom_point(alpha = 0.3)
p2 <- p2 + coord_map() + opts(axis.x.text = theme_blank(), axis.y.text = theme_blank())
p2 <- p2 + facet_wrap(~ form_neu)
p2

# plotting with Map
p <- ggplot(sightings, aes(y = lat, x = long, colour = form_neu))
p <- p + geom_path(data = map, aes(x = long, y = lat, group = group), colour = "#8D8D8D", linetype = 1)
p <- p + coord_map() + geom_point(alpha = 0.3)
p



## Plotting for Blog

# different forms
pdf("grafiken/ufo_forms.pdf")
#png("grafiken/ufo_forms.png", width = 500, height = 500, units = "px")
print(ggplot(sightings, aes(factor(form_neu, levels = names(test)))) + geom_bar()
				+ opts(title = "Different UFO forms", plot.title=theme_text(size=16), axis.text.x = theme_text(size = 11,angle = 90), axis.title.x = theme_blank()))
dev.off()

# different years
pdf("grafiken/ufo_years.pdf")
	print(ggplot(data_2, aes(date)) + geom_bar(binwidth = 365) + scale_x_date(format = "%Y", major = "5 year", limits = c(as.Date("1900-01-01"), as.Date("2011-1-1")))# + theme_bw()
		+ opts(title = "UFO sightings by year", plot.title=theme_text(size=16), axis.text.x = theme_text(size = 11,angle = 90), axis.title.x = theme_blank()))	
dev.off()


