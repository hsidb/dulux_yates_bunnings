

setwd("/Users/hagen/Desktop/Dulux/weatherdata")
library(data.table)
files <- list.files(path = "/Users/hagen/Desktop/Dulux/weatherdata", pattern = ".csv")
temp <- lapply(files, fread, sep = ",")
weatherdata_updated <- rbindlist( temp )


weatherdata_updated_stations = merge(weatherdata_updated, weather_stations, by.x = "V1", by.y = "station_name")
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="Station_Name"] <- "stationname"
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="week"] <- "weeks"
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="Rain"] <- "rain"
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="max_temp"] <- "maxtemperature"
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="min_temp"] <- "mintemperature"
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="max_humidity"] <- "maxhumidity"
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="min_humidity"] <- "minhumidity"
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="wind"] <- "windspeed"
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="solar_rad"] <- "solarradiation"
names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="Evapo-"] <- "evaporation"


weatherdata_updated_stations$date <- as.Date(weatherdata_updated_stations$Date, "%d/%m/%Y")
weatherdata_updated_stations$week = format(weatherdata_updated_stations$date, format = "%Y %W") 
#weatherdata_updated_stations$month = format(weatherdata_updated_stations$date, format = "%Y %M") 

g <- with(weatherdata_updated_stations, aggregate(evaporation, list(stationname = stationname, weeks = weeks), sum, na.rm = TRUE))
names(g)[names(g)=="x"] <- "sum_evapo"

hg <- with(weatherdata_updated_stations, aggregate(rain, list(stationname = stationname, weeks = weeks), sum, na.rm = TRUE))
g = merge(g, hg)
names(g)[names(g)=="x"] <- "sum_rain"


hg <- with(weatherdata_updated_stations, aggregate(maxtemperature, list(stationname = stationname, weeks = weeks), mean, na.rm = TRUE))
g = merge(g, hg)
names(g)[names(g)=="x"] <- "mean_max_temp"

hg <- with(weatherdata_updated_stations, aggregate(mintemperature, list(stationname = stationname, weeks = weeks), mean, na.rm = TRUE))
g = merge(g, hg)
names(g)[names(g)=="x"] <- "mean_min_temp"

hg <- with(weatherdata_updated_stations, aggregate(maxhumidity, list(stationname = stationname, weeks = weeks), mean, na.rm = TRUE))
g = merge(g, hg)
names(g)[names(g)=="x"]<- "mean_max_humid"

hg <- with(weatherdata_updated_stations, aggregate(minhumidity, list(stationname = stationname, weeks = weeks), mean, na.rm = TRUE))
g = merge(g, hg)
names(g)[names(g)=="x"] <- "mean_min_humid"

hg <- with(weatherdata_updated_stations, aggregate(windspeed, list(stationname = stationname, weeks = weeks), mean, na.rm = TRUE))
g = merge(g, hg)
names(g)[names(g)=="x"] <- "mean_wind"

hg <- with(weatherdata_updated_stations, aggregate(solarradiation, list(stationname = stationname, weeks = weeks), mean, na.rm = TRUE))
g = merge(g, hg)
names(g)[names(g)=="x"] <- "mean_solar"

weather_weekly <- g
save(weather_weekly, file = "weather_weekly.RData")
save(weatherdata_updated_stations, file = "weatherdata_updated_stations.RData")

weeks2 <- data.frame(Dates = weatherdata_updated_stations$date, Week = format(weatherdata_updated_stations$date, format = "%Y %W") )
weeks3 <- unique(weeks2)

