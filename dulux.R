#psql Yates -U zetaris  -p 20004



write.table(sales_top_states_bunnings19, file = "sales_top_states_bunnings.psv", sep = "|", row.names = FALSE, col.names = TRUE)

library(data.table)
library(RPostgreSQL)
library(reshape2)
library(bit64)
library(foreign)
library(ggplot2)
library(GGally)
library(grid)
library(arm)
library(texreg)
library(fBasics)
library(data.table)
library(caret)
library(MASS)
library(stats)
library(Amelia)
library(pscl)
library(caret)
library(e1071)
library(ROCR)
library(stats)
library(tidyverse)
library(RJSONIO)
library(geosphere)






sapply(sales_top_states_bunnings20,function(x) sum(is.na(x)))
sapply(weatherdata_all, function(x) length(unique(x)))
sales_top_states_bunnings6 <-  sales_top_states_bunnings6[with(sales_top_states_bunnings6, order(start_date, customer, material)),]

keep <- c("VIC", "QLD", "Queensland", "Victoria", "WA", "Western Australia")
sales_top_states <- sales_top_merged[sales_top_merged$State %in% keep,]
sales_hagen2 <- sales_hagen1[sales_hagen1$State %in% keep,]

#top materials
material_id <- c(53849,                        54188,
                        50406,                        40972,
                        41002,                        50818,
                        52557,                        54934,
                        50736,                        54424,
                        34035,                        51787,
                        50735,                        52194,
                        54402,                        53851,
                        53128,                        54324,
                        52289,                        52330,
                        52331,                        51690,
                        53609,                        54443,
                        53933,                        52903,
                        52907,                        50729,
                        43998,                        50730,
                        53850,                        52554,
                        25850,                        55017,
                        50410,                        42806,
                        52085,                        33944,
                        54744,                        39306)

sales_hagen2 <- sales_hagen1[sales_hagen1$material %in% ,]                       
sales_yates_top <- sales_yates[sales_yates$material %in% material_id,]
sales_top_merged <- merge(sales_hagen_merged_top,customers, by.x = "customer",by.y = "Customer", all = TRUE)

#final_data_coke_70 = merge(final_data_coke_70, h, by.x=c("item_id", "shop_id"), by.y=c("item_id", "shop_id"))


setwd("/Users/hagen/Desktop/Dulux/weatherdata")
library(data.table)
files <- list.files(path = "/Users/hagen/Desktop/Dulux/weatherdata", pattern = ".csv")
temp <- lapply(files, fread, sep = ",")
weatherdata_updated <- rbindlist( temp )



weatherdata_all$month <- months(weatherdata_all_hagen$date)
weatherdata_all$month <- factor(weatherdata_all$month, levels= month.name)

sales_top_states_bunnings$Bun_Ind <- as.integer(grepl(pattern = "Bunnings", x = sales_top_states_bunnings$store))

rows_to_keep <- which(sales_top_states_bunnings$Bun_Ind == "1")
sales_top_states_bunnings <- sales_top_states_bunnings[rows_to_keep,]

#geocode weather station location
# weatherdata_all <- read_csv("~/Desktop/Dulux/weatherdata_all.csv")
# 
# geocodeAdddress <- function(address) {
#   require(RJSONIO)
#   url <- "http://maps.google.com/maps/api/geocode/json?address="
#   url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
#   x <- fromJSON(url, simplify = FALSE)
#   if (x$status == "OK") {
#     out <- c(x$results[[1]]$geometry$location$lng,
#              x$results[[1]]$geometry$location$lat)
#   } else {
#     out <- NA
#   }
#   Sys.sleep(0.2)  # API only allows 5 requests per second
#   out
# }
# 
# stations <- unique(weatherdata_all$stationname)
# stations_coord <- lapply(stations, geocodeAdddress)
# stations_column <- do.call(rbind.data.frame, stations)
# stations_coord_column <- do.call(rbind.data.frame, stations_coord)
# weather_stations <- cbind(stations_column,stations_coord_column)
# names(weather_stations)[1]<- "station_name"
# names(weather_stations)[2]<- "longitude"
# names(weather_stations)[3]<- "latitude"

#clean sales data lon/lat info

sales_top_states_bunnings1 <- sales_top_states_bunnings
names(sales_top_states_bunnings1)[names(sales_top_states_bunnings1)=="longitude"] <- "Latitude"
names(sales_top_states_bunnings1)[names(sales_top_states_bunnings1)=="latitude"] <- "Longitude"


sales_top_states_bunnings1$Latitude = gsub(pattern = "-", replacement = "", sales_top_states_bunnings1$Latitude)
sales_top_states_bunnings1$Latitude = gsub(pattern = "^3", replacement = "-3", sales_top_states_bunnings1$Latitude)
sales_top_states_bunnings1$Latitude = gsub(pattern = "^2", replacement = "-2", sales_top_states_bunnings1$Latitude)
sales_top_states_bunnings1$Latitude = gsub(pattern = "^4", replacement = "-4", sales_top_states_bunnings1$Latitude)
sales_top_states_bunnings1$Latitude = gsub(pattern = "^1", replacement = "-1", sales_top_states_bunnings1$Latitude)

sales_top_states_bunnings1$Latitude <- as.numeric(sales_top_states_bunnings1$Latitude)
sales_top_states_bunnings1$Longitude <- as.numeric(sales_top_states_bunnings1$Longitude)
sales_top_states_bunnings1 <- sales_top_states_bunnings1[sales_top_states_bunnings1$Longitude != 0,]

Latitude <- data.frame(unique(sales_top_states_bunnings1$Latitude))
Longitude <- data.frame(unique(sales_top_states_bunnings1$Longitude))
customer <- data.frame(unique(sales_top_states_bunnings2$customer))
store <- data.frame(unique(sales_top_states_bunnings2$store))

list1 <- cbind(Latitude$unique.sales_top_states_bunnings1.Latitude., Longitude$unique.sales_top_states_bunnings1.Longitude.)
list1 <- as.data.frame(list1)
#list2 <- cbind(list1, 
library(geosphere)

names(list1)[names(list1)=="V1"] <- "latitude"
names(list1)[names(list1)=="V2"] <- "longitude"

mat <- distm(list1[,c('longitude','latitude')],  weather_stations[,c('longitude','latitude')], fun=distHaversine)
mat <- round(mat/1000)
x <- mat[, colSums(is.na(mat))  != nrow(mat)]


list1$station_name <- list2a$station_name[max.col(-x)]
# library(dplyr)
 list2a <- weather_stations %>% group_by(station_name) %>% summarise_each(funs(mean)) %>% ungroup()
 mat2 <- distm(list1[,c('longitude','latitude')], list2a[,c('longitude','latitude')], fun=distVincentyEllipsoid)
 list3 <- list1 %>% mutate(locality = list2a$station_name[max.col(-mat2)])
 list1$near_dist <- apply(x, 1, min)

 library(data.table)
 list2a <- setDT(weather_stations)[,lapply(.SD, mean), by=station_name]
 mat3 <- distm(setDT(list1)[,.(longitude,latitude)], list2a[,.(longitude,latitude)], fun=distVincentyEllipsoid)
 list1[, locality2 := list2a$locality[max.col(-mat2)] ]

 weatherdata_updated_stations = merge(weatherdata_updated, weather_stations, by.x = "V1", by.y = "station_name")
 names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="V1"] <- "Station_Name"
 names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="V2"] <- "Date"
 names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="V4"] <- "Rain"
 names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="V6"] <- "max_temp"
 names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="V7"] <- "min_temp"
 names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="Maximum"] <- "max_humidity"
 names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="Minimum"] <- "min_humidity"
 names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="Average"] <- "wind"
 names(weatherdata_updated_stations)[names(weatherdata_updated_stations)=="V11"] <- "solar_rad"
 
 
 
 weatherdata_all_hagen1 = merge(weatherdata_all_hagen, weather_stations, by.x = "stationname", by.y = "station_name")
 yatestores_weather_postcode <- read_csv("~/Desktop/Dulux/yatestores_weather_postcode.csv")
 
 sales_top_states_bunnings2 = merge(sales_top_states_bunnings1, yatestores_weather_postcode[,c("store", "name", "lat", "lon", "distance")], by.x = "store", by.y = "store", all.x = TRUE)
 sales_top_states_bunnings2 <- sales_top_states_bunnings2[!is.na(sales_top_states_bunnings2$distance),]
 #sales_top_states_bunnings2 <- merge(sales_top_states_bunnings2,weatherdata_all)
 
 #prepare weather data
 weatherdata_all <-  weatherdata_all[with(weatherdata_all, order(stationname,date)),]
 
 
 #weatherdata_all1 <- weatherdata_all[!is.na(weatherdata_all$maxtemperature),]
 
 # library(lubridate)
  formatdate <- function(date)
    paste0("Week ", as.numeric(format(date, "%d")) %/% 7 + 1,
           ", ", format(date, "%b %Y"))
 # change "sum" to your required function
  a <- aggregate(weatherdata_all$maxtemperature, by = list(formatdate(weatherdata_all$date)), mean,na.rm = TRUE )
 # 
 # z <- aggregate(maxtemperature~week(date),data = weatherdata_all, mean)
  do.call(rbind, by(weatherdata_all$maxtemperature, week(weatherdata_all$date), summary))
 # weatherdata_all$mean_max_temp <- aggregate(maxtemperature~week(date),data = weatherdata_all, mean)

   
 library(tidyquant)
 df <- weatherdata_all %>%
   tq_transmute(select     = maxtemperature,
                mutate_fun = apply.weekly,
                FUN        = mean, na.rm = TRUE)
 

weatherdata_all1 <- subset(weatherdata_all, date >= "2012-11-05")

weatherdata_updated_stations$date <- as.Date(weatherdata_updated_stations$Date, "%d/%m/%Y")
weatherdata_updated_stations$week = format(weatherdata_updated_stations$date, format = "%Y %W") 
#weatherdata_updated_stations$month = format(weatherdata_updated_stations$date, format = "%Y %M") 



weatherdata_all3 <- weatherdata_all1
weeks2 <- data.frame(Dates = weatherdata_all1$date, Week = format(weatherdata_all1$date, format = "%Y %W") )
weatherdata_all1 <- merge(weatherdata_all1, weeks, by.x = "date", by.y= "Dates", all.x = TRUE)


  Week = format(weatherdata_all1$date, format = "%Y %W") 
  weatherdata_all3$weeks1 <-  Week
  Week = format(sales_top_states_bunnings3$start_date, format = "%Y %W") 
  sales_top_states_bunnings3$weeks1 <-  Week
  
  
names(sales_top_states_bunnings2)[names(sales_top_states_bunnings2)=="name"] <- "weather station"
sales_top_states_bunnings2$distance <- round(sales_top_states_bunnings2$distance)
sales_top_states_bunnings2$ph4 <-NULL
sales_top_states_bunnings2$bunnings_sub_class <- NULL

# library(data.table)
# weatherdata_all2 <- data.table(weatherdata_all1)
# setkey(weatherdata_all2, date)
# 
# 
# weeks2 <- data.table(weeks)
# setkey(weeks2, Dates)
weatherdata_all3$pan <- as.numeric(weatherdata_all3$pan)


g <- with(weatherdata_all3, aggregate(evapo, list(stationname = stationname, weeks1 = weeks1), mean, na.rm = TRUE))
weatherdata_all3 = merge(weatherdata_all3, g)
names(weatherdata_all3)[names(weatherdata_all3)=="x"] <- "mean_evapo"

g <- with(weatherdata_all3, aggregate(rain, list(stationname = stationname, weeks1 = weeks1), sum, na.rm = TRUE))
weatherdata_all3 = merge(weatherdata_all3, g)
names(weatherdata_all3)[names(weatherdata_all3)=="x"] <- "sum_rain"

g <- with(weatherdata_all3, aggregate(pan, list(stationname = stationname, weeks1 = weeks1), mean, na.rm = TRUE))
weatherdata_all3 = merge(weatherdata_all3, g)
names(weatherdata_all3)[names(weatherdata_all3)=="x"] <- "mean_pan"

g <- with(weatherdata_all3, aggregate(maxtemperature, list(stationname = stationname, weeks1 = weeks1), mean, na.rm = TRUE))
weatherdata_all3 = merge(weatherdata_all3, g)
names(weatherdata_all3)[names(weatherdata_all3)=="x"] <- "mean_max_temp"

g <- with(weatherdata_all3, aggregate(mintemperature, list(stationname = stationname, weeks1 = weeks1), mean, na.rm = TRUE))
weatherdata_all3 = merge(weatherdata_all3, g)
names(weatherdata_all3)[names(weatherdata_all3)=="x"] <- "mean_min_temp"

g <- with(weatherdata_all3, aggregate(maxhumidity, list(stationname = stationname, weeks1 = weeks1), mean, na.rm = TRUE))
weatherdata_all3 = merge(weatherdata_all3, g)
names(weatherdata_all3)[names(weatherdata_all3)=="x"] <- "mean_max_humid"

g <- with(weatherdata_all3, aggregate(minhumidity, list(stationname = stationname, weeks1 = weeks1), mean, na.rm = TRUE))
weatherdata_all3 = merge(weatherdata_all3, g)
names(weatherdata_all3)[names(weatherdata_all3)=="x"] <- "mean_min_humid"

g <- with(weatherdata_all3, aggregate(windspeed, list(stationname = stationname, weeks1 = weeks1), mean, na.rm = TRUE))
weatherdata_all3 = merge(weatherdata_all3, g)
names(weatherdata_all3)[names(weatherdata_all3)=="x"] <- "mean_wind"

g <- with(weatherdata_all3, aggregate(solarradiation, list(stationname = stationname, weeks1 = weeks1), mean, na.rm = TRUE))
weatherdata_all3 = merge(weatherdata_all3, g)
names(weatherdata_all3)[names(weatherdata_all3)=="x"] <- "mean_solar"

names(sales_top_states_bunnings3)[names(sales_top_states_bunnings3)=="weather station"] <- "weather_station"

weatherdata_all5 <- weatherdata_all3[c(1:2,14:22)]
weatherdata_all5 <- unique(weatherdata_all5)

#sales_top_states_bunnings3 <- as.data.table(sales_top_states_bunnings2)
#setkey(sales_top_states_bunnings3, weather_station, weeks1)

# weatherdata_all4 <- as.data.table(weatherdata_all3)
# setkey(weatherdata_all4, stationname, weeks1)
# 
# sales_top_states_bunnings4 <- sales_top_states_bunnings3[weatherdata_all4]
# weatherdata_all5 <- weatherdata_all3[c(1:2,14:22)]

sales_top_states_bunnings5 <- merge(sales_top_states_bunnings3,weatherdata_all5, by.x = c("weather_station", "weeks1") ,by.y = c("stationname", "weeks1"), all.x = TRUE)


sales_top_states_bunnings5 <- as.data.frame(sales_top_states_bunnings5)
sales_top_states_bunnings6 <- subset(sales_top_states_bunnings5, start_date <= "2016-07-24")
sales_top_states_bunnings6 <- subset(sales_top_states_bunnings6, sales > 0)

sales_top_states_bunnings6$month <- months(sales_top_states_bunnings6$start_date)
sales_top_states_bunnings6$month <- factor(sales_top_states_bunnings6$month, levels= month.name)




sales_top_states_bunnings6$weather_station <- as.factor(sales_top_states_bunnings6$weather_station)
sales_top_states_bunnings6$weeks1 <- as.factor(sales_top_states_bunnings6$weeks1)
sales_top_states_bunnings6$store <- as.factor(sales_top_states_bunnings6$store)
sales_top_states_bunnings6$customer <- as.factor(sales_top_states_bunnings6$customer)
sales_top_states_bunnings6$product <- as.factor(sales_top_states_bunnings6$product)
sales_top_states_bunnings6$State <- as.factor(sales_top_states_bunnings6$State)
sales_top_states_bunnings6$Territory <- as.factor(sales_top_states_bunnings6$Territory)
sales_top_states_bunnings6$Area <- as.factor(sales_top_states_bunnings6$Area)
sales_top_states_bunnings6$cat2 <- as.factor(sales_top_states_bunnings6$cat2)
sales_top_states_bunnings6$cat3 <- as.factor(sales_top_states_bunnings6$cat3)
sales_top_states_bunnings6$cat4 <- as.factor(sales_top_states_bunnings6$cat4)


table(sales_top_states_bunnings6$State)

# training and test data have differnet amount of unique products/material/stores
sales_top_states_bunnings7 <- sales_top_states_bunnings6[ ! sales_top_states_bunnings6$product %in% c("Yts NW Caterpillar Killer Dipel 40g",
                        "Yts Zero Rapid 1HR RTU 1L AU", 
                        "DL Organic Plant Food 2kg WA##",
                        "NWay Veg & Herb Natrasoap 200ml",
                        "Yts Pathweeder Concentrate 500ml",
                        "ZZZYts ClonexPurpleRootHormone Gel 50ml#"), ]





sales_top_states_bunnings7$closed_Ind <- as.integer(grepl(pattern = "xxCLOSEDxx", x = sales_top_states_bunnings7$store))
sales_top_states_bunnings8 <- sales_top_states_bunnings7[sales_top_states_bunnings7$closed_Ind == '0']


sales_top_states_bunnings9 <- sales_top_states_bunnings8[ ! sales_top_states_bunnings8$store %in%  c("Bunnings Anzac Parade Wodonga 6975", 
                                                                                                    "Bunnings Balcatta 2010", 
                                                                                                    "Bunnings Cranbourne 6201",
                                                                                                     "Bunnings Frankston 6221", 
                                                                                                    "Bunnings Hawthorn 6299", 
                                                                                                    "Bunnings Kalamunda 2094", 
                                                                                                    "Bunnings Mornington 6180", 
                                                                                                    "Bunnings Morwell 6160", 
                                                                                                    "Bunnings Port Melbourne 6042",
                                                                                                    "Bunnings Torquay 6388", 
                                                                                                    "Bunnings Traralgon 6399", 
                                                                                                    "Bunnings Waurn Ponds 6185", 
                                                                                                    "Bunnings Whitfords 2400"), ]
                                                          
                                                          # c("xxCLOSEDxx Bunnings Corio 6164",
                                                          #                                                "xxCLOSEDxx Bunnings Hoppers Crossin",      "xxCLOSEDxx Bunnings Mackay 8446",  "xxCLOSEDxx Bunnings Harbourtown 813",  
                                                          #                                                "xxCLOSEDxx Bunnings Maribyrnong 635",   "xxCLOSEDxx Bunnings Mindarie 2128",    "xxCLOSEDxx Bunnings Sandown 6065", 
                                                          #                                                "xxCLOSEDxx Bunnings South Perth 222",    "xxCLOSEDxx Bunnings Sunbury 6223", "xxCLOSEDxx Bunnings Thomastown 6351", "xxCLOSEDxx Bunnings Joondalup 2078" ), ]


sales_top_states_bunnings9$qty <- as.numeric(sales_top_states_bunnings9$qty)

#Townsville insect killer
sales_top_states_bunnings13 <- sales_top_states_bunnings6[sales_top_states_bunnings6$product %in% c("Yts NW Caterpillar Killer Dipel 40g"),]
sales_top_states_bunnings13 <- sales_top_states_bunnings13[sales_top_states_bunnings11$store %in% c("Bunnings Smithfield 8466"),]
sales_top_states_bunnings13 <- sales_top_states_bunnings13[,c(7,2,25:32)]
plot.ts(sales_top_states_bunnings13)

sales_top_states_bunnings11 <- sales_top_states_bunnings6[sales_top_states_bunnings6$product %in% c("Yts NW Caterpillar Killer Dipel 40g"),]
sales_top_states_bunnings12 <- sales_top_states_bunnings11[sales_top_states_bunnings11$store %in% c("Bunnings Fairfield Waters 8440"),]
#library(reshape2)
sales_top_states_bunnings12 <- sales_top_states_bunnings12[,c(7,8,25:32)]
plot.ts(sales_top_states_bunnings12)


sales_top_states_bunnings11 <- sales_top_states_bunnings6[sales_top_states_bunnings6$product %in% c("Yts NW Caterpillar Killer Dipel 40g"),]
sales_top_states_bunnings12 <- sales_top_states_bunnings11[sales_top_states_bunnings6$store %in% c("Bunnings Townsville 8437"),]
#library(reshape2)
sales_top_states_bunnings12 <- sales_top_states_bunnings12[,c(7,8,25:32)]
plot.ts(sales_top_states_bunnings12)

#df <- data.frame(time = 1:30, sales_top_states_bunnings12)
df <- melt(sales_top_states_bunnings12, id.vars =  'weeks1', variable.name = 'series')

ggplot(df, aes(time,value)) + geom_line(aes(colour = series))

ggplot(df, aes(weeks1,value))+geom_line() + facet_grid(paramter ~ .)


weatherdata_townsvilleaero <- weatherdata_all3[weatherdata_all3$stationname %in% c("TOWNSVILLE AERO"),]   

sales_data_townsville_caterpillar <- sales_top_states_bunnings3[sales_top_states_bunnings3$product %in% c("Yts NW Caterpillar Killer Dipel 40g"),]
sales_data_townsville_caterpillar <- sales_data_townsville_caterpillar[sales_data_townsville_caterpillar$store %in% c("Bunnings Fairfield Waters 8440"),]


library("reshape2")
msales_top_states_bunnings12 <- melt(sales_top_states_bunnings12, id.var = "start_date")
xyplot(sales ~ start_date |)

ggplot(data = sales_top_states_bunnings12, aes(x=start_date, y= sales)
       
 #sales_hagen had missing weeks for sales =0 
   weatherdata_all6 <- merge(weatherdata_all5, yatestores_weather_postcode,by.x = c("stationname") ,by.y = c("name"), all.x = TRUE) 
       
       
  weeks2 <- weeks
  weeks2 <- subset(weeks2, Dates >= "2012-11-05")
  weeks2 <- as.data.frame(unique(weeks2$Week))
  product <- as.data.frame(unique(sales_top_states_bunnings3$product))
  df = expand.grid(store$unique.sales_top_states_bunnings2.store., product$`unique(sales_top_states_bunnings3$product)`, weeks2$`unique(weeks2$Week)`)
  
  sales_top_states_bunnings15 <- merge(sales_top_states_bunnings3,df, by.x = c("store", "product", "weeks1") ,by.y = c("Var1", "Var2" ,"Var3"), all.y =  TRUE)
  sales_top_states_bunnings16 <- merge(sales_top_states_bunnings15,weatherdata_all6, by.x = c("store", "weeks1") ,by.y = c("store", "weeks1"), all.x =  TRUE)
       
       
      
      # training_data_copyv2$display_level[training_data_copyv2$Major_Ind == '1']  <- 3 
       sales_top_states_bunnings16$month <- months(sales_top_states_bunnings16$start_date)
      
       
       sales_top_states_bunnings16$month <- factor(sales_top_states_bunnings16$month, levels= month.name)
   
      sales_top_states_bunnings16$weather_station <- as.factor(sales_top_states_bunnings16$weather_station)
       sales_top_states_bunnings16$weeks1 <- as.factor(sales_top_states_bunnings16$weeks1)
       sales_top_states_bunnings16$store <- as.factor(sales_top_states_bunnings16$store)
       sales_top_states_bunnings16$customer <- as.factor(sales_top_states_bunnings16$customer)
       sales_top_states_bunnings16$product <- as.factor(sales_top_states_bunnings16$product)
       sales_top_states_bunnings16$State <- as.factor(sales_top_states_bunnings16$State)
       sales_top_states_bunnings16$Territory <- as.factor(sales_top_states_bunnings16$Territory)
       sales_top_states_bunnings16$Area <- as.factor(sales_top_states_bunnings16$Area)
       sales_top_states_bunnings16$cat2 <- as.factor(sales_top_states_bunnings16$cat2)
       sales_top_states_bunnings16$cat3 <- as.factor(sales_top_states_bunnings16$cat3)
       sales_top_states_bunnings16$cat4 <- as.factor(sales_top_states_bunnings16$cat4)
       sales_top_states_bunnings16$qty <- as.numeric(sales_top_states_bunnings16$qty)sales_top_states_bunnings16$distance.x <- NULL
sales_top_states_bunnings16$weather_station <- NULL

sales_top_states_bunnings18$customer <- NULL
sales_top_states_bunnings18$State <- NULL
sales_top_states_bunnings18$mean_pan <- NULL
sales_top_states_bunnings18$lat.x <- NULL
sales_top_states_bunnings18$lon.x <- NULL
sales_top_states_bunnings18$store_s <- NULL
sales_top_states_bunnings18$storename <- NULL
sales_top_states_bunnings18$mindist <- NULL
sales_top_states_bunnings18$postcode.y <- NULL
sales_top_states_bunnings16$customer <- NULL
sales_top_states_bunnings16$material <- NULL

sales_top_states_bunnings17 <- sales_top_states_bunnings16 

sales_top_states_bunnings17$sales[is.na(sales_top_states_bunnings17$sales)] <- 0

# sales_top_states_bunnings17 <- as.data.frame(sales_top_states_bunnings17)
#  sales_top_states_bunnings17 <- subset(sales_top_states_bunnings17, start_date <= "2016-07-24")
sales_top_states_bunnings17$sales[sales_top_states_bunnings17$sales < '0'] <- 0      


df <- as.data.frame(weatherdata_all4[,c(2,3)])
df <- df[!duplicated(df$weeks1),]

sales_top_states_bunnings18 <- merge(sales_top_states_bunnings17,df, by = "weeks1")
sales_top_states_bunnings18$month <- months(sales_top_states_bunnings18$date)
sales_top_states_bunnings18$month <- factor(sales_top_states_bunnings18$month, levels= month.name)
#sales_top_states_bunnings18$weeks1 <- as.Date(sales_top_states_bunnings18$weeks1, format = "%Y)


g <- with(sales_top_states_bunnings18, aggregate(sales, list(store = store,product =product, month = month), mean, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings18, g)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "mean_monthly_sales"

sales_top_states_bunnings19$mean_temp <- sales_top_states_bunnings19$mean_max_temp - sales_top_states_bunnings19$mean_min_temp
sales_top_states_bunnings19$mean_humid <- sales_top_states_bunnings19$mean_max_humid - sales_top_states_bunnings19$mean_min_humid

g <- with(sales_top_states_bunnings19, aggregate(sum_rain, list(stationname= stationname, month = month), sum, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings19, g)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "monthly_rain"

g <- with(sales_top_states_bunnings19, aggregate(mean_temp, list(stationname= stationname, month = month), mean, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings19, g)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "mean_monthly_temp"

g <- with(sales_top_states_bunnings19, aggregate(mean_humid, list(stationname= stationname, month = month), mean, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings19, g)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "mean_monthly_humid"

sales_top_states_bunnings18$quarter <- quarters(sales_top_states_bunnings18$date)
sales_top_states_bunnings18$quarter <- factor(sales_top_states_bunnings18$quarter)

g <- with(sales_top_states_bunnings19, aggregate(sales, list(store = store,product =product, quarter =quarter), mean, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings19, g)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "mean_quarterly_sales"

g <- with(sales_top_states_bunnings19, aggregate(mean_rain, list(stationname= stationname, quarter =quarter), mean, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings19, g)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "mean_quarterly_rain"

g <- with(sales_top_states_bunnings19, aggregate(mean_temp, list(stationname= stationname, quarter =quarter), mean, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings19, g)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "mean_quarterly_temp"

sales_top_states_bunnings19$mean_humid <- sales_top_states_bunnings19$mean_max_humid - sales_top_states_bunnings19$mean_min_humid


#sales_top_states_bunnings17$weeks1 <- as.character(sales_top_states_bunnings17)

Bunnings_Store_Areas <- read_excel("~/Desktop/Dulux/Bunnings Store Areas.xlsm",sheet = "Master")

customers$Longitude = gsub(pattern = "-", replacement = "", customers$Longitude)
customers$Longitude = gsub(pattern = "^3", replacement = "-3", customers$Longitude)
customers$Longitude = gsub(pattern = "^2", replacement = "-2", customers$Longitude)
customers$Longitude = gsub(pattern = "^4", replacement = "-4", customers$Longitude)
customers$Longitude = gsub(pattern = "^1", replacement = "-1", customers$Longitude)

names(customers)[names(customers)=="Longitude"] <- "latitude"
names(customers)[names(customers)=="Latitude"] <- "longitude"

sales_top_states_bunnings18 <- merge(sales_top_states_bunnings18, Bunnings_Store_Areas, by.x = c("latitude", "longtitude"), by.y = c("latitude", "longitude"), all.x = TRUE)

Bunnings_Store_Areas <- merge(Bunnings_Store_Areas, customers, by = "Customer", all.x = TRUE)
sales_top_states_bunnings18 <- merge(sales_top_states_bunnings18, Bunnings_Store_Areas, by.x = c("latitude", "longtitude"), by.y = c("latitude", "longitude"), all.x = TRUE)
sales_top_states_bunnings18 <- merge(sales_top_states_bunnings18, materials1, by.x = "product", by.y =  "Product", all.x = TRUE)

i  <- with(sales_top_states_bunnings19, aggregate(mean_rain, list(stationname= stationname, quarter = quarter), max, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings19, i)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "max_rain_quarter"

i  <- with(sales_top_states_bunnings19, aggregate(mean_max_temp, list(stationname= stationname, quarter = quarter), max, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings19, i)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "max_temp_quarter"

i  <- with(sales_top_states_bunnings19, aggregate(mean_rain, list(stationname= stationname, month = month), sum, na.rm = TRUE))
sales_top_states_bunnings19 = merge(sales_top_states_bunnings19, i)
names(sales_top_states_bunnings19)[names(sales_top_states_bunnings19)=="x"] <- "sum_rain_month"



sales_top_states_bunnings19$rain_index <- sales_top_states_bunnings19$max_rain_quarter/sales_top_states_bunnings19$mean_rain
sales_top_states_bunnings19$rain_index[sales_top_states_bunnings19$rain_index == '-Inf'] <- NA      
sales_top_states_bunnings19$rain_index[sales_top_states_bunnings19$rain_index == 'Inf'] <- NA      

sales_top_states_bunnings19$temp_index <- sales_top_states_bunnings19$mean_temp/sales_top_states_bunnings19$max_temp_quarter

sales_top_states_bunnings19$qty[sales_top_states_bunnings19$sales == '0'] <- 0 
sales_top_states_bunnings19$cost[sales_top_states_bunnings19$sales == '0'] <- 0 

sales_top_states_bunnings19$rain_5 <- ifelse(sales_top_states_bunnings19$mean_rain <= 0.5,1,0)
sales_top_states_bunnings19$tmp10 <- ifelse(sales_top_states_bunnings19$mean_min_temp >= 10,1,0)

sales_top_states_bunnings19 <- sales_top_states_bunnings19[ !sales_top_states_bunnings19$store %in% c("Bunnings Kalamunda 2094"),]


sales_top_states_bunnings19 <- sales_top_states_bunnings19[ ! sales_top_states_bunnings19$product %in% c("NWay Veg & Herb Natrasoap 200ml",
                                                                                                         "Yts NW Caterpillar Killer Dipel 40g",
                                                                                                         "Yts Pathweeder Concentrate 500ml",
                                                                                                         "Yts Zero Rapid 1HR RTU 1L AU",
                                                                                                         "ZZZYts ClonexPurpleRootHormone Gel 50ml#",
                                                                                                         "DL Organic Plant Food 2kg WA##"),]




  #Townsville insect killer
      # sales_top_states_bunnings20 <- sales_top_states_bunnings19[sales_top_states_bunnings19$product %in% c("Yts NW Caterpillar Killer Dipel 40g"),]
       #sales_top_states_bunnings20 <- sales_top_states_bunnings20[sales_top_states_bunnings20$state %in% c("QLD"),]
       #BRISBANE  BRISBANE EAST BRISBANE NORTH BRISBANE SOUTH  BRISBANE WEST
       sales_top_states_bunnings20 <- sales_top_states_bunnings19[sales_top_states_bunnings19$Area.x %in% c("Brisbane", "Brisbane West", "Brisbane North", "Brisbane South"),]
       sales_top_states_bunnings30 <- sales_top_states_bunnings19[sales_top_states_bunnings19$Area.x %in% c("Melb West", "Melb South", "Melb East", "Melb North", "Melb NW", "Melb SW"),]
       sales_top_states_bunnings40 <- sales_top_states_bunnings19[sales_top_states_bunnings19$Area.x %in% c("Perth East", "Perth South", "Perth"),]
       
       
       #NWay Veg & Herb Natrasoap 200ml, Yts NW Caterpillar Killer Dipel 40g, Yts Pathweeder Concentrate 500ml, Yts Zero Rapid 1HR RTU 1L AU, ZZZYts ClonexPurpleRootHormone Gel 50ml#
       
       #sales_top_states_bunnings20 <- sales_top_states_bunnings20[sales_top_states_bunnings20$Cat2 %in% c("Plant Protection"),]
                                                        
                                                                     
                                                                    
       
       
       sales_top_states_bunnings20 <- sales_top_states_bunnings20[ !sales_top_states_bunnings20$store_s %in% c("Compton Rd Underwood"),]
      # sales_top_states_bunnings20 <- sales_top_states_bunnings20[,c(8,24:31,42)]
      # plot.ts(sales_top_states_bunnings20)    
      sales_top_states_bunnings20 <- sales_top_states_bunnings20[! sales_top_states_bunnings20$weeks1 %in% c("2012 00", "2013 00", "2014 00", "2015 00", "2016 00"),] 
      sales_top_states_bunnings30 <- sales_top_states_bunnings30[! sales_top_states_bunnings30$weeks1 %in% c("2012 00", "2013 00", "2014 00", "2015 00", "2016 00"),] 
      sales_top_states_bunnings40 <- sales_top_states_bunnings40[! sales_top_states_bunnings40$weeks1 %in% c("2012 00", "2013 00", "2014 00", "2015 00", "2016 00"),] 
      
      
      
      sales_top_states_bunnings30 <- merge(sales_top_states_bunnings30, g, by.x = c("stationname", "weeks1") , by.y = c("stationname", "weeks1"), all.x = TRUE) 
      names(sales_top_states_bunnings30)[names(sales_top_states_bunnings30)=="x"] <- "sum_rain_perweek"
      
      sales_top_states_bunnings40 <- merge(sales_top_states_bunnings40, g, by.x = c("stationname", "weeks1") , by.y = c("stationname", "weeks1"), all.x = TRUE) 
      names(sales_top_states_bunnings40)[names(sales_top_states_bunnings40)=="x"] <- "sum_rain_perweek"
      
      
       # h <- h[!h$weeks1 %in% c("2012 00", "2013 00", "2014 00", "2015 00", "2016 00"),]     
       # hist(sales_top_states_bunnings20$mean_temp)
       
      # sales_top_states_bunnings20$rain_index200 <- ifelse(sales_top_states_bunnings20$rain_index > 200,1,0)
       sales_area$area_rain_5 <- ifelse(sales_area$weekly_rain_per_area > 5,1,0)
       sales_area$area_tmp5 <- ifelse(sales_area$weekly_min_temp_per_area > 5,1,0)
       
       sales_area_melb$area_rain_10 <- ifelse(sales_area_melb$weekly_rain_per_area.x > 10,1,0)
       sales_area_melb$area_tmp5 <- ifelse(sales_area_melb$weekly_min_temp_per_area > 5,1,0)
       sales_area_melb$area_tmp18 <- ifelse(sales_area_melb$weekly_max_temp_per_area > 18,1,0)
       
       sales_area_perth$area_rain_5 <- ifelse(sales_area_perth$weekly_rain_per_area.x > 5,1,0)
       sales_area_perth$area_tmp5 <- ifelse(sales_area_perth$weekly_min_temp_per_area > 5,1,0)
       sales_area_perth$area_tmp18 <- ifelse(sales_area_perth$weekly_max_temp_per_area > 18,1,0)
       sales_area_perth$area_humid50 <- ifelse(sales_area_perth$weekly_mean_humid_per_area > 50,1,0)
       
       
       sales_area_brisbane$area_tmp27 <- ifelse(sales_area_brisbane$weekly_max_temp_per_area > 27,1,0)
       sales_area_brisbane$area_rain_12 <- ifelse(sales_area_brisbane$weekly_rain_per_area.x > 12,1,0)
  
       sales_area7$area_tmp51 <- ifelse(sales_area7$last_week_min_temp > 5,1,0)
       sales_area7$area_tmp52 <- ifelse(sales_area7$min_temp_twoweeksago > 5,1,0)
       sales_area7$area_tmp53 <- ifelse(sales_area7$min_temp_threeweeksago > 5,1,0)
       sales_area7$area_tmp54 <- ifelse(sales_area7$min_temp_fourweeksago > 5,1,0)
       sales_area_brisbane$area_tmp55 <- ifelse(sales_area_brisbane$min_temp_fiveweeksago > 5,1,0)
       
       
       sales_area_melb$area_tmp51 <- ifelse(sales_area_melb$last_week_mean_min_temp > 5,1,0)
       sales_area_melb$area_tmp52 <- ifelse(sales_area_melb$min_temp_twoweeksago > 5,1,0)
       sales_area_melb$area_tmp53 <- ifelse(sales_area_melb$min_temp_threeweeksago > 5,1,0)
       sales_area_melb$area_tmp54 <- ifelse(sales_area_melb$min_temp_fourweeksago > 5,1,0)
       sales_area_melb$area_tmp55 <- ifelse(sales_area_melb$min_temp_fiveweeksago > 5,1,0)
       sales_area_melb$area_humid51 <- ifelse(sales_area_melb$last_week_rel_humid > 45,1,0)
       sales_area_melb$area_humid52 <- ifelse(sales_area_melb$rel_humid_twoweeksago > 45,1,0)
       sales_area_melb$area_humid53 <- ifelse(sales_area_melb$rel_humid_threeweeksago > 45,1,0)
       sales_area_melb$area_humid54 <- ifelse(sales_area_melb$rel_humid_fourweeksago > 45,1,0)
       sales_area_melb$area_humid55 <- ifelse(sales_area_melb$rel_humid_fiveweeksago > 45,1,0)
       
       sales_area_perth$area_tmp51 <- ifelse(sales_area_perth$last_week_mean_min_temp > 5,1,0)
       sales_area_perth$area_tmp52 <- ifelse(sales_area_perth$min_temp_twoweeksago > 5,1,0)
       sales_area_perth$area_tmp53 <- ifelse(sales_area_perth$min_temp_threeweeksago > 5,1,0)
       sales_area_perth$area_tmp54 <- ifelse(sales_area_perth$min_temp_fourweeksago > 5,1,0)
       
       sales_area_perth$area_humid51 <- ifelse(sales_area_perth$last_week_rel_humid > 50,1,0)
       sales_area_perth$area_humid52 <- ifelse(sales_area_perth$rel_humid_twoweeksago > 50,1,0)
       sales_area_perth$area_humid53 <- ifelse(sales_area_perth$rel_humid_threeweeksago > 50,1,0)
       sales_area_perth$area_humid54 <- ifelse(sales_area_perth$rel_humid_fourweeksago > 50,1,0)
       sales_area_perth$area_tmp55 <- ifelse(sales_area_perth$min_temp_fiveweeksago > 5,1,0)
       
       sales_area_brisbane$area_humid51 <- ifelse(sales_area_brisbane$last_week_rel_humid > 45,1,0)
       sales_area_brisbane$area_humid52 <- ifelse(sales_area_brisbane$rel_humid_twoweeksago > 45,1,0)
       sales_area_brisbane$area_humid53 <- ifelse(sales_area_brisbane$rel_humid_threeweeksago > 45,1,0)
       sales_area_brisbane$area_humid54 <- ifelse(sales_area_brisbane$rel_humid_fourweeksago > 45,1,0)
       
       sales_area_brisbane$area_tmp55 <- ifelse(sales_area_brisbane$min_temp_fiveweeksago > 5,1,0)
       
       
       
       
       #train_ridge <- train[!is.na(train),]
      
      #sales_top_states_bunnings21 <- sales_top_states_bunnings20
      
       i <- with(sales_top_states_bunnings20, aggregate(sales, list(Area.x = Area.x, product = product, weeks1 = weeks1), sum))
       names(i)[names(i)=="x"] <- "sales_per_area"
       
       i <- with(sales_top_states_bunnings30, aggregate(sales, list(Area.x = Area.x, product = product, weeks1 = weeks1), sum))
       names(i)[names(i)=="x"] <- "sales_per_area"
       
       i <- with(sales_top_states_bunnings40, aggregate(sales, list(Area.x = Area.x, product = product, weeks1 = weeks1), sum))
       names(i)[names(i)=="x"] <- "sales_per_area"
       
       
       ll <- with(sales_top_states_bunnings20, aggregate(mean_rain, list(Area.x = Area.x, weeks1 = weeks1), mean))
       names(ll)[names(ll)=="x"] <- "weekly_rain_per_area"
       
       ll <- with(sales_top_states_bunnings30, aggregate(sum_rain_perweek, list(Area.x = Area.x, weeks1 = weeks1), mean))
       names(ll)[names(ll)=="x"] <- "weekly_rain_per_area"
       
       ll <- with(sales_top_states_bunnings40, aggregate(sum_rain_perweek, list(Area.x = Area.x, weeks1 = weeks1), mean))
       names(ll)[names(ll)=="x"] <- "weekly_rain_per_area"
       
       #check if here sum is needed
       l <- with(h, aggregate(last_week_rain, list(Area.x = Area.x, weeks1 = weeks1), mean))
       names(l)[names(l)=="x"] <- "lastweek_rain_per_area"
       
     
       
        sales_area <- merge(i,l, by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
        sales_area <- merge(sales_area,ll, by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
        
     # hih <- with(hh, aggregate(last_week_sales, list(Area.x = Area.x, weeks1 = weeks1), sum))
      
        sales_area_melb <- merge(i,ll, by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
        sales_area_perth <- merge(i,ll, by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
     
       ii <- with(sales_top_states_bunnings20, aggregate(mean_max_temp, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area = merge(sales_area, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area)[names(sales_area)=="x"] <- "weekly_max_temp_per_area"
       
       ii <- with(sales_top_states_bunnings20, aggregate(mean_wind, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area = merge(sales_area, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area)[names(sales_area)=="x"] <- "weekly_mean_wind_per_area"
       
       ii <- with(sales_top_states_bunnings20, aggregate(mean_evapo, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area = merge(sales_area, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area)[names(sales_area)=="x"] <- "weekly_mean_evapo_per_area"
       
       ii <- with(sales_top_states_bunnings20, aggregate(mean_humid, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area = merge(sales_area, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area)[names(sales_area)=="x"] <- "weekly_mean_humid_per_area"
       
       ii <- with(sales_top_states_bunnings20, aggregate(mean_solar, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area = merge(sales_area, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area)[names(sales_area)=="x"] <- "weekly_mean_solar_per_area"
       
       ii <- with(sales_top_states_bunnings20, aggregate(mean_min_temp, list(Area.x = Area.x, weeks1 = weeks1), min))
       sales_area = merge(sales_area, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area)[names(sales_area)=="x"] <- "weekly_min_temp_per_area"
  
       
       ii <- with(sales_top_states_bunnings30, aggregate(mean_max_temp, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area_melb = merge(sales_area_melb, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_melb)[names(sales_area_melb)=="x"] <- "weekly_max_temp_per_area"
       
       ii <- with(sales_top_states_bunnings30, aggregate(mean_wind, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area_melb = merge(sales_area_melb, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_melb)[names(sales_area_melb)=="x"] <- "weekly_mean_wind_per_area"
       
       ii <- with(sales_top_states_bunnings30, aggregate(mean_evapo, list(Area.x = Area.x, weeks1 = weeks1), mean, na.rm= TRUE))
       sales_area_melb = merge(sales_area_melb, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_melb)[names(sales_area_melb)=="x"] <- "weekly_mean_evapo_per_area"
       
       ii <- with(sales_top_states_bunnings30, aggregate(mean_humid, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area_melb = merge(sales_area_melb, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_melb)[names(sales_area_melb)=="x"] <- "weekly_mean_humid_per_area"
       
       ii <- with(sales_top_states_bunnings30, aggregate(mean_solar, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area_melb = merge(sales_area_melb, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_melb)[names(sales_area_melb)=="x"] <- "weekly_mean_solar_per_area"
       
       ii <- with(sales_top_states_bunnings30, aggregate(mean_min_temp, list(Area.x = Area.x, weeks1 = weeks1), mean, na.rm = TRUE))
       sales_area_melb = merge(sales_area_melb, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_melb)[names(sales_area_melb)=="x"] <- "weekly_min_temp_per_area"
       
       ii <- with(sales_top_states_bunnings40, aggregate(mean_max_temp, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area_perth = merge(sales_area_perth, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_perth)[names(sales_area_perth)=="x"] <- "weekly_max_temp_per_area"
       
       ii <- with(sales_top_states_bunnings40, aggregate(mean_wind, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area_perth = merge(sales_area_perth, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_perth)[names(sales_area_perth)=="x"] <- "weekly_mean_wind_per_area"
       
       ii <- with(sales_top_states_bunnings40, aggregate(mean_evapo, list(Area.x = Area.x, weeks1 = weeks1), mean, na.rm= TRUE))
       sales_area_perth = merge(sales_area_perth, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_perth)[names(sales_area_perth)=="x"] <- "weekly_mean_evapo_per_area"
       
       ii <- with(sales_top_states_bunnings40, aggregate(mean_humid, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area_perth = merge(sales_area_perth, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_perth)[names(sales_area_perth)=="x"] <- "weekly_mean_humid_per_area"
       
       ii <- with(sales_top_states_bunnings40, aggregate(mean_solar, list(Area.x = Area.x, weeks1 = weeks1), mean))
       sales_area_perth = merge(sales_area_perth, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_perth)[names(sales_area_perth)=="x"] <- "weekly_mean_solar_per_area"
       
       ii <- with(sales_top_states_bunnings40, aggregate(mean_min_temp, list(Area.x = Area.x, weeks1 = weeks1), mean, na.rm = TRUE))
       sales_area_perth = merge(sales_area_perth, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
       names(sales_area_perth)[names(sales_area_perth)=="x"] <- "weekly_min_temp_per_area"
      # sales_area1 <- sales_area
       
       sales_area <- merge(sales_area, materials, by.x = c("product"), by.y = c("Product"), all.x = TRUE)
       sales_area1 <- sales_area1[! sales_area1$product %in% c("DL Organic Plant Food 2.5kg 4pk##",
                                                              "DL Organic Plant Food 2kg WA",
                                                              "Yts Clonex Purple Root Hormone Gel 50ml#"),]
       
       sales_area_melb <- merge(sales_area_melb, materials, by.x = c("product"), by.y = c("Product"), all.x = TRUE)
      
       sales_area1$Cat2 <- as.factor(sales_area1$Cat2)
       sales_area1$Cat3 <- as.factor(sales_area1$Cat3)
       sales_area1$Cat4 <- as.factor(sales_area1$Cat4)
       
 library(zoo)
  weatherdata_all_new <- weatherdata_all3
 #g$rain_2weeks <- rollsumr(weatherdata_all_new$rain, k=1 , fill = NA)
 #weatherdata_all_new$rain_2weeks <- ave(weatherdata_all_new$sum_rain, weatherdata_all_new$stationname,weatherdata_all_new$weeks1, cumsum(c(0, head(x, -1))))
 
  
 #weatherdata_all_9 <- weatherdata_all_new(c(1,2,)) 
 library(tidyverse)

   h <- g %>% group_by(stationname) %>% mutate(last_week_rain = lag(sum_rain))
   
   llg <- ll %>% group_by(Area.x) %>% mutate(last_week_rain = lag(x))
   llg <- llg %>% group_by(Area.x) %>% mutate(rain_twoweeks_ago = lag(last_week_rain))
   llg <- llg %>% group_by(Area.x) %>% mutate(rain_threeweeks_ago = lag(rain_twoweeks_ago))
   llg <- llg %>% group_by(Area.x) %>% mutate(rain_fourweeks_ago = lag(rain_threeweeks_ago))
   llg <- llg %>% group_by(Area.x) %>% mutate(rain_fiveweeks_ago = lag(rain_fourweeks_ago))
   llg <- llg[-c(3:7)]
   
   iig <- ii %>% group_by(Area.x) %>% mutate(last_week_mean_max_temp = lag(x))
   iig <- iig %>% group_by(Area.x) %>% mutate(max_temp_twoweeksago = lag(last_week_mean_max_temp))
   iig <- iig %>% group_by(Area.x) %>% mutate(max_temp_threeweeksago = lag(max_temp_twoweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(max_temp_fourweeksago = lag(max_temp_threeweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(max_temp_fiveweeksago = lag(max_temp_fourweeksago))
   iig <- iig[-c(3:7)]
   
   iig <- ii %>% group_by(Area.x) %>% mutate(last_week_mean_min_temp = lag(x))
   iig <- iig %>% group_by(Area.x) %>% mutate(min_temp_twoweeksago = lag(last_week_mean_min_temp))
   iig <- iig %>% group_by(Area.x) %>% mutate(min_temp_threeweeksago = lag(min_temp_twoweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(min_temp_fourweeksago = lag(min_temp_threeweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(min_temp_fiveweeksago = lag(min_temp_fourweeksago))
   iig <- iig[-c(3:7)]
   
   
   
   iig <- ii %>% group_by(Area.x) %>% mutate(last_week_rel_humid = lag(x))
   iig <- iig %>% group_by(Area.x) %>% mutate(rel_humid_twoweeksago = lag(last_week_rel_humid))
   iig <- iig %>% group_by(Area.x) %>% mutate(rel_humid_threeweeksago = lag(rel_humid_twoweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(rel_humid_fourweeksago = lag(rel_humid_threeweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(rel_humid_fiveweeksago = lag(rel_humid_fourweeksago))
   iig <- iig[-c(3:7)]
   
   iig <- ii %>% group_by(Area.x) %>% mutate(last_week_mean_wind = lag(x))
   iig <- iig %>% group_by(Area.x) %>% mutate(mean_wind_twoweeksago = lag(last_week_mean_wind))
   iig <- iig %>% group_by(Area.x) %>% mutate(mean_wind_threeweeksago = lag(mean_wind_twoweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(mean_wind_fourweeksago = lag(mean_wind_threeweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(mean_wind_fiveweeksago = lag(mean_wind_fourweeksago))
   iig <- iig[-c(3:7)]
   
   iig <- ii %>% group_by(Area.x) %>% mutate(last_week_evapo = lag(x))
   iig <- iig %>% group_by(Area.x) %>% mutate(evapo_twoweeksago = lag(last_week_evapo))
   iig <- iig %>% group_by(Area.x) %>% mutate(evapo_threeweeksago = lag(evapo_twoweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(evapo_fourweeksago = lag(evapo_threeweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(evapo_fiveweeksago = lag(evapo_fourweeksago))
   iig <- iig[-c(3:7)]
   
   iig <- ii %>% group_by(Area.x) %>% mutate(last_week_solar = lag(x))
   iig <- iig %>% group_by(Area.x) %>% mutate(solar_twoweeksago = lag(last_week_solar))
   iig <- iig %>% group_by(Area.x) %>% mutate(solar_threeweeksago = lag(solar_twoweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(solar_fourweeksago = lag(solar_threeweeksago))
   iig <- iig %>% group_by(Area.x) %>% mutate(solar_fiveweeksago = lag(solar_fourweeksago))
   iig <- iig[-c(3:7)]
   
   
   
 i <- i[with(i, order(Area.x, product, weeks1)),]
 
 #hh  <-  i  %>% group_by(product) %>% mutate(last_week_sales = lag(sales_per_area))
 i <- with(sales_top_states_bunnings40, aggregate(sales, list(Area.x = Area.x, product = product, weeks1 = weeks1), sum))
 hhi <- i %>% group_by(Area.x,product) %>% mutate(last_week_sales = lag(x))
 hhi <- hhi %>% group_by(Area.x, product) %>% mutate(sales_twoweeksago = lag(last_week_sales))
 hhi <- hhi %>% group_by(Area.x, product) %>% mutate(sales_threeweeksago = lag(sales_twoweeksago))
 hhi <- hhi %>% group_by(Area.x, product) %>% mutate(sales_fourweeksago = lag(sales_threeweeksago))
 hhi <- hhi %>% group_by(Area.x, product) %>% mutate(sales_fiveweeksago = lag(sales_fourweeksago))
 hhi <- hhi[-c(4:8)]
 
 ti <- live_chat_rats2  %>% mutate(count_last_week = lag(count))
 ti <- ti %>% mutate(count_twoweeksago = lag(count_last_week))
 ti <- ti %>% mutate(count_threeweeksago = lag(count_twoweeksago))
 ti <- ti %>% mutate(count_fourweeksago = lag(count_threeweeksago))
 ti <- ti %>% mutate(count_fiveweeksago = lag(count_fourweeksago))
 
 
 
 
 
 #library(tidyverse)
 tt <- hh[-c(4)]
 
 
 #sales_top_states_bunnings31 <- sales_top_states_bunnings20 %>% group_by(stationname, weeks1) %>% mutate(last_week_rain = lag(sum_rain))
 
sales_area2 <- sales_area1
sales_area2 <- merge(sales_area2, tt, by.x = c("Area.x", "product", "weeks1"), by.y = c("Area.x", "product", "weeks1"), all.x = TRUE)
 # weatherdata_all_new <- weatherdata_all_new[,c(1,2,13)]
  
 k <- with(weatherdata_all_new, aggregate(rain, list(stationname = stationname, weeks1 = weeks1), sum, na.rm = TRUE))
 weatherdata_all_new = merge(weatherdata_all_new, k)
 names(k)[names(k)=="x"] <- "sum_rain"
 
 g <- with(sales_top_states_bunnings24, aggregate(sum_rain, list(stationname= stationname, month = month), sum, na.rm = TRUE))
 sales_top_states_bunnings24 = merge(sales_top_states_bunnings24, g)
 names(sales_top_states_bunnings24)[names(sales_top_states_bunnings24)=="x"] <- "monthly_rain"
 
 #final_data_coke_70 <- final_data_coke_70[with(final_data_coke_70, order(item_id, shop_id, week_id)),]

 g <- g[with(g, order(stationname, weeks1)),]

sales_area1 <- sales_area 
sales_area1$sales_areadate <- as.Date(paste("1", sales_area$weeks1, sep = "-"), format = "%w-%Y %W")
sales_area_melb$sales_areadate <- as.Date(paste("1", sales_area_melb$weeks1, sep = "-"), format = "%w-%Y %W")
sales_area_melb$season <- quarters(sales_area_melb$sales_areadate)
sales_area_melb$month <- months(sales_area_melb$sales_areadate)
sales_area_melb$month <- factor(sales_area_melb$month, levels= month.name)
sales_area_melb$Month_Yr <- format(as.Date(sales_area_melb$sales_areadate), "%Y-%m")


sales_area_perth$sales_areadate <- as.Date(paste("1", sales_area_perth$weeks1, sep = "-"), format = "%w-%Y %W")
sales_area_perth$season <- quarters(sales_area_perth$sales_areadate)
sales_area_perth$month <- months(sales_area_perth$sales_areadate)
sales_area_perth$month <- factor(sales_area_perth$month, levels= month.name)
sales_area_perth$Month_Yr <- format(as.Date(sales_area_perth$sales_areadate), "%Y-%m")


sales_area_brisbane$Month_Yr <- format(as.Date(sales_area_brisbane$sales_areadate), "%Y-%m")

sales_area1$season <- quarters(sales_area1$sales_areadate)
sales_area1$month <- months(sales_area1$sales_areadate)
sales_area1$month <- factor(sales_area1$month, levels= month.name)

gg <- with(sales_area1, aggregate(sales_per_area, list(Area.x = Area.x,product = product, month = month), sum))
sales_area1 = merge(sales_area1, gg, all.x = TRUE)
names(sales_area1)[names(sales_area1)=="x"] <- "monthly_sales"

gg <- with(sales_area1, aggregate(sales_per_area, list(Area.x = Area.x, product =product, season = season), sum))
sales_area1 = merge(sales_area1, gg, all.x = TRUE)
names(sales_area1)[names(sales_area1)=="x"] <- "quarterly_sales"

sales_area2 = merge(sales_area2, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
names(sales_area2)[names(sales_area2)=="x"] <- "weekly_max_temp_per_area"


sales_area7 = merge(sales_area7, iig ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)

sales_area_brisbane = merge(sales_area_brisbane,llg ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
sales_area_brisbane = merge(sales_area_brisbane,iig ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
sales_area_brisbane = merge(sales_area_brisbane,hhi, by.x = c("Area.x","product", "weeks1"), by.y = c("Area.x","product", "weeks1"), all.x = TRUE)

sales_area_melb = merge(sales_area_melb,llg ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
sales_area_melb = merge(sales_area_melb,iig ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
sales_area_melb = merge(sales_area_melb,hhi, by.x = c("Area.x","product", "weeks1"), by.y = c("Area.x","product", "weeks1"), all.x = TRUE)


sales_area_perth = merge(sales_area_perth,llg ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
sales_area_perth = merge(sales_area_perth,iig ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
sales_area_perth = merge(sales_area_perth,hhi, by.x = c("Area.x","product", "weeks1"), by.y = c("Area.x","product", "weeks1"), all.x = TRUE)

sales_area10_chat = merge(sales_area10_chat, ti, by.x = "weeks1", by.y = "weeks1", all.x = TRUE)


sales_area2 = merge(sales_area2, ii ,by.x = c("Area.x", "weeks1"), by.y = c("Area.x", "weeks1"), all.x = TRUE)
names(sales_area2)[names(sales_area2)=="x"] <- "weekly_max_temp_per_area"

sales_area3 <- sales_area2[sales_area2$product %in% c("Yts Tree & Blackberry Killer 250ml","Yts Zero Glyphosate Big Gun RTU 3L", 
                                                      "Zero Glyphosate 490 Concentrate 200ml", 
                                                      "Zero Glyphosate 490 Concentrate 500ml"),]

sales_area4 <- sales_area2[sales_area2$product %in% c("Thrive All Purpose Soluble 500g 6pk",
                           "Thrive Flower & Fruit Soluble 500g 6pk",
                           "Thrive Flower&Fruit Soluble 1kg"),] 

sales_area5 <- sales_area_brisbane[sales_area_brisbane$product %in% c("Yts Fungus Gun RTU ANZ 750ml"),]
sales_area5 <- sales_area_brisbane[sales_area_brisbane$product %in% c("Ratsak Double Strength Bait Station 1kg", "Ratsak Fast Action Throwpacks(5x20g)100g", "Ratsak Double Strength Bait Station 350g", "Ratsak Fast Action Waxblocks 1kg"),]
sales_area5 <- sales_area_brisbane[sales_area_brisbane$product %in% "Yts Success Ultra Insect Control 200ml", ]
#sales_area5 <- sales_area5[sales_area5$Area.x %in% "Brisbane North",]
sales_area5 <- sales_area5[!sales_area5$Area.x %in% c("Brisbane West"),]

sales_area10 <- sales_area_perth[sales_area_perth$product %in% "Yts Success Ultra Insect Control 200ml", ]
sales_area10 <- sales_area_perth[sales_area_perth$product %in% c("Yts Fungus Gun RTU ANZ 750ml"),]
sales_area10 <- sales_area_perth[sales_area_perth$product %in% c("Ratsak Double Strength Bait Station 1kg", "Ratsak Fast Action Throwpacks(5x20g)100g", "Ratsak Double Strength Bait Station 350g", "Ratsak Fast Action Waxblocks 1kg"),]


sales_area8 <- sales_area_melb[sales_area_melb$product %in% "Yts Success Ultra Insect Control 200ml", ]
sales_area8 <- sales_area_melb[sales_area_melb$product %in% c("Yts Fungus Gun RTU ANZ 750ml"),]
sales_area8 <- sales_area_melb[sales_area_melb$product %in% c("Ratsak Double Strength Bait Station 1kg", "Ratsak Fast Action Throwpacks(5x20g)100g", "Ratsak Double Strength Bait Station 350g", "Ratsak Fast Action Waxblocks 1kg"),]
sales_area8 <- sales_area8[!sales_area8$Area.x %in% c("Melb NW", "Melb SW"),]



#sales_caterpillar <- sales_top_states_bunnings18[sales_top_states_bunnings18$product %in%  "Yts NW Caterpillar Killer Dipel 40g",]
 
 test_data <- test_data[with(test_data, order(weeks1, Area.x)),]
 
 live_chat_hagen1 <- live_chat_hagen
   
   live_chat_hagen1$weeks1 <- format(live_chat_hagen1$V16, format = "%Y %W")
   live_chat_hagen1$month <- months(live_chat_hagen1$V16)
   live_chat_hagen1$month <- factor(live_chat_hagen1$month, levels= month.name)
   
   sales_top_merged <- merge(sales_hagen_merged_top,customers, by.x = "customer",by.y = "Customer", all = TRUE)
   
   sales_top_states_bunnings19copy <- sales_top_states_bunnings19
   
   
   