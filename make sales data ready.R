
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
# store_locations <- unique(weatherdata_all$stationname)
# store_locations <- unique(customers$Store)
# store_locations_coord <- lapply(store_locations, geocodeAdddress)
# store_locations_column <- do.call(rbind.data.frame, store_locations)
# store_locations_coord_column <- do.call(rbind.data.frame, store_locations_coord)
# weather_store_locations <- cbind(store_locations_column,store_locations_coord_column)
# names(weather_store_locations)[1]<- "station_name"
# names(weather_store_locations)[2]<- "longitude"
# names(weather_store_locations)[3]<- "latitude"

#clean sales data lon/lat info

# Geocoding script for large list of addresses.


#load up the ggmap library
library(ggmap)
# get the input data
infile <- "customers"
#data <- read.csv(paste0('./', infile, '.csv'))
data <- customers
# get the address list, and append "Australia" to the end to increase accuracy 
# (change or remove this if your address already include a country etc.)
addresses = data$Store
addresses = paste0(addresses, ", Australia")

#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

#now we add the latitude and longitude to the main data
data$lat <- geocoded$lat
data$long <- geocoded$long
data$accuracy <- geocoded$accuracy

#finally write it all to the output files
saveRDS(data, paste0("data", infile ,"_geocoded.rds"))
write.table(data, file=paste0("data", infile ,"_geocoded.csv"), sep=",", row.names=FALSE)



sapply(sales_hagen_material, function(x) length(unique(x)))
sapply(sales_hagen_material,function(x) sum(is.na(x)))



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

sales_hagen_material <- sales_hagen[sales_hagen$material %in% material_id,]                       

sales_hagen_material_customer <- merge(sales_hagen_material,data, by.x = "customer",by.y = "Customer", all.x = TRUE)

#sales_hagen_material_customer <- sales_hagen_material_customer[sales_hagen_material_customer$customer != #]


keep <- c("VIC", "Victoria" ,"QLD","Queensland", "WA", "Western Australia")
sales_hagen_material_states <- sales_hagen_material_customer[sales_hagen_material_customer$Location %in% keep,]

# keep <- c("VIC", "Victoria" ,"QLD","Queensland", "WA", "Western Australia")
# sales_hagen_material_states1 <- sales_hagen_material[sales_hagen_material$state %in% keep,]
# sales_hagen_material_states <- rbind(sales_hagen_material_states1, sales_hagen_material_states)
# sales_hagen_material_states <-  sales_hagen_material_states[with(sales_hagen_material_states, order(start_date, customer, material)),]
# 
# names(sales_hagen_material_states)[names(sales_hagen_material_states)=="longitude"] <- "Latitude"
# names(sales_hagen_material_states)[names(sales_hagen_material_states)=="latitude"] <- "Longitude"
# 
# 
# sales_hagen_material_states$Latitude = gsub(pattern = "-", replacement = "", sales_hagen_material_states$Latitude)
# sales_hagen_material_states$Latitude = gsub(pattern = "^3", replacement = "-3", sales_hagen_material_states$Latitude)
# sales_hagen_material_states$Latitude = gsub(pattern = "^2", replacement = "-2", sales_hagen_material_states$Latitude)
# sales_hagen_material_states$Latitude = gsub(pattern = "^4", replacement = "-4", sales_hagen_material_states$Latitude)
# sales_hagen_material_states$Latitude = gsub(pattern = "^1", replacement = "-1", sales_hagen_material_states$Latitude)
# 
# sales_hagen_material_states$latitude <- as.numeric(sales_hagen_material_states$lat)
# #sales_hagen_material_states <- sales_hagen_material_states[sales_hagen_material_states$Longitude != 0,]
# 
 save(sales_hagen_material_states, file = "sales_hagen_material_states.RData")
# 
# sales_hagen_material_states_pos <- sales_hagen_material_states[sales_hagen_material_states$sales >= 0,]
# 
# sapply(sales_hagen, function(x) length(unique(x)))
# sales10 <- merge(sales_hagen_material_states,customers, by.x = "customer",by.y = "Customer", all = TRUE)

#yatestores_weather_postcode2 <- yatestores_weather_postcode

#yatestores_weather_postcode2$store_s <- toupper(yatestores_weather_postcode2$store_s)
sales_hagen_material_states$Store <- toupper(sales_hagen_material_states$Store)
#sales11 = merge(sales_hagen_material_states, yatestores_weather_postcode2, by.x = "Store", by.y = "store_s", all.x = TRUE)





sales10 = sales11[!is.na(sales11$stationid),]

sales10$week = format(sales10$date, format = "%Y %W") 
save(sales10, file = "sales10.RData")
sales12 <- merge(sales10, weather_weekly, by.x = c("name", "week"), by.y = c("stationname", "weeks"), all.x = TRUE)

save(sales12, file = 'sales12.RData')
sales12$month = format(sales12$start_date, format = "%Y %M") 
sales12$month1 <- months(sales12$start_date)
sales12$month1 <- factor(sales12$month, levels= month.name)

library(geosphere)

list1 <- data.frame(sales_hagen_material_states$long, sales_hagen_material_states$lat)
list2 <- data.frame(aws_firstdate$LONGITUDE, aws_firstdate$LATITUDE)
list1 <- unique(list1)

list1 <- as.matrix(list1)
list2 <- as.matrix(list2)
View(list1)

mat1 <- distm(list1, list2, fun = distVincentyEllipsoid)


locality  <- as.list(aws_firstdate$NAME)
locality <- c(as.vector(unlist(locality))
              
              
list2$locality <- locality
list1$locality <- list2$locality[max.col(-mat1)]

saveRDS(list1, "store_weather.rds")
write.table(list1, "store_weather.csv", sep=",", row.names=FALSE)

sales13 <- merge(sales_hagen_material_states, list1, by.x = c("lat" , "long"), by.y = c("sales_hagen_material_states.lat","sales_hagen_material_states.long"), all.x = TRUE)
saveRDS(sales13, "sales_station.rds")
write.table(sales13, "sales_station.csv", sep=",", row.names=FALSE)
#  names(list1)[names(list1)=="sales_hagen_material_states.longitude"] <- "longitude"
#  names(list1)[names(list1)=="sales_hagen_material_states.latitude"] <- "latitude"
#  names(list2)[names(list2)=="aws_firstdate.LONGITUDE"] <- "longitude"
#  names(list2)[names(list2)=="aws_firstdate.LATITUDE"] <- "latitude"
#  
# list22 <- data.frame(longitude = c(72.89537, 77.65094, 73.95325, 72.96746,
# 77.65058, 77.66715, 77.64214, 77.58415,
# 77.76180, 76.65460, 12.00),
# latitude = c(19.07726, 13.03902, 18.50330, 19.16764,
# 12.90871, 13.01693, 13.00954, 12.92079,
# 13.02212, 12.81447, 01.00),
# locality1 = c("A", "A", "B", "B", "C", "C", "C", "D", "D", "E", "F")
# list11 <- data.frame(longitude = c(80.15998, 72.89125, 77.65032, 77.60599,
# 72.88120, 76.65460, 72.88232, 77.49186,
# 72.82228, 72.88871),
# latitude = c(12.90524, 19.08120, 12.97238, 12.90927,
# 19.08225, 12.81447, 19.08241, 13.00984,
# 18.99347, 19.07990))
# mat11 <- distm(list11[,c('longitude','latitude')], list22[,c('longitude','latitude')], fun=distVincentyEllipsoid)
# # assign the name to the point in list1 based on shortest distance in the matrix
#list11$locality <- list22$locality[max.col(-mat11)]
#View(list11)
# weather_stations$station_name <- as.factor(weather_stations$station_name)
# a <- max.col(-mat11)
# View(a)



#sales13 <- na.exclude(sales12)