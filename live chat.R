#dat1 <- read.csv(/hagen/Desktop/SOW2_Chinese_Tourist_hagen_period.csv)
                 
live_chat_after20160923 <- read.table("~/Downloads/chatfafter20160923.txt", sep = "|", fill = TRUE)

live_chat <- read.table("~/Downloads/livechat20151207-20160922.txt", sep = "|")
live_chat_pre20151207 <- read.table("~/Downloads/pre20151207.txt", sep = "|", fill = TRUE)

library(data.table)
live_chatv2 <- live_chat
live_chatv2$V9 <- as.character.Date(live_chatv2$V9)
live_chatv2$V9 <- gsub("Mon, ","", live_chatv2$V9)
live_chatv2$V9 <- gsub("Tue, ","", live_chatv2$V9)
live_chatv2$V9 <- gsub("Wed, ","", live_chatv2$V9)
live_chatv2$V9 <- gsub("Thu, ","", live_chatv2$V9)
live_chatv2$V9 <- gsub("Fri, ","", live_chatv2$V9)
live_chatv2$V9 <- gsub("Sat, ","", live_chatv2$V9)
live_chatv2$V9 <- gsub("Sun, ","", live_chatv2$V9)

live_chatv2$V9 <- gsub(" pm","", live_chatv2$V9)
live_chatv2$V9 <- gsub(" am","", live_chatv2$V9)
live_chatv2$V16 <- as.Date(live_chatv2$V9, format = "%m/%d/%y")


live_chat_pre_v2 <- live_chat_pre20151207
live_chat_pre_v2$V9 <- as.character.Date(live_chat_pre_v2$V9)
live_chat_pre_v2$V9 <- gsub("Mon, ","", live_chat_pre_v2$V9)
live_chat_pre_v2$V9 <- gsub("Tue, ","", live_chat_pre_v2$V9)
live_chat_pre_v2$V9 <- gsub("Wed, ","", live_chat_pre_v2$V9)
live_chat_pre_v2$V9 <- gsub("Thu, ","", live_chat_pre_v2$V9)
live_chat_pre_v2$V9 <- gsub("Fri, ","", live_chat_pre_v2$V9)
live_chat_pre_v2$V9 <- gsub("Sat, ","", live_chat_pre_v2$V9)
live_chat_pre_v2$V9 <- gsub("Sun, ","", live_chat_pre_v2$V9)
live_chat_pre_v2$V9 <- gsub(" pm","", live_chat_pre_v2$V9)
live_chat_pre_v2$V9 <- gsub(" am","", live_chat_pre_v2$V9)

live_chat_after_v2 <- live_chat_after20160923
live_chat_after_v2$V9 <- as.character.Date(live_chat_after_v2$V9)
live_chat_after_v2$V9 <- gsub("Mon, ","", live_chat_after_v2$V9)
live_chat_after_v2$V9 <- gsub("Tue, ","", live_chat_after_v2$V9)
live_chat_after_v2$V9 <- gsub("Wed, ","", live_chat_after_v2$V9)
live_chat_after_v2$V9 <- gsub("Thu, ","", live_chat_after_v2$V9)
live_chat_after_v2$V9 <- gsub("Fri, ","", live_chat_after_v2$V9)
live_chat_after_v2$V9 <- gsub("Sat, ","", live_chat_after_v2$V9)
live_chat_after_v2$V9 <- gsub("Sun, ","", live_chat_after_v2$V9)
live_chat_after_v2$V9 <- gsub(" pm","", live_chat_after_v2$V9)
live_chat_after_v2$V9 <- gsub(" am","", live_chat_after_v2$V9)

live_chat_after_v2$V16 <- as.Date(live_chat_after_v2$V9, format = "%m/%d/%y")
live_chat_hagen <- rbind(live_chat_hagen, live_chat_after_v2)
live_chat_hagen <- rbind(live_chatv2, live_chat_pre_v2)
live_chat_rats1 <- live_chat_hagen1[live_chat_hagen1$V4 %in% "1 Pest Mice Rats",]

save(live_chat_rats1, file="live_chats_rats1.rda")

sales_top_states_bunnings15 <- merge(sales_top_states_bunnings3,df, by.x = c("store", "product", "weeks1") ,by.y = c("Var1", "Var2" ,"Var3"), all.y =  TRUE)


file_list <- list.files("~/Downloads/daily00")

setwd("/Users/hagen/Desktop/Dulux/daily00")
setwd("/Users/hagen/Desktop/Dulux/tables/vic/aireys_inlet")
file_list <- list.files()
dataset <- do.call("rbind",lapply(file_list,FUN=function(files){read.table(files,
                                                                 header=FALSE, sep=",")}))

files_names <- dir("/Users/hagen/Desktop/Dulux/daily00")
files_names <- dir("/Users/hagen/Desktop/Dulux/tables/vic")
weather_vic <- do.call(rbind,lapply(files_names,read.csv))

files <- list.files(path = "/Users/hagen/Desktop/Dulux/daily00", pattern = "")
files <- list.files(path = "/Users/hagen/Desktop/Dulux/tables/vic/", pattern = ".csv")

temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )



for (file in file_list){
       
                        # if the merged dataset doesn't exist, create it
                        if (!exists("livechat_daily00")){
                        dataset <- read.table(file, header=FALSE, sep=",")
                        }
                        
                        # if the merged dataset does exist, append to it
                        if (exists("livechat_daily00")){
                        temp_dataset <-read.table(file, header=FALSE, sep=",")
                        dataset<-rbind(dataset, temp_dataset)
                        rm(temp_dataset)
                        }
                        
                        }
 
for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep="\t")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep="\t")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

live_chat_hagen1 <- live_chat_hagen

live_chat_hagen1$weeks1 <- format(live_chat_hagen1$V16, format = "%Y %W")
live_chat_hagen1$month <- months(live_chat_hagen1$V16)
live_chat_hagen1$month <- factor(live_chat_hagen1$month, levels= month.name)
sales_area11 <- merge(sales_area10, live_chat_hagen1, by.x = "weeks1", by.y = "weeks1", all.x = TRUE)   


live_chat_rats1 <- live_chat_rats1[live_chat_rats1$V4 %in% "1 Pest Mice Rats"),]

live_chat_rats1$count <- as.numeric(ave(live_chat_rats1$weeks1,live_chat_rats1$weeks1, FUN = length))


live_chat_rats_brisbane <- subset(live_chat_rats1, V1 > 4000 & V1 < 4507)

live_chat_caterpillar <- live_chat_hagen1[live_chat_hagen1$V4 %in% "1 Pest Caterpillar",]
#live_chat_caterpillar <- live_chat_caterpillar[live_chat_caterpillar$V1 %between% c("3000","4000"),]

live_chat_rats2 <- live_chat_rats1[c(17,19)]
sales_area10_chat <- merge(sales_area10, live_chat_rats2, all.x = TRUE)

save(live_chat_caterpillar, file="live_chats_caterpillar.rda")
