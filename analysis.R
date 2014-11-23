#set the working directory to be the course project 2 folder
setwd("~/Desktop/Online/OWinter Term 2014/Reproducible Research/Course Projects/Course Project 2")

#read in the database file into R
fileName <- "repdata-data-StormData.csv.bz2"
#unzip("repdata-data-StormData.csv.bz2")
#con1 <- unz(fileName, filename="repdata-data-StormData.csv", open = "r")da
data <- read.csv(fileName)

#create a new dataframe with the columns EVTYPE, FATALITIES, INJURIES,
#PROPDMG, PROPDMGEXP, CROPDMG, and CROPDMGEXP from data
data1 <- data.frame(data$EVTYPE, data$FATALITIES, data$INJURIES,
                    data$PROPDMG, data$PROPDMGEXP, data$CROPDMG,
                    data$CROPDMGEXP)
data1_noNAs <- data1[complete.cases(data1),]
data[,8] <- tolower(data$EVTYPE)
# head(data)
# nrow(data)
# ncol(data)
# names(data)
# data$EVTYPE
# data$PROPDMG

# #variables I should consider
# data$EVTYPE
# data$FATALITIES[1:100]
# data$INJURIES[1:100]
# data$PROPDMG[1:100]
# #removing NAs leaves us with 0 rows
# #A version of the dataset with NAs removed
# #data_noNAs <- data[complete.cases(data),]
# 
# #head(data_noNAs)
# #nrow(data_noNAs)
# # events <- list(flood=2:3, lightning=4, wind=5:8, abnormal=10:12,
# #                snowfall=13, freeze=14, astronomicalTide=16:17,
# #                avalanche=18:19, beachErosionFlood=21:24, windChill=26:27,
# #                blackIce=28:29, blizzard=30:38, blowoutTides=39:40,
# #                blowingDust=41, blowingSnow=42:47, brushFires=49:50,
# #                coastalErosionFlood=51:58, coastalStorm=59:61,
# #                cold=65:82, damFailure=84:85, hail=88, fog=89, 
# #                smoke=90, drought=95:96, dry=98:112, dust=113:119,
# #                frost=120:122, extremeWeather=129:149, flashFlood=153:187,
# #                forestFire=190, freezingRain=191:213, 
# # #ncol(data_noNAs)
# #names(data_noNAs)


#install.packages("plyr")
#library(plyr)
data2 <- data1[order(data1$PROPDMG, data1$CROPDMG), ]
#turn all of the strings in the EVTYPE column into lowercase
data1$EVTYPE <- tolower(data1$EVTYPE)