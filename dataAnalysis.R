#set the working directory to be the Course Project 2 folder
setwd("~/Desktop/Online/OWinter Term 2014/Reproducible Research/Course Projects/Course Project 2")

#install.packages("R.cache")
#install.packages("memoise")
#library(R.cache)
#library(memoise)

#read in the database file into R
fileName <- "repdata-data-StormData.csv.bz2"
data <- read.csv(fileName)

#create a new dataframe with the columns EVTYPE, FATALITIES, INJURIES,
#PROPDMG, PROPDMGEXP, CROPDMGEXP and CROPDMG from data
data1 <- data.frame(data$EVTYPE, data$FATALITIES, data$INJURIES,
                    data$PROPDMG, data$PROPDMGEXP, data$CROPDMG, data$CROPDMGEXP)

#re-name the columns in the data1 data frame
names(data1) <- tolower(c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG","PROPDMGEXP", "CROPDMG", "CROPDMGEXP"))

#Turn the elements in the evtype, propdmgexp and cropdmgexp columns
#from factor into character
data1[,1] <- as.character(data1$evtype)
data1[,5] <- as.character(data1$propdmgexp)
data1[,7] <- as.character(data1$cropdmgexp)

#Please note that we will ignore the multipliers for property
#damage (PROPDMGEXP) and cropdamage (CROPDMGEXP) unless they are
#one of h/H (hundred), k/K (thousand), m/M (million) or b/B
#(billion).
#There are two reasons for doing so. 
#(1)We could not determime the meaning of the other values (such
#as, "0", "1", "2" etc.) in the PROPDMGEXP and CROPDMGEXP columns.
#(2) Including multipliers other than h, k, m or b in our analysis
#drastically increases our processing time which we are unable to
#afford with our computer's limited processing power and the 
#project deadline.
#Therefore, please be advised that our analysis and result
#interpretation that follows may not be precise or entirely correct

#28 weather types considered (please not that the following weather
#condition listing is not exhaustive). Other unrecognized weather
#conditions are considered under "other"
weather <- c("rain", "storm", "sun", "cloud", "hot", "cold",
             "dry", "wet", "windy", "hurricane", "typhoon", 
             "sand storms", "snow storms", "tornado", "humid",
             "fog", "snow", "thundersnow", "hail", "sleet", 
             "drought", "wildfire", "blizzard", "avalanche",
             "mist", "freez", "dust", "flood", "other")

#create a new data frame - dataSummary - to record the aggregate
#results of weather incident counts, fatalities, injuries,
#property damage, crop damage and
#total damage for each of the above weather conditions.
incidentTotal <- vector(mode="numeric", length=length(weather))
fatalitiesTotal <-vector(mode="numeric", length=length(weather))
injuriesTotal <-vector(mode="numeric", length=length(weather))
propDmgTotal <-vector(mode="numeric", length=length(weather))
cropDmgTotal <-vector(mode="numeric", length=length(weather))
damageTotal <-vector(mode="numeric", length=length(weather))
dataSummary <- data.frame(weather, incidentTotal, 
                          fatalitiesTotal, injuriesTotal,
                          propDmgTotal, cropDmgTotal, damageTotal)

#function to determine the multiplier's numerical value from the
#propdmgexp and cropdmgexp columns in the data1 dataframe
multiplier <- function(letter) {
        #Return the multiplier letter's numerical value
        #If the multiplier is not one of h/H, k/K, m/M, and b/B,
        #the function returns 1
        multiplier <- 1
        
        if (grepl('h', tolower(letter))) multiplier <- 100
        else if (grepl('k', tolower(letter))) multiplier <- 1000
        else if (grepl('m', tolower(letter))) multiplier <- 1000000
        else if (grepl('b', tolower(letter))) multiplier <- 1000000000
        
        multiplier
} 

#populate the dataSummary data frame with the aggregate results
#for each of the categories - fatalities, injuries, property
#damage, crop damage and total damage - for each of the 29 weather
#conditions.

#The major simplifying assumption in our analysis is to ignore
#the rows in data1 where:
#* propdmgexp is not one of h/H, k/K, m/M, and b/B
#* cropdmgexp is not one of h/H, k/K, m/M, and b/B

for (row in seq(data1$evtype)) {
        instance <- tolower(data1$evtype[row])
        propMultiplier <- multiplier(data1$propdmgexp[row])
        cropMultiplier <- multiplier(data1$cropdmgexp[row])
        print(row)
        if (propMultiplier== 1 || cropMultiplier==1) {
                next
        }
        iteration <- 0
        for (index in seq(dataSummary$weather)) {
                condition <- dataSummary$weather[index]
                iteration <- iteration + 1
                found = grepl(condition, instance)
                if (found) {
                        data1$evtype[row] = condition
                        dataSummary$incidentTotal[index] = dataSummary$incidentTotal[index] + 1 
                        dataSummary$fatalitiesTotal[index] = dataSummary$fatalitiesTotal[index] + data1$fatalities[row]
                        dataSummary$injuriesTotal[index] = dataSummary$injuriesTotal[index] + data1$injuries[row]
                        dataSummary$propDmgTotal[index] = dataSummary$propDmgTotal[index] + (data1$propdmg[row] * propMultiplier)
                        dataSummary$cropDmgTotal[index] = dataSummary$cropDmgTotal[index] + (data1$cropdmg[row] * cropMultiplier)
                        dataSummary$damageTotal[index] = dataSummary$damageTotal[index] + (dataSummary$propDmgTotal[index] + dataSummary$cropDmgTotal[index])
                        print(condition)
                        break
                        print("after the break statement")
                }
                else if (!found & iteration==29) {
                        data1$evtype[row] = condition
                        dataSummary$incidentTotal[index] = dataSummary$incidentTotal[index] + 1 
                        dataSummary$fatalitiesTotal[index] = dataSummary$fatalitiesTotal[index] + data1$fatalities[row]
                        dataSummary$injuriesTotal[index] = dataSummary$injuriesTotal[index] + data1$injuries[row]
                        dataSummary$propDmgTotal[index] = dataSummary$propDmgTotal[index] + (data1$propdmg[row] * propMultiplier)
                        dataSummary$cropDmgTotal[index] = dataSummary$cropDmgTotal[index] + (data1$cropdmg[row] * cropMultiplier)
                        dataSummary$damageTotal[index] = dataSummary$damageTotal[index] + (dataSummary$propDmgTotal[index] + dataSummary$cropDmgTotal[index])
                        print(condition)
                }
        }
}

#Save the dataSummary containing the aggregate data in a .csv file
#in case we need the processed data again and if we do not want to
#wait for the time consuming processing to repeat itself
write.csv(file='cleanData.csv', x=dataSummary)

#Read in the cleaned up data from the cleanData.csv file into R
newData <- read.csv("cleanData.csv")

#The columns and data in the newData dataframe
names(newData)
head(newData)

#Create a new data frame newData1 with an additional column that
#contains the total human injuries and fatalities due to each
#event
injuriesFatalitiesTotal <- newData$injuriesTotal + newData$fatalitiesTotal

newData1 <- cbind(newData[,2:3], newData$injuriesTotal,
                  newData$fatalitiesTotal, injuriesFatalitiesTotal,
                  newData[,6:8])
names(newData1) <- c("weather", "incidentCount", "injuries",
                     "fatalities", "injuryFatalityTotal",
                     "propertyDamage", "cropDamage", "damageTotal")

#The columns and data in the newData1 dataframe
names(newData1)
head(newData1)

#Across the United States, which types of events (as indicated in
#the EVTYPE variable) are most harmful with respect to population
#health?

#Pie chart of the top 5 types of event corresponding to the effect on
#injuries and fatalities combined

#Initialize the margin and plotting parameters
par(mar=c(2,2,3,1))
par(mfrow=c(1,1))

#Install the 3D pie chart package "plotrix" if necessary
#install.packages("plotrix")
library(plotrix)

X <- newData1[order(newData1$injuryFatalityTotal, decreasing=TRUE),][1:5,]
labels <- paste(X$weather, "\n", X$injuryFatalityTotal)
pie3D(X$injuryFatalityTotal, labels = labels, 
    main="Injuries and Fatalities Combined due to the Top 5 Weather
    Events", labelcex=1.2)

#Across the United States, which types of events have the greatest
#economic consequences?

#Pie chart of the top 5 types of event with the greatest economic
#consequences

#Plot the 3D pie chart of the top 5 events that had the most effect on
#the economy in the past
Y <- newData1[order(newData1$damageTotal, decreasing=TRUE),][1:5,]
damageInTrillions <- round(Y$damageTotal/1e+12)
labels <- paste(Y$weather, "\n", damageInTrillions)
pie3D(damageInTrillions, labels = labels, main="Consequences of the Top 5\n Weather Events on the Economy (in Trillions of Dollars)", labelcex=1.2)

### (Additional analysis) Across the United States, which types of
#events occured the most?

#Pie chart of the top 5 types of event with the highest frequency

#Plot the 3D pie chart of the top 5 events that occured the most in the 
#past

Z <- newData1[order(newData1$incidentCount, decreasing=TRUE),][1:5,]
labels <- paste(Z$weather, "\n", Z$incidentCount)
pie3D(Z$incidentCount, labels = labels, main="5 Highest Occuring Weather Events in the\n United States between 1950 and 2011", labelcex=1.2)
