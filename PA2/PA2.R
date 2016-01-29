#PA2.R

#Install necessary packages
packages <- c('dplyr','data.table')
for(package in packages){
    if(package %in% rownames(installed.packages()) == FALSE){
        install.packages(package)
    }
}
require("dplyr")
require('data.table')

if(!file.exists('repdata-data-StormData.csv.bz2')){
    URL <- "https://github.com/bret-gardner/Rep_Research/blob/master/PA2/repdata-data-StormData.csv.bz2"
    download.file(URL,"repdata-data-StormData.csv.bz2")
}

if(!exists("StormData")){
    StormData <- read.csv('repdata-data-StormData.csv.bz2')
}

StormDataB <- StormData
#Convert to Date format:
StormDataB$BGN_DATE <- as.Date(as.character(StormDataB$BGN_DATE),
                               "%m/%d/%Y %H:%M:%S")
StormDataB <- subset(StormDataB, select=c("EVTYPE",
                                            "BGN_DATE",
                                            "FATALITIES",
                                            "INJURIES",
                                            "PROPDMG",
                                            "PROPDMGEXP",
                                            "CROPDMG",
                                            "CROPDMGEXP",
                                            "REFNUM"))

#Limit to data collected beginning in 1996 (more complete)
StormDataSmall <- StormDataB[StormDataB$BGN_DATE >= "2004-01-01" 
                             & StormDataB$BGN_DATE <= "2008-01-01",]

#Interpret crop/propdmgexp letter values as multipliers
StormDataSmall$PropDmgNum <- 0
StormDataSmall[StormDataSmall$PROPDMGEXP == "K", ][, "PropDmgNum"] <- 10^3
StormDataSmall[StormDataSmall$PROPDMGEXP == "M", ][, "PropDmgNum"] <- 10^6
StormDataSmall[StormDataSmall$PROPDMGEXP == "B", ][, "PropDmgNum"] <- 10^9

StormDataSmall$CropDmgNum <- 0
StormDataSmall[StormDataSmall$CROPDMGEXP == "K", ][, "CropDmgNum"] <- 10^3
StormDataSmall[StormDataSmall$CROPDMGEXP == "M", ][, "CropDmgNum"] <- 10^6
StormDataSmall[StormDataSmall$CROPDMGEXP == "B", ][, "CropDmgNum"] <- 10^9
StormDataSmall$EVTYPE <- as.character(StormDataSmall$EVTYPE)

#Calculate total damage costs (Sum of Prop and Crop damage)
StormDataSmall$TotalDmg<- ((StormDataSmall$PropDmgNum * StormDataSmall$PROPDMG)
                            + (StormDataSmall$CropDmgNum * StormDataSmall$CROPDMG))

AvgFatalities <- aggregate(FATALITIES ~ EVTYPE, data = StormDataSmall, FUN = mean)
AvgInjuries <- aggregate(INJURIES ~ EVTYPE, data = StormDataSmall, FUN = mean)
AvgCost <- aggregate(TotalDmg ~ EVTYPE, data = StormDataSmall, FUN = mean)
AvgCostSmall <- AvgCost[order(-AvgCost$TotalDmg)]
AvgCostSmall <- AvgCost[1:10,]

#Combine fatalities and injuries
HealthCost <- merge(AvgFatalities,AvgInjuries, by = "EVTYPE")
HealthCost$FatInj <- (HealthCost$FATALITIES + HealthCost$INJURIES)

#Order data
HealthCost <- HealthCost[order(-HealthCost$FatInj),]
HealthCostSmall <- HealthCost[1:10,]




#Create Bar Plots
par(mfrow = c(1,2), cex = 0.8, fg = "yellow", mar = c(6, 4, 2,0), oma = c(0,0,2,0) )
barplot(HealthCostSmall$FATALITIES, names.arg = HealthCostSmall$EVTYPE,
        las = 3, ylab = "Fatalities", main = "Fatalities", col = "black")
barplot(HealthCostSmall$INJURIES, names.arg = HealthCostSmall$EVTYPE,
        las = 3, main = "Injuries", col = "red")
mtext("Storm Effects on Human Health", outer = TRUE, col = "black", cex = 2)

barplot(AvgCostSmall$TotalDmg, names.arg = AvgCostSmall$EVTYPE,
        las = 3, ylab = "Cost", main = "Cost", col = "black")
mtext("Storm Effects on Property and Crop Damage", outer = TRUE, col = "black", cex = 2)


#Group data by EVTYPE
#StormDataEvent <- group_by_(StormDataSmall, .dots=c("EVTYPE"))
#StormFatalities <- select(StormDataEvent, EVTYPE,FATALITIES,INJURIES)
#TotalFatalities <- summarise_each(StormFatalities,funs(mean, sum))

#Pseudocode
#Download file if not already done  x
#Import file if not already done    x
#Convert dates to Date format       x
#Limit data to 1996 and on          x
#Group by event type                x
#Compute average cost per event by type
#Compute total cost over 10 years by event type
#Display graphical results