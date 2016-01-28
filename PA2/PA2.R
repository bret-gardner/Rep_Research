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
StormDataB$BGN_DATE <- as.Date(as.character(StormDataB$BGN_DATE), "%m/%d/%Y %H:%M:%S")
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
StormDataSmall <- StormDataB[StormDataB$BGN_DATE >= "1996-01-01" 
                             & StormDataB$BGN_DATE <= "2016-01-01",]

#Group data by EVTYPE
StormDataEvent <- group_by_(StormDataSmall, .dots=c("EVTYPE"))
AvgFatalities <- summarise_each(StormDataEvent,funs(mean))

#Pseudocode
#Download file if not already done  x
#Import file if not already done    x
#Convert dates to Date format       x
#Limit data to 1996 and on          x
#Group by event type
#Compute average cost per event by type
#Compute total cost over 10 years by event type
#Display graphical results