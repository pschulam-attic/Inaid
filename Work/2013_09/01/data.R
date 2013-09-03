library(plyr)

source("patient.R")

read.data <- function(filename) {
    data <- arrange(read.csv(filename), PtID, diff)
    
    data$PtID <- as.factor(data$PtID)
    data$visit <- as.factor(data$visit)
    data$strFemale <- as.factor(data$strFemale)
    data$strRaceId1 <- as.factor(data$strRaceId1)
    data$smoke <- as.factor(data$smoke)

    data
}

prep.data <- function(data, bucket.size = 92, min.visits = 6) {
    pdata <- ddply(data, .(PtID), prep.patient.data, bucket.size = bucket.size)

    nvisits <- ddply(pdata, .(PtID), summarize, n = max(x))
    pids <- subset(nvisits, n >= 6)$PtID
    
    subset(pdata, PtID %in% pids)
}
