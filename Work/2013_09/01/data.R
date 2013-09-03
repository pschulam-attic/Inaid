library(plyr)

read.data <- function(filename) {
    data <- arrange(read.csv(filename), PtID, diff)
    
    data$PtID <- as.factor(data$PtID)
    data$visit <- as.factor(data$visit)
    data$strFemale <- as.factor(data$strFemale)
    data$strRaceId1 <- as.factor(data$strRaceId1)
    data$smoke <- as.factor(data$smoke)

    data
}
