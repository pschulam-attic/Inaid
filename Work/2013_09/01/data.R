library(lme4)
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

prep.data <- function(data, bucket.size = 92, min.visits = 6) {
    pdata <- ddply(data, .(PtID), prep.patient.data, bucket.size = bucket.size)

    nvisits <- ddply(pdata, .(PtID), summarize, n = max(x))
    pids <- subset(nvisits, n >= 6)$PtID
    
    subset(pdata, PtID %in% pids)
}

prep.normalized.data <- function(data, bucket.size = 92, min.visits = 6) {
    pdata <- prep.data(data, bucket.size, min.visits)

    mem.fit <- lmer(y ~ x + (1|PtID), data = pdata)@ranef
    names(rand.effects) <- unique(pdata$PtID)

    cpid <- function(pid) {
        as.character(pid[1])
    }

    pdata <- ddply(pdata, .(PtID), transform, y = y - rand.effects[[cpid(PtID)]])
    pdata
}
