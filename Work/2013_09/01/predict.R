library(lme4)
library(plyr)

k.nearest <- function(patient, others, k) {
    sim.to <- function(df) {
        patient.dist(patient, df)
    }

    dists <- ddply(others, .(PtID), sim.to)
    dists <- arrange(dists, distance)

    k.nearest.pids <- dists[1:k, "PtID"]
    k.nearest <- subset(others, PtID %in% k.nearest.pids)

    k.nearest
}

knn.predict <- function(pid, data, k, n.observed = 6) {
    patient <- subset(data, PtID == pid)
    others <- subset(data, PtID != pid)

    patient.obs <- patient[1:n.observed, ]
    patient.new <- patient[(n.observed+1):nrow(patient), ]

    kn <- k.nearest(patient.obs, others, k)
    kn <- subset(kn, x %in% patient.new$x)

    predictions <- ddply(kn, .(x), summarize, p = mean(y))

    results <- merge(patient.new, predictions, by.x = "x", by.y = "x")
    results
}

compute.mem.coef <- function(df) {
    mem.fit <- lmer(y ~ poly(x, degree = 2) + (poly(x, degree = 2)|PtID), data = df)
    mem.coef <- coef(mem.fit)$PtID

    pids <- as.factor(as.numeric(row.names(mem.coef)))
    int <- mem.coef[, 1]
    x <- mem.coef[, 2]
    x2 <- mem.coef[, 3]

    data.frame(PtID = pids, intercept = int, x = x, x2 = x2)
}

knn.mem.predict <- function(pid, data, k, n.observed = 6) {
    cdata <- compute.mem.coef(subset(data, x < n.observed))
    p.cdata <- subset(cdata, PtID == pid)
    
    offsets <- subset(cdata, select = c(PtID, intercept))
    data <- merge(data, offsets, by = "PtID")
    
    patient <- subset(data, PtID == pid)
    others <- subset(data, PtID != pid)

    cdist <- function(df.row) {
        v1 <- c(p.cdata$x, p.cdata$x2)
        v2 <- c(df.row$x, df.row$x2)
        data.frame(distance = sqrt(sum((v1 - v2) ** 2)))
    }

    dists <- ddply(subset(cdata, PtID != pid), .(PtID), cdist)
    dists <- arrange(dists, distance)

    kn <- subset(others, PtID %in% dists$PtID[1:k])
    kn <- transform(kn, y = y - intercept)

    changes <- ddply(kn, .(x), summarize, change = mean(y))
    predictions <- transform(changes, p = change + patient$intercept[1])

    patient.new <- patient[(n.observed+1):nrow(patient), ]
    results <- merge(patient.new, predictions, by = "x")

    results
}
