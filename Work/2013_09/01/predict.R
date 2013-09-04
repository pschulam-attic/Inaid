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
