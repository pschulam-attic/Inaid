# features:
#   - PtID
#   - diff
#   - fvc
#   - strFemale
#   - strRaceId1
#   - ethnic
#   - smoke
#   - age

library(plyr)

restrict_df <- function(test_df, train_df) {
    female <- test_df$strFemale[1]
    race <- test_df$strRaceId1[1]
    smoke <- test_df$smoke[1]
    subset(train_df, strFemale == female & strRaceId1 == race & smoke == smoke)
}

patient_sim <- function(p1_df, p2_df) {
    ss <- 0
    c <- 0

    for (i in seq(nrow(p1_df))) {
        r <- p1_df[1, ]
        if (has_bucket(p2_df, r$bucket)) {
            d <- subset(p2_df, bucket == r$bucket)
            c <- c + 1
            ss <- ss + (r$fvc - mean(d$fvc)) ** 2
        }
    }

    if (c == 0)
        return(Inf)
    else
        return(ss / c)
}

add_buckets <- function(df, bucket_size) {
    transform(df, bucket = diff %/% bucket_size)
}

has_bucket <- function(df, b) {
    b %in% df$bucket
}

knn_predict <- function(test_df, train_df, k = 5) {
    test <- arrange(test_df, diff)
    test <- test[1:4, ]

    test_out <- test[5, ]

    train <- train_df

    similarity_to <- function(df) {
        similarity <- patient_sim(test, df)
        hasbucket <- has_bucket(df, test_out$bucket)
        data.frame(similarity = similarity, hasbucket = hasbucket)
    }

    sim <- ddply(train, .(PtID), similarity_to)
    sim <- subset(sim, hasbucket == TRUE)
    sim <- arrange(sim, similarity)
    sim[1:k, ]
}

read_data <- function(filename) {
    data <- arrange(read.csv(filename), PtID, diff)
    data <- subset(data, select = c(PtID, diff, fvc, strFemale, strRaceId1, smoke, age, aca))

    data$PtID <- as.factor(data$PtID)
    data$strFemale <- as.factor(data$strFemale)
    data$strRaceId1 <- as.factor(data$strRaceId1)
    data$smoke <- as.factor(data$smoke)

    return(data)
}

main <- function(bucket_size = 365) {
    data <- read_data("train.csv")
    data <- add_buckets(data, bucket_size)

    
}

main <- function() {
    bucket_size <- 365

    data <- arrange(read.csv("train.csv"), PtID, diff)
    data <- subset(data, select = c(PtID, diff, fvc, strFemale, strRaceId1, ethnic, smoke, age))
    data <- add_buckets(data, bucket_size)

    data <- within(data, {
        PtID <- as.factor(PtID)
        strFemale <- as.factor(strFemale)
        strRaceId1 <- as.factor(strRaceId1)
        ethnic <- as.factor(ethnic)
        smoke <- as.factor(smoke)
    })

    patient_ids <- unique(data$PtID)

    results <- data.frame()
    k <- 5

    i <- 0

    for (pid in patient_ids) {
        if (i > 5)
            break

        i <- i + 1
        
        message(sprintf("Predicting patient %s", pid))
        test <- subset(data, PtID == pid)
        train <- subset(data, PtID != pid)

        r <- data.frame(knn_predict(test, train, k))
        results <- rbind(results, r)
    }

    results
}
