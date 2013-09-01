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

diff_mod <- 92

restrict_df <- function(test_df, train_df) {
    female <- test_df$strFemale[1]
    race <- test_df$strRaceId1[1]
    smoke <- test_df$smoke[1]
    subset(train_df, strFemale == female & strRaceId1 == race & smoke == smoke)
}

compute_similarity <- function(test_df, train_df) {

    patient_sim <- function(df) {
        buckets <- test_df$bucket

        ss <- 0
        c <- 0

        for (i in seq(nrow(test_df))) {
            row <- test_df[i, ]
            d <- subset(df, bucket == row$bucket)
            if (nrow(d) == 0)
                next
            ss <- (row$fvc - mean(d$fvc)) ** 2
            c <- c + 1
        }

        if (c == 0)
            return(Inf)
        else
            return(ss / c)
    }

    train_df <- ddply(train_df, .(PtID), patient_sim)
    arrange(train_df, V1)
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

has_bucket <- function(df, b) {
    ddply(df, .(PtID), summarize, hasbucket = any(b == bucket))$hasbucket
}

knn_predict <- function(test_df, train_df, k = 5) {
    test <- transform(test_df, bucket = diff %/% diff_mod)
    test <- arrange(test, diff)
    test <- test[1:4, ]

    test_out <- test[5, ]

    train <- transform(train_df, bucket = diff %/% diff_mod)

    sim <- compute_similarity(test, train)
    sim <- sim[has_bucket(train, test_out$bucket), ]
    sim <- sim[1:k, ]$PtID

    k_nearest <- subset(train, PtID %in% sim)
}



data <- arrange(read.csv("train.csv"), PtID, diff)
data <- subset(data, select = c(PtID, diff, fvc, strFemale, strRaceId1, ethnic, smoke, age))
data <- transform(data, bucket = diff %/% diff_mod)

data <- within(data, {
    PtID <- as.factor(PtID)
    strFemale <- as.factor(strFemale)
    strRaceId1 <- as.factor(strRaceId1)
    ethnic <- as.factor(ethnic)
    smoke <- as.factor(smoke)
})


