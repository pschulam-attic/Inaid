library(plyr)

interpolate.trajectory <- function(x, y) {
    if (length(x) != length(y)) {
        stop("x and y are different sizes")
    }

    n <- length(x)

    if (length(x) == 1) {
        y
    } else {

        if (x[2] - x[1] == 1) {
            c(y[1], interpolate.trajectory(x[2:n], y[2:n]))
        } else {
            int.y <- approx(x[1:2], y[1:2], xout = x[1]:(x[2]-1))$y
            c(int.y, interpolate.trajectory(x[2:n], y[2:n]))
        }
        
    }
}

prep.patient.data <- function(df, bucket.size = 92) {
    pid <- df$PtID[1]
    
    bucket.df <- arrange(transform(df, bucket = diff %/% bucket.size), bucket)
    bucket.df <- ddply(bucket.df, .(bucket), summarize, fvc = mean(fvc))

    x <- bucket.df$bucket
    y <- bucket.df$fvc

    if (x[1] > 0) {
        y <- c(rep(y[1], x[1]), y)
        x <- c(seq_len(x[1]) - 1, x)
    }

    int.x <- 0:max(x)
    int.y <- interpolate.trajectory(x, y)

    data.frame(PtID = pid, x = int.x, y = int.y)
}

patient.dist <- function(df1, df2) {
    pid1 <- df1$PtID[1]
    pid2 <- df2$PtID[2]
    
    min.length <- min(length(df1$y), length(df2$y))
    y1 <- df1$y[1:min.length]
    y2 <- df2$y[1:min.length]

    d <- (y1 - y2) ** 2
    d <- mean(d)

    data.frame(distance = d)
}
