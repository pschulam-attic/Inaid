gower.distance <- function(df1, df2) {
    x1 <- df1$x
    x2 <- df2$x
    common <- intersect(x1, x2)

    d <- 0
    for (xc in common) {
        y1 <- subset(df1, x == xc)
        y2 <- subset(df2, x == xc)

        d <- d + (y1 - y2) ** 2
    }

    d / length(common)
}


