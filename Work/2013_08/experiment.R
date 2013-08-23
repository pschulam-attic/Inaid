library(plyr)

source("R/preprocess.R")

predict.prognosis <- function(df) {
    fit <- lm(fvc ~ year, data=df)
    coef(fit)[2] < 0
}

data <- read.orig()

results <- ddply(data, .(PtID), summarize, prognosis=fvc[1] > fvc[5])
predictions <- by(data, factor(data$PtID), predict.prognosis)
results$prediction <- unlist(predictions)