library(plyr)

predict.prognosis <- function(df) {
    fit <- lm(fvc ~ year, data=df)
    coef(fit)[2] < 0
}

data <- arrange(read.csv("../../Data/fvc.5.visits.csv"), PtID, visit)

results <- ddply(data, .(PtID), summarize, prognosis=fvc[1] > fvc[5])
predictions <- by(data, factor(data$PtID), predict.prognosis)
results$prediction <- unlist(predictions)
