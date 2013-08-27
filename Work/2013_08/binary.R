library(plyr)

predict.prognosis <- function(df) {
    fit <- lm(fvc ~ year, data=df)
    coef(fit)[2] < 0
}

data <- arrange(read.csv("../../Data/fvc.5.visits.csv"), PtID, visit)

qplot(seq.diff, data = data)

results <- ddply(data, .(PtID), summarize, prognosis=fvc[1] > fvc[5])
predictions <- by(data, factor(data$PtID), predict.prognosis)
results$prediction <- unlist(predictions)

missed_det <- subset(results, prognosis == TRUE, prediction == FALSE)
subset(data, PtID == 20)
patient_20 <- subset(data, PtID == 20)
plot(patient_20$year, patient_20$fvc)
plot(year, fvc, data = patient_20)
lines(ksmooth(patient_20$year, patient_20$fvc, bandwidth = 5))

with(patient_20, {
    plot(year, fvc)
    lines(ksmooth(year, fvc, "normal", bandwidth = 2))
})

with(subset(data, PtID == 33), {
    plot(diff, fvc)
    lines(ksmooth(year, fvc, "normal", bandwidth = 4), col = 2)
})