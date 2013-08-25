library(ggplot2)
library(plyr)

data <- read.csv("../../Data/fvc.5.visits.csv")

qplot(year, data = data, geom = "density")
qplot(nvisits, data = ddply(data, .(PtID), summarize, nvisits = length(visit)))
qplot(fvc, data = subset(data, visit == 1), geom = "density")
qplot(fvc, data = subset(data, visit == 2), geom = "density")
