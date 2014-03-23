#setwd("~/Scrivania/esiti 1992")
setwd("/home/alan/Documents/GIT/Rstuff")
data <- read.delim("heidel_datePrecision.txt", header=F)
colnames(data) <- c("id", "value", "prec")

str(data)


# check
levels(data$prec)

summary(data)

# Percentuali tipo di espressioni
summary(data$prec)/length(data$prec)


# PREC per documento
prec.per.doc <- tapply(data$id, data$prec, summary, maxsum = Inf)
str(prec.per.doc) 
date <- prec.per.doc[[1]]
date <- data.frame(date)
str(date)

summary(date)

# RICERCA UNDEFINED
undef <- subset(data, data$prec == "undefined")
summary(undef)



