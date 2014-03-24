#setwd("~/Scrivania/esiti 1992")
setwd("/home/alan/Documents/GIT/Rstuff")
data <- read.delim("heidel_dimensioneTemporale.txt", header=F)
#colnames(data) <- c("id", "days")
colnames(data) <- c("id", "begin", "end")

str(data)


# check
levels(data$begin)

summary(data)


#prove varie

data[2,2]

summary(data[2])
plot(data[1])

max(as.Date(data[2]))

as.Date(data[2])


type.per.doc <- tapply(data$id,data$begin, summary, maxsum = Inf)
str(type.per.doc)
# DATE per documento 
date <- type.per.doc[[1]]
date <- data.frame(date)
str(date)

summary(date)





