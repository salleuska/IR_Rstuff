setwd("~/Scrivania/TipsterData/")
setwd("/home/alan/Documents/GIT/Rstuff")
#-------------------------------------------------------------------#
# Lettura 

data <- read.delim("heidel_details.txt", header=F)
# Tutti i dati
# data <- read.delim("esiti tutti/heidel_details.txt", header=F)
colnames(data) <- c("id", "type", "value", "term", "creation")
str(data)
#-------------------------------------------------------------------#
data$gran <- as.factor(data$value)
str(data)
# TIME
data[which(data$type == "TIME"), ]
levels(data$gran)[which(data$type == "TIME")] <- "time"

data[which(data$type == "TIME"), ]
library(lubridate)
day <- ymd(data$value)


data$gran[which(!is.na(day))] <- "day"

