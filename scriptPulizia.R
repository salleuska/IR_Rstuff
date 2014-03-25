setwd("~/altracartella/IR_Rstuff")
setwd("/home/alan/Documents/GIT/Rstuff")
#-------------------------------------------------------------------#
source("~/altracartella/IR_Rstuff/TipsterFunzioni.R")
# Lettura 
data <- read.table("heidel_date&precision(tutti).txt", header=T)
#-------------------------------------------------------------------#
str(data)
# TIME
data[which(data$type == "TIME"), ]
levels(data$gran) <- c(levels(data$gran), "time")
data$gran[which(data$type == "TIME")]<- "time"
data[which(data$type == "TIME")[1:10], ]

# check anni per errori - FINIRE
anni <-data[which((data$type == "DATE")&(data$gran == "year")), ]
anni <- droplevels(anni)
str(anni)
levels(anni$value)

anni.info <- info(anni$value)

for(i in 1:length(anni.info))
{
  # cat(paste(names(anni.info[i]), anni.info[i]$freq, sep = " "))
  print(paste(names(anni.info[i]) , anni.info[[i]]$freq))
}

anni.info["1973"]

#
prove random
library(lubridate)
day <- ymd(data$value)
length(which(!is.na(day)))
length(which(data$gran =="day") )

daybase <- as.Date(data$value)
length(which(!is.na(daybase)))

data$gran[which(!is.na(day))] <- "day"

