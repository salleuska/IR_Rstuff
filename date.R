#setwd("~/Scrivania/esiti 1992")
setwd("/media/DATA/Ubuntu/Rstuff")
myData <- read.delim("heidel_date.txt", header=F)
colnames(myData) <- c("id", "term")

str(myData)


# check
levels(myData$term)

summary(myData)

# Percentuali tipo di espressioni
summary(myData$term)/length(myData$term)




# FREQUENZA 

term.per.doc <- tapply(myData$id, myData$term, summary, maxsum = Inf)
str(term.per.doc)
# TERM per documento 
date <- term.per.doc[[1]]
date <- myData.frame(date)
str(date)

summary(date)