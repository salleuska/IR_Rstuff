#setwd("~/Scrivania/esiti 1992")
setwd("/media/DATA/Ubuntu/Rstuff")
data <- read.delim("heidel_check100.txt", header=F)
colnames(data) <- c("value", "term")

str(data)


# check
levels(data$term)

summary(data)

# Percentuali tipo di espressioni
summary(data$term)/length(data$term)


## elimino termini con century o centuries
noCentury <- data[regexpr("Century", data$term, ignore.case=TRUE) == -1, ]
noCentury
noCentury <- data[regexpr("Centuries", noCentury$term, ignore.case=TRUE) == -1, ]
noCentury

