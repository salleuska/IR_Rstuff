#setwd("~/Scrivania/esiti 1992")
setwd("/media/DATA/Ubuntu/Rstuff")
data <- read.delim("heidel_annoEsatto.txt", header=F)
colnames(data) <- c("id", "value", "frase")

str(data)


# check
levels(data$value)

summary(data)

# Percentuali tipo di espressioni
summary(data$id)/length(data$id)



as.integer(levels(data$value))

as.integer(summary(data$value))







