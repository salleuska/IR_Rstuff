# Prova con i dati del 1992
dim.temp<- read.delim("~/Scrivania/heidel_dimensioneTemporale (id+date).txt", header=F)
colnames(dim.temp) <- c("id", "day")
# Gestion date in R-base
dim.temp$day <- as.Date(dim.temp$day)
str(dim.temp)
summary(dim.temp$day)
hist(dim.temp$day, breaks = "year")

# Numero di documenti
length(unique(dim.temp$id))

# Prova con primo documento
prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[1]), ]
prova <- droplevels(prova)
summary(prova$day)
hist(prova$day, breaks = "months")

# Libreria "lubridate" per la gestione delle date
# install.packages("lubridate")
library("lubridate")
vignette("lubridate")
prova$day <- ymd(prova$day)
round_date(summary(prova$day), "day")

library(ggplot2)

qplot(prova$day, 0, data = prova)

#------------------------------------#
prova2 <-  dim.temp[which(dim.temp$id ==levels(dim.temp$id)[1:10]), ]
prova2 <- droplevels(prova2)
summary(prova2$day)
qplot(prova2$day, 0, data = prova2, col = prova2$id)