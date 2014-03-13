#setwd("~/Scrivania/esiti 1992")
data.raw <- read.delim("heidel_details.txt", header=F)
summary(data.raw)
# Mantengo una copia dei dati grezzi (sai che fai casini)
data <- data.raw
colnames(data) <- c("id", "type", "value", "term", "creation")

str(data)

# numero documenti 
length(unique(data$id))
# check
levels(data$type)

str(data)
summary(data)

# Percentuali tipo di espressioni
summary(data$type)/length(data$type)

# barplot orribile

plot(data$type)

# distribuzione espressioni(tipo) per documento
# distr termini(importanti) per tipo di espressioni
# valori per documenti??

# distribuzione tipi espressione per documento
# funzione 
typefor <- function(tipo, valore)
{
  par(mfrow = c(2, 2))
  #e.g. per ogni documento voglio il numero di espressioni per tipo
  # tipo = data$id, valore = data$type
  tipo.per.valore <- tapply(tipo, valore, summary, maxsum = Inf)
  for(i in 1:length(unique(valore)))
  {
    d <- data.frame(tipo.per.valore[[i]])

    cat("Distribuzione nei documenti", levels(data$type)[i], "\n")
    print(summary(d))
    cat("******************* \n")
    
    #hist(d$tip, breaks = 100, prob = T, main= paste("distribution", unique(data$type)[i]))
    
  }
}

cat("Statistiche", levels(data$type)[1])


typefor(tipo= data$value, data$type)
#---------------------------------------------------#
# Prima a mano
type.per.doc <- tapply(data$id, data$type, summary, maxsum = Inf)
str(type.per.doc)
# DATE per documento 
date <- type.per.doc[[1]]
date <- data.frame(date)
str(date)

summary(date)
# un doc con 271 espressioni 
rownames(date)[which(date == 271)]
data[data$id == rownames(date)[which(date == 271)], ]
summary(date[-which(date == 271), ])
# valori
sort(as.vector(date$date))
# Distribuzione numero di date per documento
hist(date$typ, breaks = 200, prob = T, main= paste("distribution", unique(data$type)[1]))


# DURATION per documento
duration <- type.per.doc[[2]]
duration <- data.frame(duration)
head(duration)
length(which(duration$duration != 0))

summary(duration)

# Documento con 63 espressioni
rownames(duration)[which(duration == 63)]
data[data$id == rownames(duration)[which(duration != 0)], ]
length(unique(rownames(duration)[which(duration != 0)]))



#boxplot
hist(duration$duration, prob = T, breaks = 100)

type.per <- tapply(data$value, data$type, summary, maxsum = Inf)
str(type.per)
sort(type.per[[4]])

# ID DA MAPPARE A INT
# recode library(car)
library(gdata)
id.map <- mapLevels(data$id)
