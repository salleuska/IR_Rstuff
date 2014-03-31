# setwd("~/Scrivania/TipsterData")
setwd("/home/alan/Documents/GIT/Rstuff")
#-------------------------------------------------------------------#
source("~/altracartella/IR_Rstuff/TipsterFunzioni.R") # per info()
# Lettura 
data <- read.table("heidel_details&precision.txt", header=T)
#-------------------------------------------------------------------#
# Toglie dati con value nullo (2)
data <- data[-which(data$value == ""), ]
data <- droplevels(data)
str(data)
#--------------------------------------------------------#
# Da controllare (numero di osservazioni)
# Tipo DATE undefined 
dim(data[which((data$type == "DATE")&(data$gran == "undefined")), ])[1]
# Tipo DATE & gran = year 
dim(data[which((data$type == "DATE")&(data$gran == "year")), ])[1]
# Tipo DATE & gran = years 
dim(data[which((data$type == "DATE")&(data$gran == "years")), ])[1]
# Tipo date & gran = day/days/month/months/ref dovrebbero essere a posto
dim(data[which((data$type == "DATE")&(data$gran != "years")&(data$gran != "year")&(data$gran != "undefined")), ])[1]
# Tipo DATE totale
dim(data[which(data$type == "DATE"), ])[1]
#------------------------------------------------------------------#
# Tipo DATE & gran = year #
#-------------------------#
# Creazione di un subset
anno <- data[which((data$type == "DATE")&(data$gran == "year")), ]
anno <- droplevels(anno)
str(anno)
as.integer(levels(anno$value)) 

anno.info <- info(anno)
length(anno.info)
anno.freq <- matrix(NA, length(names(anno.info)), ncol = 2)
for(i in 1:length(names(anno.info)))
{
  anno.freq[i, ] <- c(names(anno.info[i]) , anno.info[[i]]$freq)
}
str(anno.freq)
anno.freq <- as.data.frame(anno.freq)
# frequenze come variabile numerica
anno.freq$V2 <- as.numeric(levels(anno.freq$V2))[anno.freq$V2]
# Identifico quelli con una frequenza minore di 0.0001
anno.drop <-anno.freq[which(anno.freq$V2 < 1e-04), ]$V1
anno.drop <- droplevels(anno.drop)
anno.drop

anno.datogliere <- which(levels(data$value) %in% levels(anno.drop))
oss.datogliere <- which(data$value %in% levels(data$value)[anno.datogliere])

data <- data[-oss.datogliere, ]
data <- droplevels(data)
# check
anno <- data[which((data$type == "DATE")&(data$gran == "year")), ]
anno <- droplevels(anno)
str(anno)
anno.info[levels(anno$value)] 
#-------------------------------------#
# backup
write.table(data, file = "heidel_year.pulito.txt")
# pulizia workspace e rilettura
rm(list = ls())
data <- read.table("heidel_year.pulito.txt", header=T)
source("~/altracartella/IR_Rstuff/TipsterFunzioni.R")
#------------------------------------#
# Tipo DATE undefined 
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# Gestione valori "UNDEF-REF"
dim(undef[grep("UNDEF", undef$value), ])[1]
dim(data[grep("UNDEF", data$value), ])[1]
data[grep("UNDEF", data$value), ]

data <- data[-grep("UNDEF", data$value), ]
data <- droplevels(data)
str(data)
####################
# Gestion possibili giorni
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
levels(undef$value)
library(lubridate)
day.check <- levels(undef$value)[which(!is.na(ymd(levels(undef$value))))]
day.drop <- day.check[which((ymd(day.check) > ymd("2000-01-01"))|(ymd(day.check) < ymd("1300-01-01")))]

# Aggiunta gran = day per i valori mappati giusti
new.day <- day.check[!(day.check %in% day.drop)]
data[which(data$value %in% new.day), ]$gran <- "day"
# Tolgo i valori sbagliati
data <- data[- which(data$value %in% day.drop), ]
data <- droplevels(data)
str(data)
#------------------------------------------------------------#
# backup
write.table(data, file = "heidel_pulizia.tmp.txt")
# pulizia workspace e rilettura
rm(list = ls())
data <- read.table("heidel_pulizia.tmp.txt", header=T)
# serve fare il source del file TipsterFunzioni.R per poter utilizzare la funzione info()
source("~/altracartella/IR_Rstuff/TipsterFunzioni.R")
# source("D:/IR_Rstuff/TipsterFunzioni.R")

#-----------------------------------------------------------------#
# Scrittura diversi file
# Scelta cartella
setwd("/home/sally/Scrivania/TipsterData/prova")
data <- read.table("/home/sally/Scrivania/TipsterData/heidel_pulizia.tmp.txt", header=T)
# Subset solo tipo DATE senza i gran = undefined
dat.sub <- data[which((data$type =="DATE")&(data$gran != "undefined")), ]
dat.sub <- droplevels(dat.sub)
# tolgo la colonna term
dat.sub <- dat.sub[,-4]
str(dat.sub)

prova <- dat.sub[1:100, ]
prova <- droplevels(prova)
str(prova)
write.files <- function(data)
{
  file.name = paste(data$id[1],"_",data$creation[1], ".txt", sep = "") 
  subset.to.write <- data[, c("type", "value", "gran")]
  write.table(subset.to.write, file = file.name,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}
# scrittura
lapply(split(prova, prova$id), function(x) write.files(x))
