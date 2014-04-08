# Rimane questo blocchetto di codice da controllare all'inzio dello script,
# non ho trovato niente di meglio (o meglio non volevo perderci troppo tempo)
# NOTA: getwd() restituisce la working directory corrente
#------------------------------------------------------------------#
# source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
# config <- set.config(user = "alan")
source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
config <- set.config(user = "sally")
config
#------------------------------------------------------------------#
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))
setwd(config[3])

# Nota: ho usato una parte dei dati (generata da 1000 documenti)
dim.temp <- read.delim("heidel_dimensioneTemporale.txt", header = F)
colnames(dim.temp) <- c("id", "day")
str(dim.temp)
dim.temp$day <- as.Date(dim.temp$day)

# Prova "a mano"
# considero la distribuzione discreta empirica

prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[13]), ]
prova <- droplevels(prova)

tb <- table(prova$day)/length(prova$day)
plot(tb, type = "h")
plot(sort(tb, decreasing= T), type = "h", ylim = c(0, max(sort(tb))))

# La seguente funzione:
# calcola le probabilità empiriche (frequenze/tot) e le ordina in maniera descrescente;
# le somma fino a quando non viene raggiunta la proporzione di densità di probabilità
# voluta, e restituisce la lista delle date associate 
hdr0 <- function(prob = 0.95, data)
{
  freq <- table(data$day)/length(data$day)
  freq.ord <- sort(freq, decreasing= T)
  cum.freq <- as.numeric(freq.ord[1])
  date <- names(freq.ord)[1]
  while(sum(cum.freq) < prob)
  {
    i <- length(cum.freq)
    cum.freq <- c(cum.freq, freq.ord[i+ 1])
    date <- c(date, names(freq.ord)[i +1])
  }
  list(date = date, prob = sum(cum.freq))
}

startTimer()
test <- hdr0(0.7, data = prova)
stopTimer()

plot(as.Date(test$date), type = "p")

summary(as.Date(test$date))
max(as.Date(test$date)) - min(as.Date(test$date))


#-------------------------------------------------------------#
# modifiche per applicazione
hdr <- function(prob = 0.9, data)
{
  out <- numeric(3)
  freq <- table(data$day)/length(data$day)
  freq.ord <- sort(freq, decreasing= T)
  cum.freq <- as.numeric(freq.ord[1])
  date <- as.Date(names(freq.ord)[1])
  while(sum(cum.freq) < prob)
  {
    i <- length(cum.freq)
    cum.freq <- c(cum.freq, freq.ord[i+ 1])
    date <- c(date, as.Date(names(freq.ord)[i +1]))
  }
  out <- c(sum(cum.freq), as.numeric(max(date) - min(date)))
  
}
test <- hdr(0.9, data = prova)

str(test)

library(plyr)
startTimer()
test <- daply(droplevels(dim.temp[1:40000, ]), "id", function(x) hdr(data = x))
stopTimer()

as.data.frame(test)

# NOTA: bisogna identificare i possibili buchi.. pper ora l'intervallo restituito è solo la
# differenza tra minimo è massimo.. Potrebbe bastare tenere traccia degli indici..

# Eventualmente 
library(doParallel)

# registerDoParallel()
# si può scegliere il numero di copie di R che girano in parallelo con makeCluster
cl <- makeCluster(4)
registerDoParallel(cl)

prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[13]), ]
prova <- droplevels(prova)
