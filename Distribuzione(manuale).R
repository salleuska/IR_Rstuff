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

hdr <- function(prob, data)
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
test <- hdr(0.9, data = prova)
stopTimer()

summary(as.Date(test$date))
ndays <- as.integer(max(as.Date(test$date)) - min(as.Date(test$date)))