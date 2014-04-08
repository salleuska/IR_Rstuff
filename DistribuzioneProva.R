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

#-------------------------------------------------------------------#
# PROVA (si può ignorare)
# Gestion date in R-base (rappresentazione di tutto il dataset - prove)
str(dim.temp)
summary(dim.temp$day)
hist(dim.temp$day, breaks = "year")
lines(density(as.numeric(dim.temp$day)), col = 2)
density(as.numeric(dim.temp$day))

# install.packages("coda") 
library(coda)
int <- HPDinterval(as.mcmc(dim.temp$day), prob=0.9)
int

abline(v= int[1], col = 2)
abline(v= int[2], col = 2)

#-------------------------------------------------------------------------#
# Check numero di documenti (non è detto che tutti abbiamo almeno una data)
length(unique(dim.temp$id))
#-------------------------------------------------------------------------#
# Prova con primo documento
prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[1]), ]
prova <- droplevels(prova)
summary(prova$day)
# Nota: si può calcolare l'ampiezza di un intervallo di tempo
max(prova$day) - min(prova$day)

# Distribuzione
hist(prova$day, breaks = "day")

# install.packages("coda") 
library(coda)
hist(as.mcmc(prova$day), breaks = 1000, prob = T)
# Calcolo dell'intervallo HPD
int <- HPDinterval(as.mcmc(prova$day), prob=0.9)
int
abline(v= int[1], col = 2)
abline(v= int[2], col = 2)

# Check per mappatura (ok)
as.integer(unique(prova$day))
as.mcmc(unique(prova$day))
cbind(as.integer(unique(prova$day)), levels(as.factor(prova$day)))

# dovremmo riavere due date (ok)
unique(prova$day[which(as.mcmc(prova$day) == int[1])])
unique(prova$day[which(as.mcmc(prova$day) == int[2])])


## Prova con secondo documento
prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[2]), ]
prova <- droplevels(prova)
summary(prova$day)

max(prova$day) - min(prova$day)

# Distribuzione
hist(prova$day, breaks = "day")

int <- HPDinterval(as.mcmc(prova$day), prob=0.5)
int
abline(v= int[1], col = 2)
abline(v= int[2], col = 2)

##################################
# terzo documento (bimodale)
prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[89]), ]
prova <- droplevels(prova)

# Distribuzione
hist(prova$day, breaks = "day")

# hist(as.mcmc(prova$day), prob = T)
int <- HPDinterval(as.mcmc(prova$day), prob=0.9)
int
abline(v= int[1], col = 2)
abline(v= int[2], col = 2)
# NON FA QUELLO CHE DOVREBBE
# HPDinterval assume che si abbia a che fare con una distribuzione unimodale -.- grr
#-----------------------------------------------------------------------------------#
# prova con altra libreria
# non presente nei repository
# scaricare a http://cran.r-project.org/src/contrib/Archive/LaplacesDemon/
# e installare da terminale
# R CMD INSTALL LaplacesDemon_13.03.04.tar.gz 

library(LaplacesDemon)
int <- p.interval(as.numeric(prova$day), MM = T, prob = 0.5)
int
abline(v= int[1], col = 2)
abline(v= int[2], col = 2)

p.interval(as.numeric(prova$day), MM = T, prob = 0.5, plot = T)

# Sembra funzionare solo che gli intervalli mulitmodali me li stampa e basta
# però ha una funzione che identifica se la distribuzione ha più di una moda
is.multimodal(as.numeric(prova$day))

#--------------------------------------------------------------------------------#
# install.packages("hdrcde")
library(hdrcde)

prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[3]), ]
prova <- droplevels(prova)

# Distribuzione
hist(prova$day, breaks = "day")

dd <- density(as.numeric(prova$day)) # da vedere come stimare (di default usa una mistura di normali)
lines(dd, col = 2)
hdr <-   hdrconf(as.numeric(prova$day), list(x = dd$x, y = dd$y), prob = 0.90, conf = 0.90)
hdr
abline(v = hdr$hdr[1], col = 3)
abline(v = hdr$hdr[2], col = 3)
abline(v = hdr$hdr[3], col = 3)
abline(v = hdr$hdr[4], col = 3)


prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[12]), ]
prova <- droplevels(prova)
hist(prova$day, breaks = "day")
dd <- density(as.numeric(prova$day)) # da vedere come stimare (di default usa una mistura di normali)
lines(dd, col = 2)
hdr <-   hdrconf(as.numeric(prova$day), list(x = dd$x, y = dd$y), prob= 0.90)
hdr
abline(v = hdr$hdr[1], col = 3)
abline(v = hdr$hdr[2], col = 3)
abline(v = hdr$hdr[3], col = 3)
abline(v = hdr$hdr[4], col = 3)


# Prova "a mano"
# considero la distribuzione discreta empirica

prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[17]), ]
prova <- droplevels(prova)

tb <- table(prova$day)/length(prova$day)
plot(tb, type = "h")

plot(sort(tb, decreasing= T), type = "l", ylim = c(0, max(sort(tb))))

hdr <- function(prob, data)
{
  freq <- table(data$day)/length(data$day)
  freq.ord <- sort(freq, decreasing= T)
  cum.freq <- as.numeric(tb.ord[1])
  date <- names(frew.ord)[1]
  while(sum(cum.freq) < prob)
  {
    i <- length(cum.freq)
    cum.freq <- c(cum.freq, freq.ord[i+ 1])
    date <- c(date, names(freq.ord)[i +1])
  }
  list(date = date, prob = sum(cum.freq))
}

startTimer()
ba <- hdr(0.9, data = prova)
stopTimer()

summary(as.Date(ba$date))
max(as.Date(ba$date)) - min(as.Date(ba$date))
