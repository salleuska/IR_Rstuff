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

hdr <- function(data)
{
  prob = 0.1
  freq.ord <- sort(table(data$day)/length(data$day), decreasing= F)
  cum.freq <- as.numeric(freq.ord[1])
  while(sum(cum.freq) < prob)
  {
    i <- length(cum.freq)
    cum.freq <- c(cum.freq, freq.ord[i+ 1])
    
  }
  date <- sort(as.Date(names(freq.ord)[-c(1:length(cum.freq))]))
  estremi.lo <- c(min(date), date[which(diff.Date(date) != 1) + 1])
  estremi.up <- c(date[which(diff.Date(date) !=1)], max(date))
  
  list(lower = estremi.lo, upper = estremi.up, prob = 1-sum(cum.freq))
}
#--------------------------------------------------------------------------#
intervalli <- function(fileName)
{
  data <- read.delim(fileName, header = F, col.names = c("id", "day"))
  data$day <- as.Date(data$day)
  cat(fileName, "\n")
  # il calcolo viene eseguito solo per quei documenti con più di una data (per quelli ritorna NULL)
  if(dim(data)[1] > 1) hdr(data)
  
}
#--------------------------------------------------------------------------#
# setwd(paste(config[2], "DimSplitted", sep = ""))
# setwd("/home/sally/Documents/Dimensione_temporale_divisi")
docs <- list.files()
docs <- as.list(list.files())
names(docs) <- sub("_txt", "", docs)
# calcolo
# la funzione llply applica una funzione ad una lista di oggetti e restituisce i risultati
# in una lista 
library(plyr)
results <- llply(.data = docs, .fun = function(x) intervalli(as.character(x) ))
stopTimer()
setwd(config[2])
save(results, file = "results80.RData")