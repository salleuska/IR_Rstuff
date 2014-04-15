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

load("/home/sally/Documents/results.RData")

str(results) # lista di liste
# lista relativa ad un documento
results[1]
# accedere ad un elemento della lista con [[]]
results[[3]]
results[[3]]$lower
results[[3]]$upper

# Calcolo ampiezza intervallo/i
results[[3]]$upper - results[[3]]$lower

# Controllo della probabilità associata agli intervalli 
for(i in 1:100)
  print(results[[i]]$prob)

# documenti in cui non è stata raggiunta la probabilità di circa 0.90
# dovrebbero essere quelli con poche date
# mentre quelli con NULL hanno solo una data

# Esempi
setwd("/home/sally/Documents/Dimensione_temporale_divisi")
docs <- list.files()

results[1]
data <- read.delim(docs[1], header = F, col.names = c("id", "day"))
data

# mentre quelli con NULL hanno solo una data
results[2]
data <- read.delim(docs[2], header = F, col.names = c("id", "day"))
data

#------------------------------------------------------------------#
# Voglio vedere quanti e quali documenti con intervalli di densità di probabilità minori di 0.8 
# perchè saranno quelli con un numero basso di date

# Estraggo le densità di probabilità dall'intera lista
densities <- lapply(results, "[[", 3)
# recupero i documenti per i quali gli intervalli hanno densità minore di quella voluta
docsToClass <- results[names(which(unlist(densities) < 0.8))]
length(docsToClass)
head(docsToClass)
# solo nomi dei documenti
head(names(docsToClass))

to.check <- as.list(paste(names(docsToClass), "_txt", sep = ""))
library(plyr)
# calcolo il numero di date dopo l'espansione per i documenti individuati
ndate <- llply(to.check, .fun = function(x) length(readLines(x)))
names(ndate) <- names(docsToClass)
plot(unlist(ndate), type = "h")
summary(unlist(ndate)) 
#il 75% circa ha un numero di date tra 2 e 3.. andranno gestiti prima dell'espansione

head(ndate)
ndate[3]
data <- read.delim(as.character(to.check[3]), header = F)
data

sort(unlist(ndate), decreasing = T)[1:50]
names(sort(unlist(ndate), decreasing = T))[1:50]
results$"WSJ891019-0166"
data <- read.delim("WSJ891019-0166_txt", header = F)
data
# tante ripetizioni di due date
hist(as.Date(data$V2), breaks = "day")

# quindi sarebbero da tenere buoni cmq i documenti con più di 2/3 date..
# In effetti basta gestirli prima
#------------------------------------------------------------#
# Supponendo di avere tutti intervalli "Buoni"
# calcolo delle ampiezze (in giorni)

library(plyr)

# ampiezze intervalli (prova)
for(i in 1:10)
  print(as.numeric(results[[i]]$upper - results[[i]]$lower))

  
  

