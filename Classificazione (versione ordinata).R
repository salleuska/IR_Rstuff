#------------------------------------------------------------------#
# source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
# config <- set.config(user = "alan")
source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
config <- set.config(user = "sally")
#config
source(paste(config[1], "FunzioniClassificazione.R", sep = ""))
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))
#------------------------------------------------------------------#
# Caricamento libreria
library(data.table) 

# Lettura dataset pulito
setwd(config[2])
data <- ricarica.dataset("heidel_pulizia.def.txt")

data <- data.table(data)
setkey(data, id, type)
# ---------------------------#
# Creazione dataframe classificazione (id doc + classe + score)
classificazione <- data.frame(cbind(levels(data$id), NA))
colnames(classificazione) <- c("id", "class")
# Tag
classificazione$class <- factor(classificazione$class, levels= c("past", "present", "future", 
                                                                 "day", "days", "month",
                                                                 "months", "year", "years", 
                                                                 "undef", "recurrence", "duration"))

# Colonna rappresentante il punteggio associato alla classificazione
# e.g. frequenza pesata
classificazione$score <- rep(0, dim(classificazione)[1])
#----------------------------------------------------------#
data.names <- levels(data$id)
str(data)
# Elimino le espressioni con granularità undefined di tipo DATE
if(length(which((data$gran == "undefined")&(data$type =="DATE"))) > 0)
{
  data <- droplevels(data[- which((data$gran == "undefined")&(data$type =="DATE")), ])
}
# Documenti che avevano solo espressioni di tipo DATE undefined -> non classificabili

classificazione <- tag(classificazione, id = setdiff(data.names, levels(data$id)),
                       tag = "undef", score = rep(0, length(setdiff(data.names, levels(data$id)))))

stato.classificazione()
# pesi(esempio): DATE = 4, DURATION = 1,SET= 2,  TIME = 0
# per i TIME non classifico
# DURATION < SET
# SET << DATE
# Calcolo tabella frequenze pesate
w <- c(4, 1, 2, 0)
tab <- prop.table.weighted(table(data[, list(id, type)]), w)
# Individuazione tipo di espressione con frequenza pesata massima
max <- max.freq(tab)
#===================================================#
# Individuazione dei ties (DA GESTIRE ANCORA)
ties.type <- ties(max)

if(length(ties.type) > 0)  max <-max[-which(names(max) %in% names(ties.type))]
#===================================================#

max <- list.to.data.frame(max, colnames = c("id", "type"))

class.DATE <- droplevels(max[which(max$type == "DATE"), ])
class.SET <- droplevels(max[which(max$type == "SET"), ])
class.DURATION <- droplevels(max[which(max$type == "DURATION"), ])
# Se rimangono fuori dei documenti, sono quelli con espressioni solo di tipo TIME
class.TIME <- droplevels(max[which((max$type != "DURATION")&(max$type != "SET")&(max$type != "DATE")), ])

# check length(levels(data$id)) - length(ties.type)
# check dim(class.SET)[1] + dim(class.DATE)[1] + dim(class.DURATION)[1] + dim(class.TIME)[1]

classificazione <- tag(classificazione, names(class.TIME$id), "undef", rep(0, length(names(class.TIME$id))))
# Frequenza pesata come score provvisorio
classificazione <- tag(classificazione, names(class.DURATION$id), "duration", class.DURATION$Freq)
classificazione <- tag(classificazione, names(class.SET$id), "recurrence", class.SET$Freq)

stato.classificazione()

# Gestione classficazione traminte DATE
DATE <- droplevels(data[which(data$id %in% unique(class.DATE$id)), ])
DATE <- droplevels(DATE[which(DATE$type == "DATE"), ])
str(DATE) #110072
# Rinomino i livelli per suddivisioni in "ref" e "noref"
levels(DATE$gran)[which(levels(DATE$gran) != "ref")] <- rep("noref")
# PESI
# noref = 1
# ref = 1/4

w.ref <- c(1, 1/4)
tab.DATE <- prop.table.weighted(table(DATE[, list(id, gran)]), w.ref)
#=======================================================================#
# NOTA: aggiustamento frequenze pesate per lo score
# le frequenze di ref e noref in realtà non sommano a 1, ma dovrebbero essere relative
# alla frequenza di tipo DATE calcolata al passo precedente
# e.g. ho scelto di classificare il documento d in base a DATE con freq = 0.8
# il documento d ha frequenze noref = 0.9 e ref = 0.1 (ma all'interno dell'80% dei DATE di d)
# quindi 0.9 : 1 = x : 0.8 --> x = 0.9*0.8 basta la moltiplicazione, anche dopo il calcolo
# della classificazione, per avere lo score "assoluto"
# Prova: controllo che l'ordina venga mantenuto
# cbind(tab.DATE[1:10, ], class.DATE$Freq[1:10 ])
# tab.DATE[1:10, ]*class.DATE$Freq[1:10]

tab.DATE <- tab.DATE*class.DATE$Freq

# Individuo la categoria massima 
max.DATE <- max.freq(tab.DATE)

#===================================================#
# Individuazione dei ties (DA GESTIRE ANCORA.. intanto vengono tolti)
ties.DATE <- ties(max.DATE)

if(length(ties.DATE) > 0)  max.DATE <-max.DATE[-which(names(max.DATE) %in% names(ties.DATE))]
#===================================================#
# Documenti da classicare tramite REF

max.DATE <- list.to.data.frame(max.DATE, c("id" , "gran"))
# Documenti da classificare tramite REF
class.REF <- droplevels(max.DATE[which(max.DATE$gran == "ref"), ])
# Documenti da classificare tramite gli intervalli
class.noREF <- droplevels(max.DATE[which(max.DATE$gran == "noref"), ])

# check dim(class.REF)[1] + dim(class.noREF)[1]
#---------------------------------#
# Classificazione in base ai ref
REF <- droplevels(data[which((data$id %in%levels(class.REF$id))&(data$gran == "ref")), ])

tab.ref <- prop.table(table(REF[, list(id, value)]), margin = 1)

#===================================================#
# Aggiornamento dello score? 
# tab.DATE <- tab.DATE*class.DATE$Freq
#===================================================#

max.ref <- max.freq(tab.ref)
#===================================================#
# Individuazione dei ties (DA GESTIRE ANCORA.. intanto vengono tolti)
ties.ref <- ties(max.ref)
# 272 + 8 (ce ne sono anche con 3)
if(length(ties.ref) > 0)  max.ref <-max.ref[-which(names(max.ref) %in% names(ties.ref))]
#===================================================#
max.ref<- list.to.data.frame(max.ref, c("id", "ref" ))

class.PAST <- droplevels(max.ref[which(max.ref$ref == "PAST_REF"), ])
class.PRESENT <- droplevels(max.ref[which(max.ref$ref == "PRESENT_REF"), ])
class.FUTURE <- droplevels(max.ref[which(max.ref$ref == "FUTURE_REF"), ])

# check dim(max.ref)[1]
# dim(class.PAST)[1] + dim(class.PRESENT)[1] + dim(class.FUTURE)[1]

classificazione <- tag(classificazione, levels(class.PAST$id), "past", class.PAST$Freq)
classificazione <- tag(classificazione, levels(class.PRESENT$id), "present", class.PRESENT$Freq)
classificazione <- tag(classificazione, levels(class.FUTURE$id), "future", class.FUTURE$Freq)

stato.classificazione()

# Check NA (Da classificare)
# length(ties.type) + length(ties.DATE) + length(ties.ref) + dim(class.noREF)[1]

#---------------------------------------------------------------#
# Classificazione per intervalli
# parto da levels(class.noREF$id) e recupero gli intervalli associati

load("/home/sally/Documents/results90.RData")

to.class <- results[which(names(results) %in% levels(class.noREF$id))]
length(to.class)
#check length(levels(class.noREF$id))

# Individuo i documenti con un solo DATE di tipo day
# Sono quelli che hanno un valore NULL 
class.oneDATE <- names(to.class[sapply(sapply(to.class,  "[[", 3), function(x) length(x) == 0)])
#=================================================#
# DA definire la penalizzione dello score, in quanto la stima è fatta su un'unica espressione
# di tipo DATE
# (per ora tengo la frequenza pesata di base)
score.oneDATE <- class.noREF[which(class.noREF$id %in% class.oneDATE), ]$Freq

#=================================================#
classificazione <- tag(classificazione, id= class.oneDATE, tag= "day", score = score.oneDATE)
stato.classificazione()

if(length(which(names(to.class) %in% class.oneDATE)) > 0)
{
  to.class <- to.class[-which(names(to.class) %in% class.oneDATE)]
}
# check length(to.class) + length(ties.type) + length(ties.DATE) + length(ties.ref)

# Quelli che hanno uno 0 per la probabilità, sono sempre di tipo day, ma  hanno più di una data
# (ma non abbastanza per raggiungere la probabilità desiderata nel calcolo degli intervalli)
class.day <- names(to.class[sapply(sapply(to.class,  "[[", 3), function(x) x == 0)])
#=================================================#
# DA definire lo score in base al numero di espressioni
score.day <- class.noREF[which(class.noREF$id %in% class.day), ]$Freq
#=================================================#
classificazione <- tag(classificazione, id= class.day, tag= "day", score = score.day)


if(length(which(names(to.class) %in% class.day)) > 0)
{
  to.class <- to.class[-which(names(to.class) %in% class.day)]
}

# check length(to.class) + length(ties.type) + length(ties.DATE) + length(ties.ref)
stato.classificazione()

# Calcolo ampiezze intervalli
# prova <- to.class[1:100]
# prova[[1]]$interval <- as.numeric(prova[[1]]$upper - prova[[1]]$lower) + 1
#=============================================================#
tagfrominterval <- function(x)
{
  if(x == 0) x = "day"
  else if((x > 0)&(x <= 28)) x = "days"
  else if((x > 28)&(x <= 31)) x = "month"
  else if((x > 31)&(x <= 360)) x = "months"
  else if((x > 360)&(x <= round(365*1.5))) x = "year"
  else if(x > round(365*1.5)) x = "years"
  # else if((x > 29)&(x <= 31)) x = "decade"
  # else if((x > 29)&(x <= 31)) x = "decades"
  # else if((x > 29)&(x <= 31)) x = "century"
}

add <- function(list) 
{
  list$interval <- as.numeric(list$upper - list$lower)
  if(length(list$interval) == 1)
  {
    list$tag <- tagfrominterval(list$interval)
  }
  else
  {
    list$tag <- unique(sapply(list$interval, tagfrominterval))
    # (uso di unique dalle considerazioni in fondo alla script)

  }
  
  list
}

library(plyr)

prova <-llply(to.class[1:100], function(x) add(x))
prova

startTimer()
to.class <-llply(to.class, function(x) add(x))
stopTimer()


# Aggiungere a classificazione (bisognerà ripetere le righe id)
l <- sapply(sapply(to.class,  "[[", 5), function(x) length(x))
summary(l)


#=============================================================#
# Considerazioni (implementate poi nelle funzioni sopra)
# Se l'intervallo è uno solo la classificazione è facile
lengths <- sapply(sapply(to.class,  "[[", 4), function(x) length(x))
summary(lengths)

# Documento  con il massimo numero di intervalli (50)
names(which.max(lengths))

as.data.frame(data[which(data$id %in% names(which.max(lengths))), ])
# check distribuzione
setwd(paste(config[3], "DimSplitted", sep = ""))
check <- names(which.max(lengths))
bo <- read.delim(paste(check, "_txt", sep = ""), header = F)
bo$V2 <- as.Date(bo$V2)
hist(bo$V2, breaks = "day")
# Un taglio più alto sarebbe stato più utile forse


to.class[names(which.max(lengths))]
# si potrebbero cmq usare gli intervalli "unici" 
unique(to.class[names(which.max(lengths))][[1]]$interval)
# avremmo ad esempio day/days/year/years ()
