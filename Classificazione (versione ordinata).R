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
str(data)
data <- data.table(data)
setkey(data, id, type)
#----------------------------------------------------------#
data.names <- levels(data$id)

# Elimino le espressioni con granularità undefined di tipo DATE
if(length(which((data$gran == "undefined")&(data$type =="DATE"))) > 0)
{
  data <- droplevels(data[-which((data$gran == "undefined")&(data$type =="DATE")), ])
}
# Documenti che avevano solo espressioni di tipo DATE undefined -> non classificabili
# Creazione dataframe classificazione
classificazione <- tag(NULL, id = setdiff(data.names, levels(data$id)),
                        tag = "undef", score = rep(0, length(setdiff(data.names, levels(data$id)))))
colnames(classificazione) <- c("id", "class", "score")
classificazione$class <- factor(classificazione$class, levels = c("undef", "duration", "recurrence",
                                                                  "past","present", "future", 
                                                                  "day", "days", "month", 
                                                                  "months", "year", "years", 
                                                                  "decade", "decades", "century",
                                                                  "centuries"))

data.names <- levels(data$id)
if(length(which((data$gran == "undefined")&(data$type =="TIME"))) > 0)
{
  data <- droplevels(data[-which((data$gran == "undefined")&(data$type =="TIME")), ])
}
classificazione <- tag(classificazione, setdiff(data.names, levels(data$id)), "undef",
                       rep(0, length(setdiff(data.names, levels(data$id)))))

# pesi(esempio): DATE = 4, DURATION = 1,SET= 2,  TIME = 0
# per i TIME non classifico
# DURATION < SET
# SET << DATE
# Calcolo tabella frequenze pesate
w <- c(4, 1, 2, 0)
tab <- prop.table.weighted(table(data[, list(id, type)]), w)
# Individuazione tipo di espressione con frequenza pesata massima
max <- max.freq(tab)
# str(max)

# Individuazione dei ties
ties.type <- ties(max)
ties.type.matrix <- matrix.ties(ties.type, c("id", "type", "Freq")) 
ties.type.matrix

if(length(ties.type) > 0)  max <-max[-which(names(max) %in% names(ties.type))]


max <- rbind(list.to.data.frame(max, colnames = c("id", "type")), ties.type.matrix)

str(max)

class.DATE <- droplevels(max[which(max$type == "DATE"), ])
class.SET <- droplevels(max[which(max$type == "SET"), ])
class.DURATION <- droplevels(max[which(max$type == "DURATION"), ])
# Se rimangono fuori dei documenti, sono quelli con espressioni solo di tipo TIME

stato(classificazione)
# Frequenza pesata come score provvisorio
classificazione <- tag(classificazione, class.DURATION$id, "duration", class.DURATION$Freq)
stato(classificazione)
classificazione <- tag(classificazione, class.SET$id, "recurrence", class.SET$Freq)
stato(classificazione)


length(setdiff(levels(DATE$id), unique(classificazione$id)))+ length(unique(classificazione$id))


# Gestione classficazione traminte DATE
DATE <- droplevels(data[which(data$id %in% unique(class.DATE$id)), ])
DATE <- droplevels(DATE[which(DATE$type == "DATE"), ])
str(DATE) #110279
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

tab.DATE <- tab.DATE*as.numeric(class.DATE$Freq)

# Individuo la categoria massima 
max.DATE <- max.freq(tab.DATE)

#
# Individuazione dei ties 
ties.DATE <- ties(max.DATE)

ties.DATE.matrix <- matrix.ties(ties.DATE, c("id", "gran", "Freq"))
# ties.DATE.matrix[1:10, ]

if(length(ties.DATE) > 0)  max.DATE <-max.DATE[-which(names(max.DATE) %in% names(ties.DATE))]

# Documenti da classificare tramite REF

max.DATE <- rbind(list.to.data.frame(max.DATE, c("id" , "gran")), ties.DATE.matrix)
# length(levels(max.DATE$id))

# Documenti da classificare tramite REF
class.REF <- droplevels(max.DATE[which(max.DATE$gran == "ref"), ])
# Documenti da classificare tramite gli intervalli
class.noREF <- droplevels(max.DATE[which(max.DATE$gran == "noref"), ])

# check length(levels(class.REF$id)) + length(levels(class.noREF$id)) - length(ties.DATE)
#---------------------------------#
# Classificazione in base ai ref
REF <- droplevels(data[which((data$id %in%levels(class.REF$id))&(data$gran == "ref")), ])

tab.ref <- prop.table(table(REF[, list(id, value)]), margin = 1)

# Aggiornamento dello score 
tab.DATE <- tab.DATE*as.numeric(class.DATE$Freq)

max.ref <- max.freq(tab.ref)
#-----------------#
# Individuazione dei ties 
ties.ref <- ties(max.ref)

ties.ref.matrix<- matrix.ties(ties.ref, c("id", "ref", "Freq"))
# str(ties.ref.matrix)

if(length(ties.ref) > 0)  max.ref <-max.ref[-which(names(max.ref) %in% names(ties.ref))]

max.ref<- rbind(list.to.data.frame(max.ref, c("id", "ref" )), ties.ref.matrix)

class.PAST <- droplevels(max.ref[which(max.ref$ref == "PAST_REF"), ])
class.PRESENT <- droplevels(max.ref[which(max.ref$ref == "PRESENT_REF"), ])
class.FUTURE <- droplevels(max.ref[which(max.ref$ref == "FUTURE_REF"), ])

# check dim(max.ref)[1]
# dim(class.PAST)[1] + dim(class.PRESENT)[1] + dim(class.FUTURE)[1]

classificazione <- tag(classificazione, levels(class.PAST$id), "past", class.PAST$Freq)
classificazione <- tag(classificazione, levels(class.PRESENT$id), "present", class.PRESENT$Freq)
classificazione <- tag(classificazione, levels(class.FUTURE$id), "future", class.FUTURE$Freq)

stato(classificazione)
#---------------------------------------------------------------#
# Classificazione per intervalli
# parto da levels(class.noREF$id) e recupero gli intervalli associati

load("/home/sally/Documents/results90.RData")
length(results)
to.class <- results[which(names(results) %in% levels(class.noREF$id))]
length(to.class)
#check 
length(levels(class.noREF$id))

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
stato(classificazione)

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
stato(classificazione)

# Calcolo ampiezze intervalli
# prova <- to.class[1:100]
# prova[[1]]$interval <- as.numeric(prova[[1]]$upper - prova[[1]]$lower) + 1
#=============================================================#
tagfrominterval <- function(x)
{
  # Da decidere quanto si vuole differenziare (ad es. aggiungere anche decades etc)
  # tuttavia poi ci mette di più
  if(x == 0) x = "day"
  else if((x > 0)&(x <= 28)) x = "days"
  else if((x > 28)&(x <= 31)) x = "month"
  else if((x > 31)&(x <= 360)) x = "months"
  else if((x > 360)&(x <= round(365*1.5))) x = "year"
  else if((x > round(365*1.5))&(x <= round(365*9.5))) x = "years"
  else if(x > round(365*9.5)&(x <= round(365*15))) x = "decade"
  else if(x > round(365*9.5)&(x <= round(365*95))) x = "decades"
  else if(x > round(365*95)&(x <= round(365*150))) x = "century"
  else if(x > round(365*150)) x = "centuries"
}


add <- function(list) 
{
  list$interval <- as.numeric(list$upper - list$lower)
  if(length(list$interval) == 1)
  {
    list$tag <- tagfrominterval(list$interval)
    list$prob <- list$prob*as.numeric(class.noREF[which(class.noREF$id %in% names(list)), ]$Freq)
  }
  else
  {
    list$tag <- unique(sapply(list$interval, tagfrominterval))
    # (uso di unique dalle considerazioni in fondo alla script)

  }
  
  list
}

library(plyr)

# prova <-llply(to.class[1:10], function(x) add(x))
# prova

# length(setdiff(names(to.class), unique(classificazione$id))) + length(unique(classificazione$id))

startTimer()
to.class <-llply(to.class, function(x) add(x))
stopTimer() #[1] "Tempo trascorso:  7 m 13 s 420728 milli"

#=============================================================#
# Aggiunta al dataframe classificazione - DA DEFINIRE LO SCORE 
# Per ora è dato da frequenzaPesata
#=============================================================#
# prova <- head(to.class)
# c <- classificazione[c(1:7), ]
# startTimer()
# d <-update.intervalli(prova, c)
# stopTimer()
#-------------------------------------------------------------------#

startTimer()
classificazione <- update.intervalli(to.class, classificazione)
stopTimer()

length(to.class)

stato(classificazione) # check conteggi da sistemare (sballati perchè abbiamo più di un tag)

save(classificazione,file =  "classCheck.RData")

