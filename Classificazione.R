#------------------------------------------------------------------#
# source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
# config <- set.config(user = "alan")
source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
config <- set.config(user = "sally")
config
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))
#------------------------------------------------------------------#
# Lettura dataset pulito
setwd(config[2])
data <- ricarica.dataset("heidel_pulizia.def.txt")
library(data.table) # libreria che gestisce più velocemente grandi moli di dati
# implementa alcuni aspetti delle basi di dati
# (quick)info libreria 
# vignette("datatable-intro")

data <- data.table(data)
setkey(data, id, type)

#-----------------------------------#
# Creazione dataframe classificazione (id doc + classe) da "riempire"
classificazione <- data.frame(cbind(levels(data$id), NA))
colnames(classificazione) <- c("id", "class")
# Bisogna decidere i livelli
classificazione$class <- factor(classificazione$class, levels= c("past", "present", "future", 
                                                                 "day", "days", "month",
                                                                 "months", "year", "years", 
                                                                 "undef", "recurrence", "duration"))

# Colonna rappresentante il punteggio associato alla classificazione
# e.g. frequenza pesata
classificazione$score <- rep(0, dim(classificazione)[1])
str(classificazione)
#-------------------------------------------------------------------------------------#
# Sottoinsieme delle espressioni relative a documenti con almeno una data
# (da passare all'espansione)
date <- droplevels(data[which(data$id %in% levels(droplevels(data[which(data$type == "DATE"), ])$id)), ])
# documenti che non hanno nemmeno una data
nodate <- droplevels(data[-which(data$id %in% levels(droplevels(data[which(data$type == "DATE"), ])$id)), ])
str(nodate)
# I documenti con solo espressioni di tipo TIME non sono classificabili  
tab <- as.data.frame(prop.table(table(nodate[, list(id, type)]), margin = 1))
solo.TIME <- droplevels(tab[which((tab$type == "TIME")&(tab$Freq == 1)), ])

classificazione[which(classificazione$id %in% levels(solo.TIME$id)), ]$class <- "undef"
classificazione[which(classificazione$class == "undef"),]

# Per gli altri? DA VEDERE (considerare anche la possibilità di calcolare frequenze pesate)
#-------------------------------------------------------------------------------------#
# PROVA Calcolo tabella frequenza pesate
# pesi
w <- c(2, 1, 1, 0)
prova <- table(date[, list(id, type)])[1:10, ]

prova[, 1] <- prova[, 1]*w[1]
prova[, 2] <- prova[, 2]*w[2]
prova[, 3] <- prova[, 3]*w[3]
prova[, 4] <- prova[, 4]*w[4]

prop.table(prova, margin = 1)
prop.table(table(date[, list(id, type)])[1:10, ], margin = 1)
# Funzione
prova2 <- table(date[, list(id, type)])[1:10, ]

prop.table.weighted <- function(table, w)
{
  for(i in 1:dim(table)[2])
  {
    table[, i] <- table[, i]*w[i]
  }
  prop.table(table, margin = 1)  
}

prop.table.weighted(prova2, w = w)

#--------------------------------------------------#
# Prove sul dataset
# pesi(esempio): DATE = 4, DURATION = 1,SET= 2,  TIME = 0
# per i TIME non classifico
# DURATION < SET
# SET << DATE

w <- c(4, 1, 2, 0)
# w <- c(2, 1,1, 0) 
tab <- prop.table.weighted(table(date[, list(id, type)]), w)
str(tab)
tab[1:100, ]

# Trovo il tipo di espressione con frequenza massima in base alla quale classificare, con
# relativa frequenza pesata. Se ci sono dei ties ("pareggi") vengono ritornati i massimi
max <- apply(tab, 1, function(x) list(colnames(tab)[which(x == max(x))], max(x)))
str(max) # 111306 elementi

# Individuo i documenti con i ties 
# sapply(max, "[[", 1) estrae il primo elemento da ogni lista (e ritorna ancora una lista)
# applico la funzione logica sugli elementi della lista, 
# e poi estraggo quelli che hanno ritornato TRUE)

cond <- sapply((sapply(max, "[[", 1)), function(x) length(x) == 2)
max[cond]
length(max[cond])

# Recupero gli id del documento (sono 189 con i pesi considerati)
# e si dovrà decidere cosa farne
names(max[cond])

# intanto li escludo dalla classificazione
max <-max[-which(names(max) %in% names(max[cond]))]
str(max) # 111117

# Trasoformo il tutto in dataframe per comodità
max.data <- data.frame(cbind(names(max), as.vector(sapply(max, "[[", 1))), row.names = NULL)
colnames(max.data)<-c ("id", "type")
max.data$Freq <- as.vector(sapply(max, "[[", 2))



class.DATE <- max.data[which(max.data$type == "DATE"), ]
class.SET <- max.data[which(max.data$type == "SET"), ]
class.DURATION <- max.data[which(max.data$type == "DURATION"), ]

# check length(class.SET) + length(class.DATE) + length(class.DURATION)

# Per i SET sarebbe possibile una classificazione più fine (ma per ora lasciamo stare)
data[which(data$id %in% unique(class.SET$id)), ]

str(classificazione)
classificazione[which(classificazione$id %in% unique(class.SET$id)), ]$class <- "recurrence"
# Check (se sgli indici hanno lo stesso ordine)
# cbind(classificazione[which(classificazione$id %in% unique(class.SET$id)), ]$id, class.SET$id) 
classificazione[which(classificazione$id %in% unique(class.SET$id)), ]$score <- class.SET$Freq

classificazione[which(classificazione$id %in% unique(class.DURATION$id)), ]$class <- "duration"
classificazione[which(classificazione$id %in% unique(class.DURATION$id)), ]$score <- class.DURATION$Freq
#-------------------------------------------------------------------------------------#
class.DATE[1:10, ]
# Per i documenti classificabile tramite DATE bisogna andare a vedere i REF

