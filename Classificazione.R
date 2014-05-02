#------------------------------------------------------------------#
# source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
# config <- set.config(user = "alan")
source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
config <- set.config(user = "sally")
config
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))
#------------------------------------------------------------------#
# Funzioni utili
list.to.data.frame <- function(lista, colnames = NULL)
{
  # trasforma una lita di liste in un dataframe
  lista.data <- as.data.frame(cbind(names(lista), as.vector(sapply(lista, "[[", 1))), row.names = NULL)
  colnames(lista.data)<-colnames
  lista.data$Freq <- as.vector(sapply(lista, "[[", 2))
  
  droplevels(lista.data)
}
#----------------------------------------------------------#
# Lettura dataset pulito
setwd(config[3])
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
str(date)
# documenti che non hanno nemmeno una data
nodate <- droplevels(data[-which(data$id %in% levels(droplevels(data[which(data$type == "DATE"), ])$id)), ])
str(nodate)
# I documenti con solo espressioni di tipo TIME non sono classificabili  
tab <- as.data.frame(prop.table(table(nodate[, list(id, type)]), margin = 1))
solo.TIME <- droplevels(tab[which((tab$type == "TIME")&(tab$Freq == 1)), ])

classificazione[which(classificazione$id %in% levels(solo.TIME$id)), ]$class <- "undef"
classificazione[which(classificazione$class == "undef"),]

#-------------------------------------------------------------------------------------#
# PROVA Calcolo tabella frequenza pesate
# pesi
# w <- c(2, 1, 1, 0)
# prova <- table(date[, list(id, type)])[1:10, ]

# prova[, 1] <- prova[, 1]*w[1]
# prova[, 2] <- prova[, 2]*w[2]
# prova[, 3] <- prova[, 3]*w[3]
# prova[, 4] <- prova[, 4]*w[4]

# prop.table(prova, margin = 1)
# prop.table(table(date[, list(id, type)])[1:10, ], margin = 1)
# --- Funzione --- #
# prova2 <- table(date[, list(id, type)])[1:10, ]

prop.table.weighted <- function(table, w)
{
  for(i in 1:dim(table)[2])
  {
    table[, i] <- table[, i]*w[i]
  }
  prop.table(table, margin = 1)  
}

# prop.table.weighted(prova2, w = w)

#--------------------------------------------------#
# Prove di classificazione: 
# 1) considero dei pesi e classifico in base alla tipologia con frequenza pesata massima
# 2) Se c'è almeno un'espressione di tipo DATE classifico in base a quel tipo
# (da definire un punteggio che penalizzi classificazioni fatte in base a poche date)

#-------------------------------------------------#
# Classificazione 1

# pesi(esempio): DATE = 4, DURATION = 1,SET= 2,  TIME = 0
# per i TIME non classifico
# DURATION < SET
# SET << DATE

w <- c(4, 1, 2, 0)

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

# intanto li metto da parte e li escludo dalla classificazione
ties.type <- max[cond]
max <-max[-which(names(max) %in% names(max[cond]))]
str(max) # 111117

# Trasformo il tutto in dataframe per comodità
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
# Per i documenti classificabile tramite DATE bisogna andare a vedere i REF
class.DATE[1:10, ]

DATE <- droplevels(date[which(date$id %in% unique(class.DATE$id)), ])
DATE <- droplevels(DATE[which(DATE$type == "DATE"), ])
str(DATE) #110439
# Identifico quei documenti che hanno espressione di tipo DATE ma solo undefined
# 361
undef.DATE <- names(which(prop.table((table(DATE[, list(id, gran)])), margin = 1)[, 6] == 1))
# Elimino tutti gli undefined

DATE <- droplevels(DATE[-which(DATE$gran == "undefined"), ])

str(DATE) # check (ok)  110078 + 361

# PESI
# day, days, month, months, year, years, undefined = 1
# ref = 1/4

w.ref <- c(1, 1, 1, 1, 1/4, 1,1)
tab.DATE <- prop.table.weighted(table(DATE[, list(id, gran)]), w.ref)
str(tab.DATE)

# creo una tabella con due colonne (ref e resto)
tab.DATE <- cbind(apply(tab.DATE[, c(1:4, 6, 7)], 1, sum), tab.DATE[, 5])
colnames(tab.DATE) <- c("other", "ref")
head(tab.DATE)


max.DATE <- apply(tab.DATE, 1, function(x) list(colnames(tab.DATE)[which(x == max(x))], max(x)))
str(max.DATE) # 111078

# Individuo i documenti con i ties 
# sapply(max, "[[", 1) estrae il primo elemento da ogni lista (e ritorna ancora una lista)
# applico la funzione logica sugli elementi della lista, 
# e poi estraggo quelli che hanno ritornato TRUE)
cond <- sapply((sapply(max.DATE, "[[", 1)), function(x) length(x) == 2)
max.DATE[cond]
length(unique(names(max.DATE[cond]))) #141 con i pesi proposti

# Intanto li metto da parte
ties.DATE <- max.DATE[cond]
names(ties.DATE)
max.DATE<- max.DATE[-which(names(max.DATE) %in% names(ties.DATE))]

# Individuo i documenti che hanno REF con frequenza pesata massima
# sapply(max, "[[", 1) estrae il primo elemento da ogni lista (e ritorna ancora una lista)
# applico la funzione logica sugli elementi della lista, 
# e poi estraggo quelli che hanno ritornato TRUE)

cond <- sapply((sapply(max.DATE, "[[", 1)), function(x) x == "ref")
cond

max.DATE[cond]
length(max[cond]) # 4449 da classificare secondo i REF

# Classificazione in base ai ref
ref <- droplevels(date[which(date$id %in% names(max.DATE[cond])), ])
ref <- droplevels(ref[which(ref$gran == "ref"),])
str(ref)

tab.ref <- prop.table(table(ref[, list(id, value)]), margin = 1)
head(tab.ref)

max.ref <- apply(tab.ref, 1, function(x) list(colnames(tab.ref)[which(x == max(x))], max(x)))
str(max.ref) #

# Individuo i documenti con i ties 
cond <- sapply((sapply(max.ref, "[[", 1)), function(x) length(x) > 2)
ties.ref <- max.ref[cond]
length(unique(names(ties.ref))) #272 + 8 (ce ne sono anche con 3)

# rimanenti
max.ref <- max.ref[-which(names(max.ref) %in% names(ties.ref))]
length(max.ref)

ref.data <- list.to.data.frame(max.ref, c("id", "ref" ))
head(ref.data)

class.PAST <- droplevels(ref.data[which(ref.data$ref == "PAST_REF"), ])
class.PRESENT <- droplevels(ref.data[which(ref.data$ref == "PRESENT_REF"), ])
class.FUTURE <- droplevels(ref.data[which(ref.data$ref == "FUTURE_REF"), ])

dim(class.PAST)[1] + dim(class.PRESENT)[1] + dim(class.FUTURE)[1]

levels(class.PAST$id)

# class.* sono data.frame di liste
# classificazione[.] <- "past" va in errore
# è necessario ricostruire i data.frame/levels

classificazione[which(classificazione$id %in% names(class.PAST$id)),]$class <- "past"
# classificazione[which(classificazione$id %in% levels(class.PAST)),]$score <- 

classificazione[which(classificazione$id %in% names(class.PRESENT$id)),]$class <- "present"
# classificazione[which(classificazione$id %in% names(class.PRESENT)),]$score

classificazione[which(classificazione$id %in% names(class.FUTURE$id)),]$class <- "future"
# classificazione[which(classificazione$id %in% levels(class.FUTURE$id)),]$score

levels(classificazione$class)
#----------------#
# Rimangono da gestire i ties
names(ties.type)
names(ties.DATE)
names(ties.ref)

# mentre i DATE undefined vanno insieme ai nodate
tmp <- droplevels(data[which(data$id %in% undef.DATE),])
tmp <- droplevels(tmp[-which(tmp$type == "DATE")])
str(tmp) #è diminuito il numero di documenti
# NON CLASSIFICABILI (documenti con solo espressioni DATE undefined)
non.class <- setdiff(undef.DATE, levels(tmp$id))
classificazione[which(classificazione$id %in% non.class), ]$class <- "undef"

# GLi altri li unisco ai nodate per classificazione alternativa (in base a SET o DURATION)
nodate <-rbind(nodate, tmp)
rm(tmp)

#----------------------------------------------------------#
# CLassificazione dei NODATE
# pesi: DATE = 4, DURATION = 1,SET= 2,  TIME = 0
# per i TIME non classifico
# DURATION < SET
# SET << DATE
str(nodate)
w <- c(1, 2, 0)
tab.nodate <- prop.table.weighted(table(nodate[, list(id, type)]), w)
str(tab.nodate)
tab.nodate[1:100, ]

# Trovo il tipo di espressione con frequenza massima in base alla quale classificare, con
# relativa frequenza pesata. Se ci sono dei ties ("pareggi") vengono ritornati i massimi
max.freq <- function(table)
{
  max <- apply(table, 1, function(x) list(colnames(table)[which(x == max(x))], max(x)))
  max  
}
max.nodate <- max.freq(tab.nodate)

ties <- function(lista)
{
  cond <- sapply((sapply(lista, "[[", 1)), function(x) length(x) == 2)
  cat("Numero di documenti con ties ", length(lista[cond]))
  lista[cond]
}  
ties.nodate <- ties(max.nodate)

max.nodate <- max.nodate[-which(names(max.nodate) %in% names(ties.nodate))]


list.to.data.frame <- function(lista, colnames = NULL)
{
  lista.data <- as.data.frame(cbind(names(lista), as.vector(sapply(lista, "[[", 1))), row.names = NULL)
  colnames(lista.data)<-colnames
  lista.data$Freq <- as.vector(sapply(lista, "[[", 2))
  
  droplevels(lista.data)
}

nodate.data <- list.to.data.frame(max.nodate, colnames = c("id", "type"))

class.SET <- droplevels(nodate.data[which(nodate.data$type == "SET"), ])
dim(class.SET)
class.DURATION <- droplevels(nodate.data[which(nodate.data$type == "DURATION"), ])
dim(class.DURATION)
# Ne ignora 13 ma sono quelli che avevano solo TIME

classificazione[which(classificazione$id %in% names(class.SET$id)),]$class <- "recurrence"
classificazione[which(classificazione$id %in% names(class.SET$id)),]$score<- class.SET$Freq

classificazione[which(classificazione$id %in% names(class.DURATION$id)),]$class <- "duration"
classificazione[which(classificazione$id %in% names(class.DURATION$id)),]$score <- class.DURATION$Freq


# Verifico stato classificazione attuale

head(classificazione)
str(classificazione)
levels(classificazione$class)


