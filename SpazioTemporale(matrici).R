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
tables()

# Considerando frequenze maggiori di una certa soglia c (e.g. c = 0.75)
# se in un certo documento un tipo di espressione temporale ha una 
# frequenze maggiore di 0.75 classifico solo in base a quella 
tab <- as.data.frame(prop.table(table(data[, list(id, type)]), margin = 1))
# check
# tab[which(tab$id == levels(tab$id)[1]),]
# tab[which(tab$id == levels(tab$id)[3]),]

# seleziono i documenti che hanno un tipo di espressione temporale con frequenze maggiore 
# del 75%
c = 0.75
class.c <- droplevels(tab[which(tab$Freq > c), ])
str(class.c)

# Documenti con prevalenza espressioni tipo TIME
dim(class.c[which(class.c$type == "TIME"), ])[1]
# cosa farne?
data[which(data$id %in% class.c[which(class.c$type == "TIME"), ]$id), ]

# Documenti con prevalenza espressioni tipo DURATION
dim(class.c[which(class.c$type == "DURATION"), ])[1]
# Documenti con prevalenza espressioni tipo SET
dim(class.c[which(class.c$type == "SET"), ])[1]
data[which(data$id %in% class.c[which(class.c$type == "SET"), ]$id), ]

# Documenti con prevalenza espressioni tipo DATE
dim(class.c[which(class.c$type == "DATE"), ])[1]

# classificazione
classificazione <- data.frame(cbind(levels(data$id), NA))
colnames(classificazione) <- c("id", "class")
# Bisogna decidere i livelli
classificazione$class <- factor(classificazione$class, levels= c("past", "present", "future", "day", "days", "month", "months", "year", "years", "undef"))
str(classificazione)

# classificazione[which(classificazione$id %in% class.c[which(class.c$type == "TIME"), ]$id), ]$class <- "time"
# classificazione[which(classificazione$id %in% class.c[which(class.c$type == "DURATION"), ]$id), ]$class <- "duration"
# classificazione[which(classificazione$id %in% class.c[which(class.c$type == "SET"), ]$id), ]$class <- "set"

# Documenti classificabili tramite DATE
data.DATE <- droplevels(data[which(data$id %in% class.c[which(class.c$type == "DATE"), ]$id), ])

str(data.DATE)

# classifico i soli Ref
# tabella frequenze
tab.gran <- as.data.frame(prop.table(table(data.DATE[, list(id, gran)]), margin = 1))
tab.gran[1:10]
# estrazione degli id dei documenti con una frequenza di espressioni di tipo ref maggiori di 
# c = 0.75
class.gran.c <- droplevels(tab.gran[which((tab.gran$Freq > c)&(tab.gran$gran == "ref")), ])
str(class.gran.c)

# Dal dataset originale estraggo le righe corrispondenti ai documenti identificati al passo precendente
solo.REF <- droplevels(data[which(data$id %in% levels(class.gran.c$id))])
str(solo.REF)
# Dato che classifico solo in base ai ref, tengo solo quelli
solo.REF <- droplevels(solo.REF[which(solo.REF$gran == "ref"),])
str(solo.REF )
solo.REF[1:100, ]

# Calcolo tabella frequenze per PAST, PRESENT, FUTURE_REF
# tab.ref <- prop.table(table(solo.REF[, list(id, value)]), margin = 1)
# tab.ref[1:100, ]

tab.ref <- droplevels(as.data.frame(prop.table(table(solo.REF[, list(id, value)]), margin = 1)))

# Determinare le soglie
c.ref <- 0.50
class.ref.c <- droplevels(tab.ref[which(tab.ref$Freq > c.ref), ])
str(class.ref.c)
dim(class.ref.c[which(class.ref.c$value == "PRESENT_REF"), ])[1]
dim(class.ref.c[which(class.ref.c$value == "PAST_REF"), ])[1]
dim(class.ref.c[which(class.ref.c$value == "FUTURE_REF"), ])[1]

# posso classificare 3059 doc
classificazione[which(classificazione$id %in% 
                       unique(class.ref.c[which(class.ref.c$value == "PRESENT_REF"), ]$id)), ]$class <- "present"
classificazione[which(classificazione$id %in% 
                        unique(class.ref.c[which(class.ref.c$value == "PAST_REF"), ]$id)), ]$class <- "past"
classificazione[which(classificazione$id %in% 
                        unique(class.ref.c[which(class.ref.c$value == "FUTURE_REF"), ]$id)), ]$class <- "future"

# rimanenti 
# length(levels(tab.ref$id)) - length(levels(class.ref.c$id))

ref.toclass <- droplevels(solo.REF[-which(solo.REF$id %in% levels(class.ref.c$id)), ])
str(ref.toclass)

prop.table(table(ref.toclass[, list(id, value)]), margin = 1)

# Documenti rimasti (espressioni "miste")
to.class <- droplevels(tab[which(!(tab$id %in% levels(class.c$id))), ])
# length(levels(to.class$id)) + dim(class.c)[1]
# Numero 
length(levels(to.class$id))
#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#
# Prova iniziale con le frequenze massime

# non classifico documenti con un numero di espressioni minore di ? DA VALUTARE
# espr.per.doc <- data[, summary(id, maxsum = Inf)]
# length(which(espr.per.doc < 2 ))
# data <- droplevels(data[-which(id %in% names(espr.per.doc[which(espr.per.doc < 2 )])), ])

#------------------------------------------#
# RAW: Calcolo frequenze per tipo
# 
# type.per.id <- table(data[, list(id, type)])
# head(type.per.id)
# str(type.per.id)

# head(prop.table(type.per.id, margin = 1))
# tab <- prop.table(type.per.id, margin = 1)

# prova <- tab[1:100, 1:4]
# colnames(prova)[apply(prova,1,which.max)]
#------------------------------------------#
# Calcolo tabella frequenze per documento
tab <- prop.table(table(data[, list(id, type)]), margin = 1)
tab[1:100, 1:4]
# PROBLEMA: cosa fare con i documenti che hanno frequenze uguali(o poca differenza)
# per più di un tipo di espressione? Ad esempio
tab[c("WSJ861201-0055","WSJ861201-0071"), ]


# Calcolo type con frequenza massima e ritorno id documento + type
max.type <- cbind(rownames(tab), colnames(tab)[apply(tab,1,which.max)])
colnames(max.type) = c("id", "type")
max.type[1:50, ]


# Selezione dei valori corrispondenti al tipo di frequenza massima
# e.g. siccome ho "WSJ861201-0001" "DATE"
# per il documento "WSJ861201-0001" tengo tutte le righe con tipo"DATE"
max.type <- as.data.table(max.type)

# str(max.type)
setkey(max.type, id, type)
data.max <- merge(data, max.type)
data.max[c(1:50), ]
# Documenti classificabili per DATE
data.DATE <- droplevels(data.max[type =="DATE", ])
str(data.DATE)

tab.gran <- prop.table(table(data.DATE[, list(id, gran)]), margin = 1)
tab.gran
tab.gran[1:10, "ref"]

# Documente con espressioni di tipo DATE con solo granularità di tipo ref
soloREF <- tab.gran[which(tab.gran[, "ref"] == 1), ]

data.soloREF <- droplevels(data.DATE[id %in% rownames(soloREF), ])
str(data.soloREF)
data.soloREF[1:100, ]

# FARE UNA FUNZIONE PER IL CALCOLO DELLE TABELLE
tab.ref <- prop.table(table(data.soloREF[, list(id, value)]), margin = 1)
tab.ref

max.ref <- cbind(rownames(tab.ref), colnames(tab.ref)[apply(tab.ref,1,which.max)])
colnames(max.ref) = c("id", "class")
max.ref[1:50, ]
