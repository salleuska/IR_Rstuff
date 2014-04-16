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
