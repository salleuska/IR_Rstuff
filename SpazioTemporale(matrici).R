#------------------------------------------------------------------#
source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
config <- set.config(user = "alan")
#source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
#config <- set.config(user = "sally")
#config
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))
#------------------------------------------------------------------#
# Lettura dataset pulito
setwd(config[3])
data <- ricarica.dataset("heidel_pulizia.def.txt")
library(data.table) # libreria che gestisce più velocemente grandi moli di dati
                    # implementa alcuni aspetti delle basi di dati
# (quick)info libreria 
# vignette("datatable-intro")

data <- data.table(data)
setkey(data, id, type)
tables()

#-----------------------------------#
# Creazione dataframe classificazione (id doc + classe) da "riempire"
classificazione <- data.frame(cbind(levels(data$id), NA))
colnames(classificazione) <- c("id", "class")
# Bisogna decidere i livelli
classificazione$class <- factor(classificazione$class, levels= c("past", "present", "future", "day", "days", "month", "months", "year", "years", "undef"))
str(classificazione)
#-------------------------------------#
# Considerando frequenze maggiori di una certa soglia c (e.g. c = 0.75)
# se in un certo documento un tipo di espressione temporale ha una 
# frequenze maggiore di 0.75 classifico solo in base a quella 
tab <- as.data.frame(prop.table(table(data[, list(id, type)]), margin = 1))
# check
# tab[which(tab$id == levels(tab$id)[1]),]
# tab[which(tab$id == levels(tab$id)[3]),]

# seleziono i documenti che hanno un tipo di espressione temporale con frequenze maggiore 
# del 75%
tab <- as.data.frame(tab)
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

# Considero i documenti classificabili tramite DATE
data.DATE <- droplevels(data[which(data$id %in% class.c[which(class.c$type == "DATE"), ]$id), ])

str(data.DATE)

# classifico i soli Ref
# tabella frequenze
tab.gran <- as.data.frame(prop.table(table(data.DATE[, list(id, gran)]), margin = 1))
tab.gran[1:10,]
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

ref.toclass <- data.table(droplevels(tab.ref[which(tab.ref$id %in% setdiff(levels(tab.ref$id), levels(class.ref.c$id))), ]))
str(ref.toclass)
prop.table(table(ref.toclass[, list(id, value)]), margin = 1)

# Documenti rimasti (espressioni "miste")
to.class <- droplevels(tab[which(!(tab$id %in% levels(class.c$id))), ])
# length(levels(to.class$id)) + dim(class.c)[1]
# Numero 
length(levels(to.class$id))
#-------------------------------------------------------------------------------------#
# CONSIDERAZIONI (vedi in drive: AnalisiTemporale)

prop.table(table(data[, list(id, type)]), margin = 1)[1:100, ]

es <- droplevels(data[which(data$id == "WSJ861201-0003"), ])
es

prop.table(table(es[which(es$type == "DATE"), list(id, gran)]), margin = 1)

#load("/home/sally/Documents/results.RData")
load("results.RData")
results["WSJ861201-0003"]
results[["WSJ861201-0003"]]$upper - results[["WSJ861201-0003"]]$lower

es <- droplevels(data[which(data$id == "WSJ861201-0007"), ])
es

es[which(es$type == "DATE"), ]
prop.table(table(es[which(es$type == "DATE"), list(id, gran)]), margin = 1)


es[which(es$gran == "ref"), ]
prop.table(table(es[which(es$gran == "ref"), list(id, gran)]), margin = 1)

results["WSJ861201-0007"]
results[["WSJ861201-0007"]]$upper - results[["WSJ861201-0007"]]$lower
#-------------------------------------------------------------------------------------#

