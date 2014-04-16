# Rimane questo blocchetto di codice da controllare all'inzio dello script,
# non ho trovato niente di meglio (o meglio non volevo perderci troppo tempo)
# NOTA: getwd() restituisce la working directory corrente
#------------------------------------------------------------------#
# source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
# config <- set.config(user = "alan")
source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
config <- set.config(user = "sally")
config
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))

setwd(config[2])
data.originali <- ricarica.dataset("heidel_pulizia.def.txt")

check.valori.undefined(data.originali)
#----------------------------------------------------#
#--- Analisi su un sottoinsieme (casuale) di dati ---#
# set.seed(1234) #fissa il numero di partenza per il sample (in modo da poter riestrarre "casualmente" 
               # gli stessi dati)

# data <- data.originali[which(data.originali$id %in% sample(levels(data.originali$id), 10000)),]
# data <- droplevels(data)

# Su tutti i dati
data <- data.originali
# DA QUI SI PUÒ SALTARE A RIGA 88 PER IL CHECK DEI DOCUMENTI NON ESPANSI
#-----------------------------------------------------#
# libreria che dovrebbe gestire grandi dataset con più facilità
# install.packages("data.table")
library(data.table) 
data <- data.table(data)

# (quick)info libreria
# vignette("datatable-intro")
# vignette("datatable-faq")

# conteggi variabili nei documenti
data[, summary(type)]
data[, summary(gran)]

data[, summary(id)]
data[, summary(term)]

# options(digits = 3)
# Distribuzione variabili nei documenti
data[, summary(type)]/dim(data)[1]
data[, summary(gran)]/dim(data)[1]


# totale documenti 
length(levels(data$id))
# Numero di espressioni nei documenti
# espr.per.doc <- summary(data$id, maxsum= Inf)
espr.per.doc <- data[, summary(id, maxsum = Inf)]
summary(espr.per.doc)

hist(espr.per.doc, breaks = 1000, prob = T, main = "Distribuzione espressioni nei documenti")
plot(espr.per.doc, type = "l")

# tanti documenti con poche espressioni (minimo una espressione)
# data[, sum(summary(id, maxsum = Inf) == 1)]
# problema: numero minimo di espressioni per poter classificare?
length(which(espr.per.doc < 2 ))
length(which(espr.per.doc >= 5)) # si dovrà decidere una soglia

# type per documento

type.per.id <- table(data[, list(id, type)])
str(type.per.id)

type.per.id[1:10, 1:4]

plot(type.per.id[, "DATE"], type = "l")
plot(type.per.id[, "DURATION"], type = "l")
plot(type.per.id[, "TIME"], type = "l")
plot(type.per.id[, "SET"], type = "l")


# Se voglio le percentuali per documento 
head(prop.table(type.per.id, margin = 1))

# distribuzione di type nei documenti

summary(data$id)[1:10]
head(table(data$id, data$type))
#------------------------------------------------------#
# Documenti che contengono espressioni di tipo DATE
date <- droplevels(data[which(data$type == "DATE"), ])

# documenti con almeno una data
length(levels(date$id))

str(date)
espr.per.doc <- summary(data$id, maxsum= Inf)
summary(espr.per.doc)

plot(table(espr.per.doc), main = "Distribuzione numero di date nei documenti", 
     xlab = "Numero di date", ylab = "Conteggi")
# (Tanti documenti con una sola data)

# Documenti con tante date 
head(sort(espr.per.doc, decreasing =T), n= 40)

# Numero documenti con più di 200 date
length(espr.per.doc[espr.per.doc > 200])
# Numero documenti con 100-200 date
length(espr.per.doc[((espr.per.doc < 200)&(espr.per.doc > 100))])
# Numero documenti con 50-100 date
length(espr.per.doc[((espr.per.doc < 100)&(espr.per.doc > 50))])

# Numero documenti con 1 sola data (inutili per espansione)
length(espr.per.doc[espr.per.doc < 2])
# distribuzione del numero di date nei documenti
summary(espr.per.doc)
#------------------------------------------------------------------#
# Check documenti per esplosione date
esplosione <- droplevels(date[which(date$gran != "ref"), ])
str(esplosione)
length(levels(esplosione$id))

esplosione <- droplevels(esplosione[which(esplosione$gran != "undefined"), ])
# Numero di documenti che dovrebbero essere esplosi
length(levels(esplosione$id))
# Nota : da bash sono 106.582
# Confronto 
check <- levels(esplosione$id)

setwd("/home/sally/Documents/Dimensione_temporale_divisi")
# Recupero gli ID dei documenti prodotti dall'espansione
divisi <- list.files()
head(divisi)
divisi <- sub("_txt", "", divisi)
divisi <-as.factor(divisi)
str(divisi)

non.espansi <-check[-which(divisi %in% check)]
non.espansi

to.check <- data[which(data$id %in% non.espansi), ]
setwd(config[3])
write.table(to.check, file = "data_to_split.txt", sep="\t",
            row.names = FALSE, col.names = FALSE, quote = FALSE)
# Elaborati : OK
#-------------------------------------------------------------------#
# Documenti che non contengono nemmeno un'espressione di tipo DATE
length(which((data$id %in% levels(date$id)))) 
length(levels(date$id))

nodate <- droplevels(data[-(which(data$id %in% levels(date$id))), ])
length(levels(nodate$id)) # check: 2575 + 111306
str(nodate)

summary(nodate$type)

type.per.id <- table(data[, list(id, type)])
head(nodate)