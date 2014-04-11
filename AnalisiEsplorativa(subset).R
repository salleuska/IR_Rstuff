# Rimane questo blocchetto di codice da controllare all'inzio dello script,
# non ho trovato niente di meglio (o meglio non volevo perderci troppo tempo)
# NOTA: getwd() restituisce la working directory corrente
#------------------------------------------------------------------#
# source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
# config <- set.config(user = "alan")
source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
config <- set.config(user = "sally")
config
# setwd(config[2])
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
#-----------------------------------------------------#
# libreria che dovrebbe gestire grandi dataset con più facilità
# install.packages("data.table")
library(data.table) 
data <- data.table(data)

# (quick)info libreria
# vignette("datatable-intro")
# vignette("datatable-faq")

# conteggi nei documenti
data[, summary(type)]
data[, summary(gran)]

data[, summary(id)]
data[, summary(term)]

# Distribuzione variabili nei documenti
data[, summary(type)]/dim(data)[1]
data[, summary(gran)]/dim(data)[1]


# totale documenti 
length(levels(data$id))
# Numero di espressioni nei documenti
# espr.per.doc <- summary(data$id, maxsum= Inf)
espr.per.doc <- data[, summary(id, maxsum = Inf)]
summary(espr.per.doc[-which.max(espr.per.doc)])

hist(espr.per.doc, breaks = 1000, prob = T, main = "Distribuzione espressioni nei documenti")
plot(espr.per.doc, type = "l")

# tanti documenti con poche espressioni (minimo una espressione)
# data[, sum(summary(id, maxsum = Inf) == 1)]
length(which(espr.per.doc > 2 ))
length(which(espr.per.doc <= 5)) # si dovrà decidere una soglia

# type per documento

type.per.id <- table(data[, list(id, type)])
str(type.per.id)

type.per.id[1:length(type.per.id)]

plot(type.per.id[, "DATE"], type = "l")
plot(type.per.id[, "DURATION"], type = "l")
plot(type.per.id[, "TIME"], type = "l")
plot(type.per.id[, "SET"], type = "l")

library(ggplot2)
qplot(data$type, data = data, geom = "bar")
qplot(data$type,data$id, data = data, geom = "bar")

# Se voglio le percentuali per documento 
head(prop.table(type.per.id, margin = 1))

# distribuzione di type nei documenti

summary(data$id)[1:10]
table(data$id, data$type)


date <- droplevels(data[which(data$type == "DATE"), ])
prop <- summary(date$id, max = Inf)
hist(prop, breaks = 300, xlim = c(0, 50))
head(sort(prop, decreasing =T))
median(prop)

# documenti con 1 data potremmo non espanderli perche tanto non li classifichiamo 
length(prop[-(which(prop  == 1))])
