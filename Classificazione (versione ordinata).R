#------------------------------------------------------------------#
# source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
# config <- set.config(user = "alan")
source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
config <- set.config(user = "sally")
#config
source(paste(config[1], "FunzioniClassificazione.R", sep = ""))
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))
#------------------------------------------------------------------#
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

#-------------------------#
# Caricamento libreria
library(data.table) 

# Lettura dataset pulito
setwd(config[2])
data <- ricarica.dataset("heidel_pulizia.def.txt")

data <- data.table(data)
setkey(data, id, type)

# ---------------------------#
# pesi(esempio): DATE = 4, DURATION = 1,SET= 2,  TIME = 0
# per i TIME non classifico
# DURATION < SET
# SET << DATE

# Individuo i documenti che hanno espressioni solo di tipo TIME
max.abs <- list.to.data.frame(max.freq(prop.table(table(data[, list(id, type)]), margin = 1)),c("id", "type"))
class.TIME <- droplevels(max[which((max.abs$type == "TIME")&(max.abs$Freq == 1)), ])

classificazione <- tag(classificazione, levels(class.TIME$id), "undef", 0)
# check classificazione[which(classificazione$class == "undef"),]

stato.classificazione()

# Altri tipi (DATE, SET, DURATION)
# Calcolo tabella frequenze pesate
w <- c(4, 1, 2, 0)
tab <- prop.table.weighted(table(data[, list(id, type)]), w)
# Individuazione tipo di espressione con frequenza pesata massima
max <- max.freq(tab)
#===================================================#
# Individuazione dei ties (DA GESTIRE ANCORA)
max.ties <- ties(max)

if(length(max.ties) > 0)  max <-max[-which(names(max) %in% names(max.ties))]
#===================================================#

max <- list.to.data.frame(max, colnames = c("id", "type"))

# trasforma una lista di liste in un dataframe
lista.data <- data.frame(cbind(names(lista), as.vector(sapply(lista, "[[", 1))), row.names = NULL)
colnames(lista.data)<-colnames
lista.data$Freq <- as.vector(sapply(lista, "[[", 2))


class.DATE <- droplevels(max[which(max$type == "DATE"), ])
class.SET <- droplevels(max[which(max$type == "SET"), ])
class.DURATION <- droplevels(max[which(max$type == "DURATION"), ])

# FARE CHECK CONTEGGI DI TUTTO!

length(levels(data$id)) - length(max.ties)

summary(data$type)
# check dim(class.SET)[1] + dim(class.DATE)[1] + dim(class.DURATION)[1]

setdiff(class)
