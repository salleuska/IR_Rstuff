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
classificazione$class <- factor(classificazione$class, levels= c("past", "present", "future", "day", "days", "month", "months", "year", "years", "undef"))
str(classificazione)

#-------------------------------------------------------------------------------------#
# Sottoinsieme delle espressioni relative a documenti con almeno una data
# (da passare all'espansione)
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
date <- droplevels(data[which(data$id %in% levels(droplevels(data[which(data$type == "DATE"), ])$id)), ])
str(date)

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


#
tab <- prop.table.weighted(table(date[, list(id, type)]), w)
str(tab)
tab[1:100, ]
