# setwd("~/Scrivania/TipsterData/")
setwd("/home/alan/Documents/GIT/Rstuff")

#-----------------------------------------------------------------------------#
# Lettura dati
data <- read.delim("esiti 1989/heidel_details.txt", header=F)
colnames(data) <- c("id", "type", "value", "term", "creation")
#------------------------------------------------------------------------------#
print(unique(data$value[data$type == "DATE"]))

data[data$value == 1169, ]

data[(as.integer(data$value)>2000), ]

for(i in 1:50)
{
  print(data[data$value ==levels(data$value)[i], ])
}

# Nuova colonna per differenziare gli anni
data$year <- data$value

cbind(as.integer(levels(data$value)), levels(data$value))
levels(data$year) <- as.integer(levels(data$value))
as.integer(levels(data$year))

# Pulizia 
# valori che potrebbero non avere senso, alti e bassi
# misstyping o errori heideltime

high <- which(as.integer(levels(data$year)) > 2000)
check.high <- levels(data$year)[high]
check.high

data[which(data$year == check.high[27]), ]

# Possibilità: eliminare quelli meno frequenti?

count <- numeric(length(check.high))
for(i in 1:length(check.high))
{
  count[i] <-dim(data[which(data$year == check.high[i]), ])[1]
  count
}
cbind( check.high, count)
check.high[which(count < 5)]

# soglia da scegliere

# quali sono century (valori bassi)
low <- which(as.integer(levels(data$year)) < 1010)
check <- levels(data$year)[low]
check
data[which(data$year == check[25]), ]

as.character(data$term[which(data$year == check[1])])

# Per ogni espressione ho una lista contenente la frequenza e i termini associati 
count <- list(list())
for(i in 1:length(check))
{
  freq <- dim(data[which(data$year == check[i]), ])[1]
  terms <- unique(as.character(data$term[which(data$year == check[i])]))
  count[[check[i]]] <- list(terms = terms, freq = freq)
  count
}
str(count)

# Supponendo che tra 1000 e 2000 siano tutti anni
# per rinominare in (year, decade, etc)
levels(data$year)[which((as.integer(levels(data$year)) > 1000) &(as.integer(levels(data$year)) < 2000))] <- "year"
levels(data$year)
#------------------------------------------------------------#
# Funzione generale
# Estrazione espressione - frequenza - termini associati
# data.temp <- vettore espressioni temporali dai dati (come variabili fattoriali)

data.temp <- data$year

info <- function(data.temp)
{
  t.espr <- levels(data.temp)
  out <- list(list())
  for(i in 1:length(t.espr))
  {
    freq <- dim(data[which(data.temp == t.espr[i]), ])[1]
    terms <- unique(as.character(data$term[which(data.temp == t.espr[i])]))
    out[[t.espr[i]]] <- list(terms = terms, freq = freq)
  }
  out
}

# prova
prova <- info(data$year)
str(prova)

check
str(prova[check])
prova[check][ 191]
##
print(unique(data$value[data$type == "DURATION"]))
print(unique(data$value[data$type == "SET"]))

#--------------------#
# Gestione tipo TIME #
#--------------------#
print(sort(unique(data$value[data$type == "TIME"])))
length(unique(data$value[data$type == "TIME"]))

# farli diventare tutti time
