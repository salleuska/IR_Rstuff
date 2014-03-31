# setwd("~/Scrivania/TipsterData")
#setwd("~/Scrivania/heidel_data&precision - pulizia")
#setwd("/home/alan/Documents/GIT/Rstuff")
#-------------------------------------------------------------------#
source("FunzioniAnalisi.R")
# source("/home/sally/altracartella/IR_Rstuff/FunzioniAnalisi.R")

startTimer()

data <- carica.details.precision("heidel_details&precision.txt")

# Salvo lo stato iniziale del dataset per poter confrontare i valori tolti
statoiniziale <- stato.dataset(data) 

stampa.stato.dataset(data)

data <- rimuovi.dati.value.nullo()

# stampa.totali.date();

data <- pulisci.anni.poco.frequenti()

stampa.stato.dataset(data, confronto = TRUE) 

salva.dataset("heidel_year.pulito.txt", data)
data <- ricarica.dataset("heidel_year.pulito.txt")

data <- rimuovi.date.undef()

data <- trasforma.undef.day()

stampa.stato.dataset(data, confronto = TRUE) 

salva.dataset("heidel_pulizia.tmp.txt", data)
data <- ricarica.dataset("heidel_pulizia.tmp.txt")

#----- fine scriptPulizia.R -----#
#---- scriptPulizia(cont).R -----#

data <- rimuovi.date.anomale(data)
stampa.stato.dataset(data, confronto = TRUE) 

data <- trasforma.undef.month(data)
stampa.stato.dataset(data, confronto = TRUE) 

data <- rimuovi.quadrimestri.Q0Q5(data)
stampa.stato.dataset(data, confronto = TRUE) 

data <- rimuovi.undef.maggiori.2100(data)
stampa.stato.dataset(data, confronto = TRUE) 

salva.dataset("heidel_pulizia.tmp.txt", data)
data <- ricarica.dataset("heidel_pulizia.tmp.txt")

data <- trasforma.undef.years(data)
stampa.stato.dataset(data, confronto = TRUE) 

data <- rimuovi.years.maggiori200(data)

data <- rimuovi.anni.da.termini.errati(data)

salva.dataset("heidel_pulizia.tmp.txt", data)
data <- ricarica.dataset("heidel_pulizia.tmp.txt")

data <- rimuovi.da.termini.ambigui(data)

data <- trasforma.undef.year(data)
stampa.stato.dataset(data, confronto = TRUE)

#----- Fine pulizia ---- #
check.valori.undefined(data)
salva.dataset("heidel_pulizia.def.txt", data)

stopTimer()

#--- Scrittura per fileR_daDividere ---#

# Uso su un subset
set.seed(1234)
data.originali <- data
data <- data.originali[which(data.originali$id %in% sample(levels(data.originali$id), 10000)),]
data <- droplevels(data)

setwd("/home/sally/altracartella/IR_PARSER")
data.DATE <- data[which(data$type == "DATE"), ]
data.DATE <- droplevels(data.DATE)
str(data.DATE[, -4])

# sep = "\t" -->  <id>\t<type>\t<value>\t<creation>\t<gran>
# sep = " " --> <id> <type> <value> <creation> <gran> (nel parser usa gli spazi  " ")

write.table(data.DATE[, -4], file ="fileR_daDividere.txt" , quote = FALSE, sep = " ", 
            row.names = FALSE, col.names = F)
