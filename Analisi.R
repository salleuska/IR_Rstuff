#setwd("~/Scrivania/heidel_data&precision - pulizia")
#setwd("/home/alan/Documents/GIT/Rstuff")
#-------------------------------------------------------------------#
# source("FunzioniAnalisi.R")
source("/home/sally/altracartella/IR_Rstuff/FunzioniAnalisi.R")

data <- carica.details.precision("heidel_details&precision.txt")

# Salvo lo stato iniziale del dataset per poter confrontare i valori tolti
statoiniziale <- stato.dataset(data) 

stampa.stato.dataset(data)

data <- rimuovi.dati.value.nullo()

stampa.totali.date();

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

data <- rimuovi.date.anomale(data) # CHECK ESPRESSIONE REGOLARE NELLA FUNZIONE

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

check.valori.undefined(data)
salva.dataset("heidel_pulizia.tmp.txt", data)
