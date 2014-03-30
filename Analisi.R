#setwd("~/Scrivania/heidel_data&precision - pulizia")
#setwd("/home/alan/Documents/GIT/Rstuff")
#-------------------------------------------------------------------#
# source("FunzioniAnalisi.R")
source("/home/sally/altracartella/IR_Rstuff/FunzioniAnalisi.R")

data <- carica.details.precision("heidel_details&precision.txt")

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







