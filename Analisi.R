#setwd("~/Scrivania/heidel_data&precision - pulizia")
#setwd("/home/alan/Documents/GIT/Rstuff")
#-------------------------------------------------------------------#
source("FunzioniAnalisi.R")

data <- carica.details.precision("heidel_details&precision.txt")

data <- rimuovi.dati.value.nullo()

stampa.totali.date();

data <- pulisci.anni.poco.frequenti()

salva.dataset("heidel_year.pulito.txt", data)
data <- ricarica.dataset("heidel_year.pulito.txt")

data <- rimuovi.date.undef()

data <- trasforma.undef.day()

salva.dataset("heidel_pulizia.tmp.txt", data)
data <- ricarica.dataset("heidel_pulizia.tmp.txt")

#----- fine scriptPulizia.R -----#
#---- scriptPulizia(cont).R -----#










