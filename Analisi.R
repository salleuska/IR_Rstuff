# Rimane questo blocchetto di codice da controllare all'inzio dello script,
# non ho trovato niente di meglio (o meglio non volevo perderci troppo tempo)
# NOTA: getwd() restituisce la working directory corrente
#------------------------------------------------------------------#
source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
config <- set.config(user = "alan")
#source("configurazione.R")
#config <- set.config(user = "sally")
config
#------------------------------------------------------------------#
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))

startTimer()

setwd(config[1])
source("merge.R")

setwd(config[3])
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

#---- Scrittura dataset per splitter ----#
# setwd(config[2])
data <- ricarica.dataset("heidel_pulizia.def.txt")

# selezionare subset = T e impostare ndoc = n in 
# subset.data per usare solo un sottonsieme 
# generato da n documenti
data <- subset.data(data)
# data <- subset.data(data,subset= T, ndoc = 1000)

# salva dataset per splitter
setwd(config[3])
write.table(data, file = "data_to_split.txt", sep="\t",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

