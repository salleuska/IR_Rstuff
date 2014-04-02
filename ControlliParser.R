# Rimane questo blocchetto di codice da controllare all'inzio dello script,
# non ho trovato niente di meglio (o meglio non volevo perderci troppo tempo)
# NOTA: getwd() restituisce la working directory corrente
#------------------------------------------------------------------#
# source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
# config <- set.config(user = "alan")
source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
config <- set.config(user = "sally")
config
#------------------------------------------------------------------#
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))
#--- Prove per controlli Espansione -- #
setwd(config[2])
data <- ricarica.dataset("heidel_pulizia.def.txt")
data <- subset.data(data, subset= T, ndoc= 1000)
# salva dataset per splitter
setwd(config[3])
write.table(data, file = "data_to_split.txt", sep="\t",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

# Esecuzione di EpifaniaTermini.DiviFileR e parser.DimensioneTemporaleFiltered
# Controlli
check.espansione <- function(data)
{
  data.DATE <- droplevels(data[which(data$type == "DATE"), ])
  cat("Totale Periodi \t")
  cat(dim(data.DATE )[1], "\n")
  cat("Totale Periodi non definiti \t")
  cat(dim(data.DATE[which((data.DATE$gran == "ref")|(data.DATE$gran == "undefined")), ])[1], "\n")
  cat("Ref \t")
  cat(dim(data.DATE[which(data.DATE$gran == "ref"), ])[1], "\n")
  cat("Undefined \t")
  cat(dim(data.DATE[which(data.DATE$gran == "undefined"), ])[1], "\n")
}

check.espansione(data)

# Seconda prova: file scritti da R e sola espansione 

write.files <- function(data)
{
  file.name = paste(data$id[1],"_",data$creation[1], ".txt", sep = "") 
  subset.to.write <- data[, c("type", "value", "gran")]
  write.table(subset.to.write, file = file.name, sep = "\t",
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

setwd("/home/sally/Documents/HeidelCollection/R_puliti_divisi")
library(plyr)
d_ply(data, "id", write.files)
# Sembra un problema di conteggio del parser

# Controlo log (anche qui conteggi che non tornano)
dt.log <- read.delim(paste(config[3], "heidel_dimensioneTemporale.txt.log", sep = ""),
                     header = F)
str(dt.log)
sum(dt.log$V1== "ref")
sum(dt.log$V1== "undefined")
dt.log[which(dt.log$V1== "undefined"), ]

# controllo che siano di tipo giusto nel dataset che ho fatto scrivere ad R
# (per comodità prendo già il sottoinsieme di DATE)

data.DATE <- droplevels(data[which(data$type == "DATE"), ])
data.DATE[which(((data.DATE$value %in% levels(dt.log[which(dt.log$V1== "undefined"), ]$V2))
                 &(data.DATE$gran != "ref"))&(data.DATE$gran != "undefined")), ]
# fatto check dei documenti scritti; granularità esatta

#--- Prova in cui dovrebbero tornare i conteggi (e invece no)  ---#
# se tolgo quegli anni appena visti dovrebbe funzionare
prova <- droplevels(data.DATE[-which(((data.DATE$value %in% levels(dt.log[which(dt.log$V1== "undefined"), ]$V2))
                                      &(data.DATE$gran != "ref"))&(data.DATE$gran != "undefined")), ])
str(prova)
# Svuotare la cartella R_puliti_divisi
setwd("/home/sally/Documents/HeidelCollection/R_puliti_divisi")
library(plyr)
d_ply(prova, "id", write.files)
# ------------------- Parser ---------------------------#
Documento 984 / 984 (limit: 0)  0 m 5 s 265 u 792000 n

Totale assoluto date memorizzate: 1579071
Totale assoluto periodi non definiti: 1374
di cui months:    0
di cui ref:       1339
di cui undefined: 35
di cui default:   0
diff:             0

Totale periodi: 7742
di cui ok:   6368
di cui ndef: 1374
diff:        0
#-------------------------------------------------------#
check.espansione(prova)

# Controllo log
dt.log <- read.delim(paste(config[3], "heidel_dimensioneTemporale.txt.log", sep = ""),
                     header = F)
str(dt.log)
sum(dt.log$V1== "ref")
sum(dt.log$V1== "undefined")
dt.log[which(dt.log$V1== "undefined"), ]
