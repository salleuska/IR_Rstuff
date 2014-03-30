data <- read.table("heidel_pulizia.tmp.txt", header=T)
# serve fare il source del file TipsterFunzioni.R per poter utilizzare la funzione info()
# source("~/altracartella/IR_Rstuff/TipsterFunzioni.R")
#------------------------------------#
# Tipo DATE undefined 
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# Possibili mesi
month <- levels(undef$value)[grep("-[0-9]", levels(undef$value))]
month
##########################################################################
# CHECK PER ESPRESSIONE REGOLARE
#-------------------------------------------------------------------------
# valori anomali: valori -2 e -6 e 2427
#-------------------------------------------------------------------------
month[grep("(^-[0-9]$)|(2427)", month)]

undef[which(undef$value %in% month[grep("(^-[0-9]$)|(2427)", month)]), ]

# elimino
data <- data[- which(data$value %in% month[grep("(^-[0-9]$)|(2427)", month)]), ]
data <- droplevels(data)
###################################################################################
#------------------------------#
# Rimangono i possibili mesi
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# Possibili mesi
month <- levels(undef$value)[grep("-[0-9]", levels(undef$value))]
month
# check termini associati
term.month <- data[which(data$value %in% month), ]$term 
term.month #OK
data[which(data$value %in% month), ]$gran <- "month"
#------------------------------------------#
# Quadrimestri
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# check Q - quarters
# q <- levels(undef$value)[grep("^X+-Q[1-4]$", levels(undef$value))]
q <- levels(undef$value)[grep("^[0-9]+-Q[05]$", levels(undef$value))]
q

if(length(which(data$value %in% q)) > 0)
{
  data <- data[-which(data$value %in% q) ,]
}
data <- droplevels(data)

#--------------------------------------------------------------#
# DATE e undefined
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)

year.check <- levels(undef$value)[!is.na(as.integer(levels(undef$value)))]
# anni sbagliati (sopra il 2000)
year.check[which(as.integer(year.check) > 2000 )]
# check termini
term.check <- undef[which(undef$value %in% year.check[which(as.integer(year.check) > 2000 )]), ]$term
# quelli con termini corretti
undef[which((undef$term %in% term.check[grep("[a-z]", term.check)])&(undef$value %in% year.check[which(as.integer(year.check) > 2000 )])), ]
# sono pochi.. via tutti
dim(data[which(data$value %in% year.check[which(as.integer(year.check) > 2000 )]), ])[1]
data <- data[-which(data$value %in% year.check[which(as.integer(year.check) > 2000 )]), ]
data <- droplevels(data)
#---------------------------------------------------#
# backup
write.table(data, file = "heidel_pulizia.tmp.txt")
# pulizia workspace e rilettura
rm(list = ls())
data <- read.table("heidel_pulizia.tmp.txt", header=T)
#---------------------------------------------------#
# DATE e undefined
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# Consideriamo i valori bassi (century/years)

check.century <- suppressWarnings(levels(undef$value)[which(as.integer(levels(undef$value)) < 1000)])
check.century
terms.century <- undef[which(undef$value %in% check.century), ]$term

# quelli del tipo 6000s etc in sospeso
terms.century[grep("^[0-9]+s$", terms.century)]
terms.century <- terms.century[-grep("^[0-9]+s$", terms.century)]

terms.century <- droplevels(terms.century)
terms.century

dim(undef[which(undef$term %in% terms.century), ])[1]
dim(data[which(data$term%in%terms.century), ])[1]
# ma
data[which(data$term%in%terms.century), ]$gran
data[which(data$term%in%terms.century), ]$gran <- "years"
#-------------------------------------------------------#
# backup
write.table(data, file = "heidel_pulizia.tmp.txt")
# pulizia workspace e rilettura
rm(list = ls())
data <- read.table("heidel_pulizia.tmp.txt", header=T)
#---------------------------------------------------#
# years > 2000
years <- data[which((data$type == "DATE")&(data$gran == "years")), ]
years <- droplevels(years)
levels(years$value)
check <- suppressWarnings(levels(years$value)[which(as.integer(levels(years$value)) > 200)])

dim(data[which((data$value %in% check)&(data$type == "DATE")), ])[1]
data[which((data$value %in% check)&(data$type == "DATE")), ]

data <- data[-which((data$value %in% check)&(data$type == "DATE")), ]
data <- droplevels(data)
#--------------------------------------------------------#
# DATE e undefined
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# valori tra 1000e e 2000
check <-suppressWarnings(levels(undef$value)[which((as.integer(levels(undef$value)) > 1000)&(as.integer(levels(undef$value)) < 2000))])
length(check)
# altri (livelli rimasti)
# levels(undef$value)[which(!(levels(undef$value)%in%check))]

# Termini dei valori "solo numeri"
terms.check <- undef[which(undef$value %in% check), ]$term
# termini solo "numeri", c'è da fidarsi che siano date?
numeri.check <- terms.check[grep("^[0-9]{4}$", terms.check)]
numeri.check <- droplevels(numeri.check)
numeri.check
# Intanto si possono togliere quelli con solo due numeri
terms.check <- terms.check[-(which(terms.check %in% numeri.check))]
terms.check <- droplevels(terms.check)
terms.check
terms.check[grep("^[0-9]{2}$", terms.check)]

if(length(which((data$term %in% terms.check[grep("^[0-9]{2}$", terms.check)])&(data$type == "DATE")&(data$gran == "undefined"))) > 0)
{
  data <- data[-which((data$term %in% terms.check[grep("^[0-9]{2}$", terms.check)])&(data$type == "DATE")&(data$gran == "undefined")), ]
}
data <- droplevels(data)
#----------------------------------------------------------------#
# Dagli undefined possibili years (century) 
# Si controllano le frasi
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# termini in sospeso 6000s (cancello)
check.century <- suppressWarnings(levels(undef$value)[which(as.integer(levels(undef$value)) < 1000)])
check.century
terms.century <- undef[which(undef$value %in% check.century), ]$term
# data[which(data$term %in% terms.century), c(1, 3, 4) ]
# write.table(data[which(data$term %in% terms.century), c(1, 3, 4) ], file = "terms_tocheck.txt",
#            row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")

terms.century <- droplevels(terms.century)
terms.delete  <- terms.century

if(length(terms.century[grep("^(1[6789])|20", terms.century)]) > 0)
{
  data[which(data$term %in% terms.century[grep("^(1[6789])|20", terms.century)]),  ]$gran <- "years"
  terms.delete <- terms.century[-grep("^(1[6789])|20", terms.century)]
}

terms.delete <- droplevels(terms.delete)
data <- data[-which((data$term %in% terms.delete)&(data$gran == "undefined")), ]
data <- droplevels(data)

#-------------------------------------------------------#
# backup
write.table(data, file = "heidel_pulizia.tmp.txt")
# pulizia workspace e rilettura
rm(list = ls())
data <- read.table("heidel_pulizia.tmp.txt", header=T)
#---------------------------------------------------#
# DATE e undefined
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# valori tra 1000e e 2000
check <- suppressWarnings(levels(undef$value)[which((as.integer(levels(undef$value)) >= 1000)&(as.integer(levels(undef$value)) < 2000))])
length(check)
# altri
levels(undef$value)[which((levels(undef$value)%in%check))]
data[which(data$value %in% levels(undef$value)[which((levels(undef$value)%in%check))]), ]$gran <- "year"
data <- droplevels(data)
#-----------------------------------------------------------------------------#
# backup finale
write.table(data, file = "heidel_pulizia.def.txt")
# pulizia workspace e rilettura
rm(list = ls())
data <- read.table("heidel_pulizia.tmp.txt", header=T)
#-----------------------------------------------------------------------------#
# Eventuali da controllare
# Tipo DATE gran years
# Alcuni valori provengono da espressioni che potrebbero non indicare decadi (eg. secondi, età)
years <- data[which((data$type == "DATE")&(data$gran == "years")), ]
years <- droplevels(years)
levels(years$value)
# I termini sbagliati saranno probabilmente quelli senza '
levels(years$term)
check <- levels(years$term)[grep("^[0-9]{2}s$", levels(years$term))]

# Frequenza dei termini 
source("~/altracartella/IR_Rstuff/TipsterFunzioni.R")
sec <- years[which(years$term %in% check), ]
sec <- droplevels(sec)
sec.info <- info(sec, terms.frequency= TRUE)
sec.info
# check dimensioni
dim(years[which(years$term %in% check), ])[1]
dim(data[which(data$term %in% check), ])[1]
#-----------------------------------------------------------------------------#
# Tipo TIME check
time <- data[which(data$type == "TIME"), ]
time <- droplevels(time)
levels(time$value)
# Tipo SET check
set <- data[which(data$type == "SET"), ]
set <- droplevels(set)
levels(set$value)
set[which(set$value %in% levels(set$value)[1:10]), ]

# Tipo DURATION check
duration <- data[which(data$type == "DURATION"), ]
duration <- droplevels(duration)
levels(duration$value)

summary(data)

duration[c(1:100), ]
length(levels(duration$id))

# tipo DATE
date <- data[which(data$type == "DATE"), ]
length(which(date$gran != "undefined"))
length(which(date$gran == "undefined"))

