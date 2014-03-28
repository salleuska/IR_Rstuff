data <- read.table("heidel_pulizia.tmp.txt", header=T)
# serve fare il source del file TipsterFunzioni.R per poter utilizzare la funzione info()
# source("~/altracartella/IR_Rstuff/TipsterFunzioni.R")
#------------------------------------#
# Tipo DATE undefined 
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
#
month <- levels(undef$value)[grep("-[0-9]", levels(undef$value))]
# valori -2 e -6 e 2427
undef[which(undef$value == month[1]), ]
undef[which(undef$value == month[2]), ]
undef[which(undef$value == month[44]), ]

data[which(data$id %in% undef[which(undef$value == month[1]), ]$id), ]
data[which(data$id %in% undef[which(undef$value == month[2]), ]$id), ]
data[which(data$id %in% undef[which(undef$value == month[44]), ]$id), ]

# elimino
data <- data[-which(data$value %in% month[1]), ]
data <- data[-which(data$value %in% month[2]), ]
data <- data[-which(data$value %in% month[44]), ]

data <- droplevels(data)

# Rimangono i possibili mesi
month <- month[-c(1, 2, 44)]
# check termini associati
term.month <- data[which(data$value %in% month), ]$term 
term.month #OK
data[which(data$value %in% month), ]$gran <- "month"
#------------------------------------------#
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# check Q - quarters
q <-levels(undef$value)[grep("-[0-9Q3]", levels(undef$value))]
data[which(data$value %in% q[1]) ,]$gran <- "months"
#---------------------------------------------------#
# Check seasons SP SU WI (AU?)
levels(undef$value)[grep("[SAUWI]", levels(undef$value))]
undef[which(undef$value %in% levels(undef$value)[grep("[SAUWI]", levels(undef$value))]), ]$term
data[which(data$value %in% levels(undef$value)[grep("[SAUWI]", levels(undef$value))]), ]$gran <- "months"
#---------------------------------------------------#
#---------------------------------------------------#
# Tipo DATE gran years
# ALCUNI VALORI VENGONO DA ESPRESSIONI CHE INDICANO SECONDI E NON DECADI
years <- data[which((data$type == "DATE")&(data$gran == "years")), ]
years <- droplevels(years)

levels(years$value)
# I termini sbagliati saranno probabilmente quelli senza '
check <- levels(years$term)[grep("^(\\d{2})[^\']*s$", levels(years$term))]
check <- check[-c(10:18, 20)]  
check
# check dimensioni
dim(years[which(years$term %in% check), ])[1]
dim(data[which(data$term %in% check), ])[1]
# pulizia 
data <- data[-which(data$term %in% check), ]
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
# DATE e undefined
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# Consideriamo i valori bassi (century/years)
check.century <-levels(undef$value)[which(as.integer(levels(undef$value)) < 1000)]
check.century
terms.century <- undef[which(undef$value %in% check.century), ]$term
# quelli del tipo 6000s etc in sospeso
terms.century[grep("^(\\d{2})[^\']*s$", terms.century)]
terms.century <- terms.century[-grep("^(\\d{2})[^\']*s$", terms.century)]
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
#----------------------#
# Questioni in sospeso #
#----------------------#
# DATE e undefined
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
# valori tra 1000e e 2000
check <-levels(undef$value)[which((as.integer(levels(undef$value)) > 1000)&(as.integer(levels(undef$value)) < 2000))]
# altri
levels(undef$value)[which(!(levels(undef$value)%in%check))]

terms.check <- undef[which(undef$value %in% check), ]$term
# termini solo "numeri", c'è da fidarsi che siano date??
numeri.check <- terms.check[grep("^(\\d{4})*$", terms.check)]
numeri.check <- droplevels(numeri.check)
terms.check <- terms.check[-(which(terms.check %in% numeri.check))]
terms.check <- droplevels(terms.check)
#-----------------------------#
# Questioni in sospeso
# Q0 e Q5??
data[which(data$value %in% levels(data$value)[grep("Q[05]", levels(data$value))]), ]
# XXXX tipo DATE da cancellare??
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
xxx <- levels(undef$value)[grep("XXX", levels(undef$value))]
# decadi 1990s (lunghe)
years <- data[which((data$type == "DATE")&(data$gran == "years")), ]
years <- droplevels(years)

levels(years$value)
# I termini sbagliati saranno probabilmente quelli senza '
check <- levels(years$term)[grep("^(\\d{2})[^\']*s$", levels(years$term))]
#-----------#
# years > 2000
years <- data[which((data$type == "DATE")&(data$gran == "years")), ]
years <- droplevels(years)
levels(years$value)
check <- levels(years$value)[which(as.integer(levels(years$value)) > 200)]
years[which(years$value %in% check), ]
# years < 1000
undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
undef <- droplevels(undef)
str(undef)
levels(undef$value)
# termini in sospeso 6000s (cancello?)
check.century <-levels(undef$value)[which(as.integer(levels(undef$value)) < 1000)]
check.century
terms.century <- undef[which(undef$value %in% check.century), ]$term
#----------------------------------------#
# Tipo TIME
time <- data[which(data$type == "TIME"), ]
time <- droplevels(time)
levels(time$value)
# inutili?
timexx <- levels(time$value)[grep("XXXX", levels(time$value))]
time[which(time$value %in% timexx), ]

# prova

time <- data[which(data$type == "TIME"), ]
time <- droplevels(time)
not.time <- data[which(!(data$type == "TIME")), ]
dim(nottime)[1] + dim(time)[1]

day.new <- as.Date(levels(time$value))

day.new <- day.new[which(!is.na(day.new))]
as.character(day.new)

prova <- data
levels(droplevels(prova[which(prova$type == "TIME"), ]$value))
timetoday <- levels(droplevels(prova[which(prova$type == "TIME"), ]$value))
levels(prova[which(prova$type == "TIME"), ]$value) <- levels(prova[which(prova$type == "TIME"), ]$value)
as.character(
  as.Date(timetoday)
  