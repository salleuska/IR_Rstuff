#-------------------------------------------------------------------#
source("TipsterFunzioni.R")
# source("~/altracartella/IR_Rstuff/TipsterFunzioni.R")
#-------------------------------------------------------------------#

set.config <- function(user) {
  
  if (user == "alan") {
    config[path1] = ""
    config[path2] = ""
    config[path3] = ""
  }
  else {
    config[path1] = ""
    config[path2] = ""
    config[path3] = ""
  }
  config
}


# Lettura 
carica.details.precision <- function(path) {
  data <- read.table(path, header=T)
  #str(data)
  data
}

#-------------------------------------------------------------------#

stato.dataset <- function(data)
{
  totale.espressioni <- dim(data)[1]
  numero.DATE <- dim(data[which(data$type == "DATE"), ])[1]
  # totale.TIME <- dim(data[data[which(data$type == "TIME"), ])[1]
  # totale.SET <- dim(data[data[which(data$type == "SET"), ])[1]
  # totale.DURATION <- dim(data[data[which(data$type == "DURATION"), ])[1]
  # totale.undefined <- dim(data[which(data$gran == "undefined"), ])[1]
  numero.undefined.DATE <- dim(data[which((data$type == "DATE")&(data$gran == "undefined")), ])[1]
  stato <- list(totale.espressioni = totale.espressioni,
                numero.DATE =  numero.DATE,
                numero.undefined.DATE = numero.undefined.DATE)
}
# Toglie dati con value nullo (2)
rimuovi.dati.value.nullo <- function() {
  data <- data[-which(data$value == ""), ]
  data <- droplevels(data)
  #str(data)
  data
}
#-------------------------------------------------------------------#
# Nota (se proprio farla stampare un po meglio)
stampa.stato.dataset <- function(data, confronto = FALSE, ...)
{
  stato <- stato.dataset(data) 
  
  if(confronto)
  {
    statoiniziale <- statoiniziale
    cat("------------------------------------------------------------------------------- \n")
    cat("Numero totale espressioni \n")
    cat("Iniziale", "Corrente", "Diff \n", sep = "\t")
    cat(statoiniziale$totale.espressioni, stato$totale.espressioni,
        statoiniziale$totale.espressioni - stato$totale.espressioni,"\n" ,sep = "\t")
    cat("------------------------------------------------------------------------------- \n")
    cat("Numero totale di tipo DATE \n")
    cat("Iniziale", "Corrente", "Diff \n", sep = "\t")
    cat(statoiniziale$numero.DATE, stato$numero.DATE,
        statoiniziale$numero.DATE -  stato$numero.DATE,"\n" , sep = "\t")
    cat("------------------------------------------------------------------------------- \n")
    cat("Numero di undefined di tipo DATE \n")
    cat("Iniziale", "Corrente", "Diff \n", sep = "\t")
    cat(statoiniziale$numero.undefined.DATE, stato$numero.undefined.DATE,
        statoiniziale$numero.undefined.DATE - stato$numero.undefined.DATE, "\n" , sep = "\t")
    cat("------------------------------------------------------------------------------- \n")  
  }
  else
  {
    cat("------------------------------------------------------------------------------- \n")
    cat("Numero totale espressioni \t")
    cat(stato$totale.espressioni, "\n")
    cat("------------------------------------------------------------------------------- \n")
    cat("Numero di totale di tipo DATE \t")
    cat(stato$numero.DATE,"\n")
    cat("------------------------------------------------------------------------------- \n")
    cat("Numero di undefined di tipo DATE \t")
    cat(stato$numero.undefined.DATE, "\n" )
    cat("------------------------------------------------------------------------------- \n")
  }  
}

#-------------------------------------------------------------------#

check.valori.undefined <- function(data)
{
  undef <- droplevels(data[which(data$gran == "undefined"), ])
  cat("------------------------------------------------------------------------------- \n")
  cat("Numero undefined totali" , length(undef$value), "\n", sep = " \t ")
  cat("------------------------------------------------------------------------------- \n")
  cat("Numero undefined tipo DATE" , length(undef[which(undef$type == "DATE"), ]$value), "\n", sep = " \t ")
  cat( "Valori associati \t",levels(droplevels(undef[which(undef$type == "DATE"), ]$value)), " \n")
  cat("------------------------------------------------------------------------------- \n")
}

# Da controllare (numero di osservazioni)
stampa.totali.date <- function(){
  cat("------------------------------------------------------------------------------- \n")
  cat("Numero di undefined di tipo DATE \t")
  cat(dim(data[which((data$type == "DATE")&(data$gran == "undefined")), ])[1], "\n")
  cat("------------------------------------------------------------------------------- \n")
  cat("Numero di year di tipo DATE \t")
  cat(dim(data[which((data$type == "DATE")&(data$gran == "year")), ])[1], "\n")
  cat("------------------------------------------------------------------------------- \n")
  cat("Numero di years di tipo DATE \t") 
  cat(dim(data[which((data$type == "DATE")&(data$gran == "years")), ])[1], "\n")
  cat("------------------------------------------------------------------------------- \n")
  cat("Numero di day/days/month/months/ref di tipo DATE (da non controllare) \t")
  cat(dim(data[which((data$type == "DATE")&(data$gran != "years")&(data$gran != "year")&(data$gran != "undefined")), ])[1], "\n")
  cat("------------------------------------------------------------------------------- \n")
  cat("Numero di totale di tipo DATE \t")
  cat(dim(data[which(data$type == "DATE"), ])[1], "\n")
  cat("------------------------------------------------------------------------------- \n")
}

#-------------------------------------------------------------------#

# Toglie dati con value nullo (2)
rimuovi.dati.value.nullo <- function() {
  if(length(which(data$value == "") > 0))
  {  
    cat("Numero di osservazioni eliminate \t")
    cat(dim(data[which(data$value == ""), ])[1])
    data <- data[-which(data$value == ""), ]
  }
  else cat("Niente da eliminare \n")
  data <- droplevels(data)
  
  data
}


#-------------------------------------------------------------------#
pulisci.anni.poco.frequenti <- function() {
  
  anno <- data[which((data$type == "DATE")&(data$gran == "year")), ]
  anno <- droplevels(anno)
  
  as.integer(levels(anno$value)) 
  
  anno.info <- info(anno)
  length(anno.info)
  anno.freq <- matrix(NA, length(names(anno.info)), ncol = 2)
  for(i in 1:length(names(anno.info)))
  {
    anno.freq[i, ] <- c(names(anno.info[i]) , anno.info[[i]]$freq)
    
  }
  anno.freq <- as.data.frame(anno.freq)
  # frequenze come variabile numerica
  anno.freq$V2 <- as.numeric(levels(anno.freq$V2))[anno.freq$V2]
  # Identifico quelli con una frequenza minore di 0.0001
  anno.drop <-anno.freq[which(anno.freq$V2 < 1e-04), ]$V1
  anno.drop <- droplevels(anno.drop)
  anno.drop
  
  anno.datogliere <- which(levels(data$value) %in% levels(anno.drop))
  oss.datogliere <- which(data$value %in% levels(data$value)[anno.datogliere])
  
  if(length(oss.datogliere) > 0)
  {
    cat("Numero di osservazioni eliminate \t")
    cat(dim(data[oss.datogliere, ])[1])
    data <- data[-oss.datogliere, ]
  }  
  else cat("Niente da eliminare \n")
  
  data <- droplevels(data)
  
  data
}

#-------------------------------------------------------------------#

salva.dataset <- function(path, dataset) {
  # backup
  write.table(dataset, file = path)
  # pulizia workspace e rilettura
}

#-------------------------------------------------------------------#

ricarica.dataset <- function(path) {
  dataset <- carica.details.precision(path)
  dataset
}

#-------------------------------------------------------------------#

rimuovi.date.undef <- function() {
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
  #str(undef)
  #levels(undef$value)
  # Gestione valori "UNDEF-REF"
  #dim(undef[grep("UNDEF", undef$value), ])[1]
  #dim(data[grep("UNDEF", data$value), ])[1]
  #data[grep("UNDEF", data$value), ]
  if(length(grep("UNDEF", data$value)) > 0)
  {
    cat("Numero di osservazioni eliminate \t")
    cat(dim(data[grep("UNDEF", data$value), ])[1])
    data <- data[-grep("UNDEF", data$value), ]
  }
  else cat("Niente da eliminare")
  data <- droplevels(data)
  #str(data)
  data
}



#-------------------------------------------------------------------#

trasforma.undef.day <- function() {
  
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
  #levels(undef$value)
  library(lubridate)
  day.check <- levels(undef$value)[which(!is.na(ymd(levels(undef$value))))]
  day.drop <- day.check[which((ymd(day.check) > ymd("2000-01-01"))|(ymd(day.check) < ymd("1300-01-01")))]
  
  # Aggiunta gran = day per i valori mappati giusti
  new.day <- day.check[!(day.check %in% day.drop)]
  cat("Numero osservazioni rinominate \t")
  cat(dim(data[which(data$value %in% new.day), ])[1], "\n")
  data[which(data$value %in% new.day), ]$gran <- "day"
  # Tolgo i valori sbagliati
  
  if(length(which(data$value %in% day.drop)) > 0)
  {
    cat("Numero di osservazioni eliminate \t")
    cat(dim(data[which(data$value %in% day.drop), ])[1])
    data <- data[- which(data$value %in% day.drop), ]
  }
  else cat("Niente da eliminare")
  
  data <- droplevels(data)
  #str(data)
  data
}

#-------------------------------------------------------------------#

rimuovi.date.anomale <- function(data)
{
  # Tipo DATE undefined 
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
  # levels(undef$value)
  
  # Possibili mesi
  month <- levels(undef$value)[grep("-[0-9]", levels(undef$value))]
  # month
  
  #--------------------------------------------------
  # valori anomali: valori -2 e -6 e 2427-09
  #--------------------------------------------------
  regex <- "^(-2)|(-6)|(2427-09)$" # solo -2, -6 o 2427
  # regex <- "^(-[0-9])|(2427)$" # da -0 a -9 o 2427
  
  # month[grep(regex, month)]
  # undef[which(undef$value %in% month[grep(regex, month)]), ]
  
  # elimino
  if(length(which(data$value %in% month[grep(regex, month)])) > 0)
  {
    cat("Numero di osservazioni eliminate \t")
    cat(dim(data[which(data$value %in% month[grep(regex, month)]), ])[1])
    data <- data[- which(data$value %in% month[grep(regex, month)]), ]
    
  }
  else cat("Niente da eliminare")
  
  data <- droplevels(data)
  data
}


#-------------------------------------------------------------------#

trasforma.undef.month <- function(data)
{  
  # Rimangono i possibili mesi
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
  
  # levels(undef$value)
  # Possibili mesi
  month <- levels(undef$value)[grep("-[0-9]", levels(undef$value))]
  # month
  # check termini associati
  term.month <- data[which(data$value %in% month), ]$term 
  term.month #OK
  cat("Numero osservazioni rinominate \t")
  cat(dim(data[which(data$value %in% month), ])[1])
  data[which(data$value %in% month), ]$gran <- "month"
 
  data
}  

#-------------------------------------------------------------------#
rimuovi.quadrimestri.Q0Q5 <- function(data)
{
  # Quadrimestri : elimina Q0 e Q5
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
  levels(undef$value)
  # check Q - quarters
  # q <- levels(undef$value)[grep("^X+-Q[1-4]$", levels(undef$value))]
  q <- levels(undef$value)[grep("^[0-9]+-Q[05]$", levels(undef$value))]
  
  if(length(which(data$value %in% q)) > 0)
  {
    cat("Numero osservazioni eliminate \t")
    cat(dim(data[which(data$value %in% q) ,])[1])    
    data <- data[-which(data$value %in% q) ,]
  }
  data <- droplevels(data)
  data
}

#-------------------------------------------------------------------#

rimuovi.undef.maggiori.2100 <- function(data)
{  
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
  levels(undef$value)
  
  # suppressWarnings ignora i warnings (NA per coercizione voluto)
  year.check <- suppressWarnings(levels(undef$value)[!is.na(as.integer(levels(undef$value)))])
  
  # anni sbagliati (sopra il 2000)
  year.check[which(as.integer(year.check) > 2000 )]
  # check termini
  term.check <- undef[which(undef$value %in% year.check[which(as.integer(year.check) > 2000 )]), ]$term
  # quelli con termini "corretti"
  undef[which((undef$term %in% term.check[grep("[a-z]", term.check)])&(undef$value %in% year.check[which(as.integer(year.check) > 2000 )])), ]
  # sono pochi.. via tutti
  # controllo
  if(length(which(data$value %in% year.check[which(as.integer(year.check) > 2000 )])) > 0) 
  {
    cat("Numero osservazioni eliminate \t")
    cat(dim(data[which(data$value %in% year.check[which(as.integer(year.check) > 2000 )]), ])[1])
    data <- data[-which(data$value %in% year.check[which(as.integer(year.check) > 2000 )]), ]
  }
  else cat("Niente da eliminare")
  
  data <- droplevels(data)
  data
}

#-------------------------------------------------------------------#

trasforma.undef.years <- function(data)
{
  # DATE e undefined
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
  levels(undef$value)

  # Consideriamo i valori bassi (century/years)
  check.century <- suppressWarnings(levels(undef$value)[which(as.integer(levels(undef$value)) < 1000)])
  check.century
  terms.century <- undef[which(undef$value %in% check.century), ]$term
  
  # quelli del tipo 6000s etc in sospeso
  if(length(terms.century[grep("^[0-9]+s$", terms.century)]) > 0)
  {
    terms.century <- terms.century[-grep("^[0-9]+s$", terms.century)]
  }
  terms.century <- droplevels(terms.century)
  terms.century
  
  cat("Numero osservazioni rinominate \t")
  cat(dim(data[which(data$term%in%terms.century), ])[1])
  # ma
  data[which(data$term%in%terms.century), ]$gran <- "years"

  data
}

#-------------------------------------------------------------------#

rimuovi.years.maggiori200 <- function(data)
{ 
  years <- data[which((data$type == "DATE")&(data$gran == "years")), ]
  years <- droplevels(years)
  levels(years$value)
  check <- suppressWarnings(levels(years$value)[which(as.integer(levels(years$value)) > 200)])
  
  dim(data[which((data$value %in% check)&(data$type == "DATE")), ])[1]
  data[which((data$value %in% check)&(data$type == "DATE")), ]

  if(length(which((data$value %in% check)&(data$type == "DATE"))) > 0)
  {
    cat("Numero osservazioni eliminate \t")    
    cat(cat(dim(data[which((data$value %in% check)&(data$type == "DATE")), ])[1]))
    data <- data[-which((data$value %in% check)&(data$type == "DATE")), ]
  }
  else cat("Niente da eliminare")
  
  data <- droplevels(data)
  
  data
}

#-------------------------------------------------------------------#

rimuovi.anni.da.termini.errati <- function(data)
  {# DATE e undefined
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
  
  levels(undef$value)
  # valori tra 1000e e 2000
  check <-suppressWarnings(levels(undef$value)[which((as.integer(levels(undef$value)) > 1000)&(as.integer(levels(undef$value)) < 2000))])
  length(check)
  # altri (livelli rimasti)
  # levels(undef$value)[which(!(levels(undef$value)%in%check))]
  
  # Termini dei valori "solo numeri"
  terms.check <- undef[which(undef$value %in% check), ]$term
  # termini solo "numeri"
  numeri.check <- terms.check[grep("^[0-9]{4}$", terms.check)]
  numeri.check <- droplevels(numeri.check)
  # numeri.check

  # Intanto si possono togliere quelli con solo due numeri
  terms.check <- terms.check[-(which(terms.check %in% numeri.check))]
  terms.check <- droplevels(terms.check)
  # terms.check
  # terms.check[grep("^[0-9]{2}$", terms.check)]
  
  if(length(which((data$term %in% terms.check[grep("^[0-9]{2}$", terms.check)])&(data$type == "DATE")&(data$gran == "undefined"))) > 0)
  {
    cat("Numero osservazioni eliminate \t")
    cat(dim(data[which((data$term %in% terms.check[grep("^[0-9]{2}$", terms.check)])&(data$type == "DATE")&(data$gran == "undefined")), ])[1])
    data <- data[-which((data$term %in% terms.check[grep("^[0-9]{2}$", terms.check)])&(data$type == "DATE")&(data$gran == "undefined")), ]
  }
  else cat("Niente da eliminare")
  
  data <- droplevels(data)
  
  data
}

#-------------------------------------------------------------------#

rimuovi.da.termini.ambigui <- function(data)
{
  # Dagli undefined possibili years (century) 
  # Si controllano le frasi  
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
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
  
  if(length(terms.delete) > 0)
  {
    if(length(terms.century[grep("^(1[6789])|20", terms.century)]) > 0)
    {
      cat("Numero osservazioni rinominate \t")
      cat(dim(data[which(data$term %in% terms.century[grep("^(1[6789])|20", terms.century)]),  ])[1])
      data[which(data$term %in% terms.century[grep("^(1[6789])|20", terms.century)]),  ]$gran <- "years"
      terms.delete <- terms.century[-grep("^(1[6789])|20", terms.century)]
    }
    
    terms.delete <- droplevels(terms.delete)
    if(length(which((data$term %in% terms.delete)&(data$gran == "undefined"))) > 0)
    {    
      cat("Numero osservazioni eliminate \t")
      cat(dim(data[which((data$term %in% terms.delete)&(data$gran == "undefined")), ])[1])
      data <- data[-which((data$term %in% terms.delete)&(data$gran == "undefined")), ]
    }
    else cat("Niente da eliminare")
  }
  data <- droplevels(data)
  data
}

#-------------------------------------------------------------------#
trasforma.undef.year <- function(data)
{
  # DATE e undefined
  undef <- data[which((data$type == "DATE")&(data$gran == "undefined")), ]
  undef <- droplevels(undef)
  # levels(undef$value)
  
  # valori tra 1000e e 2000
  check <- suppressWarnings(levels(undef$value)[which((as.integer(levels(undef$value)) >= 1000)&(as.integer(levels(undef$value)) < 2000))])
  
  cat("Numero osservazioni rinominate \t")
  cat(dim(data[which(data$value %in% levels(undef$value)[which((levels(undef$value)%in%check))]), ])[1])
  data[which(data$value %in% levels(undef$value)[which((levels(undef$value)%in%check))]), ]$gran <- "year"
  
  data <- droplevels(data)

  data
}

#-------------------------------------------------------------------#
