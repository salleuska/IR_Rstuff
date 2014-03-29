#-------------------------------------------------------------------#
source("TipsterFunzioni.R")
#-------------------------------------------------------------------#

# Lettura 
carica.details.precision <- function(path) {
  data <- read.table(path, header=T)
  #str(data)
  data
}
#-------------------------------------------------------------------#

# Toglie dati con value nullo (2)
rimuovi.dati.value.nullo <- function() {
  data <- data[-which(data$value == ""), ]
  data <- droplevels(data)
  #str(data)
  data
}


#-------------------------------------------------------------------#
# Da controllare (numero di osservazioni)
stampa.totali.date <- function(){
  # Tipo DATE undefined 
  print(dim(data[which((data$type == "DATE")&(data$gran == "undefined")), ])[1])
  # Tipo DATE & gran = year 
  print(dim(data[which((data$type == "DATE")&(data$gran == "year")), ])[1])
  # Tipo DATE & gran = years 
  print(dim(data[which((data$type == "DATE")&(data$gran == "years")), ])[1])
  # Tipo date & gran = day/days/month/months/ref dovrebbero essere a posto
  print(dim(data[which((data$type == "DATE")&(data$gran != "years")&(data$gran != "year")&(data$gran != "undefined")), ])[1])
  # Tipo DATE totale
  print(dim(data[which(data$type == "DATE"), ])[1])
}

#-------------------------------------------------------------------#
pulisci.anni.poco.frequenti <- function() {
  
  anno <- data[which((data$type == "DATE")&(data$gran == "year")), ]
  anno <- droplevels(anno)
  str(anno)
  as.integer(levels(anno$value)) 
  
  anno.info <- info(anno)
  length(anno.info)
  anno.freq <- matrix(NA, length(names(anno.info)), ncol = 2)
  for(i in 1:length(names(anno.info)))
  {
    anno.freq[i, ] <- c(names(anno.info[i]) , anno.info[[i]]$freq)
    
  }
  str(anno.freq)
  anno.freq <- as.data.frame(anno.freq)
  # frequenze come variabile numerica
  anno.freq$V2 <- as.numeric(levels(anno.freq$V2))[anno.freq$V2]
  # Identifico quelli con una frequenza minore di 0.0001
  anno.drop <-anno.freq[which(anno.freq$V2 < 1e-04), ]$V1
  anno.drop <- droplevels(anno.drop)
  anno.drop
  
  anno.datogliere <- which(levels(data$value) %in% levels(anno.drop))
  oss.datogliere <- which(data$value %in% levels(data$value)[anno.datogliere])
  
  data <- data[-oss.datogliere, ]
  data <- droplevels(data)
  # check
  anno <- data[which((data$type == "DATE")&(data$gran == "year")), ]
  anno <- droplevels(anno)
  str(anno)
  anno.info[levels(anno$value)] 
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
  
  data <- data[-grep("UNDEF", data$value), ]
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
  data[which(data$value %in% new.day), ]$gran <- "day"
  # Tolgo i valori sbagliati
  data <- data[- which(data$value %in% day.drop), ]
  data <- droplevels(data)
  #str(data)
  data
}











