# Rimane questo blocchetto di codice da controllare all'inzio dello script,
# non ho trovato niente di meglio (o meglio non volevo perderci troppo tempo)
# NOTA: getwd() restituisce la working directory corrente
#------------------------------------------------------------------#
source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
config <- set.config(user = "alan")
#source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
#config <- set.config(user = "sally")
#config
#------------------------------------------------------------------#
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))

#--------------------------------------------------------------------------#
setwd(config[3])
data <- ricarica.dataset("heidel_pulizia.def.txt")

# selezionare subset = T e impostare ndoc = n in 
# subset.data per usare solo un sottonsieme 
# generato da n documenti
#data <- subset.data(data)
data <- subset.data(data,subset= T, ndoc = 10000)

#set.seed(1234) #fissa il numero di partenza per il sample (in modo da poter riestrarre "casualmente" gli stessi dati)
doc <- data[which(data$id %in% sample(levels(data$id), 1)),]
doc <- droplevels(doc)
doc

doc.tmp <- doc

# cerco type con maggior frequenza
summary(doc$type)
t <- summary(doc$type)/length(doc$type)
freq.type.max <- max(t)
freq.type.max

frequenza.type <- function(doc, type)
{
  d <- doc[which(doc$type == type), ]
  d <- droplevels(d)
  #summary(d$type)
  t <- summary(d$type)/length(doc$type)
  freq <- max(t)
  freq
}

frequenza.date.gran <- function(doc, gran)
{
  d <- doc[which(doc$type == "DATE"), ]
  d <- d[which(d$gran == gran), ]
  d <- droplevels(d)
  #summary(d$type)
  t <- summary(d$gran)/length(doc$gran)
  freq <- max(t)
  freq
}

frequenza.date.ref <- function(doc, ref)
{
  d <- doc[which(doc$type == "DATE"), ]
  d <- d[which(d$gran == "ref"), ]
  d <- d[which(d$value == ref), ]
  d <- droplevels(d)
  #summary(d$type)
  t <- summary(d$value)/length(doc$value)
  freq <- max(t)
  freq
}

freq.date <- frequenza.type(doc, "DATE")
freq.date

freq.time <- frequenza.type(doc, "TIME")
freq.time

freq.set <- frequenza.type(doc, "SET")
freq.set

freq.duration <- frequenza.type(doc, "DURATION")
freq.duration

analisi.date.ref <- function(doc)
{
  doc.data <- droplevels(doc[which(doc$type == "DATE"), ])
  doc.ref <- droplevels(doc.data[which(doc.data$gran == "ref"), ])
  
  freq.past <- frequenza.date.ref(doc.ref, "PAST_REF")
  freq.present <- frequenza.date.ref(doc.ref, "PRESENT_REF")
  freq.future <- frequenza.date.ref(doc.ref, "FUTURE_REF")
  
  t <- summary(doc.ref$value)/length(doc.ref$value)
  freq.ref.max <- max(t)
  freq.ref.max
  
  if (freq.past == freq.ref.max)
  {
    print("DOCUMENTO DEL PASSATO")
  }
  if (freq.present == freq.ref.max)
  {
    print("DOCUMENTO ATTUALE")
  }
  if (freq.future == freq.ref.max)
  {
    print("DOCUMENTO DEL FUTURO")
  }
}

analisi.date <- function(doc)
{
  # cerco date$gran con maggior frequenza
  doc.data <- doc[which(doc$type == "DATE"), ]
  summary(doc.data$gran)
  t <- summary(doc.data$gran)/length(doc$gran)
  freq.gran.max <- max(t)
  freq.gran.max
  
  freq.ref <- frequenza.date.gran(doc, "ref")
  freq.undef <- frequenza.date.gran(doc, "undefined")
  
  
  if (freq.ref > 0.5)
  {
    print("ref HA frequenza massima")
    
    analisi.date.ref(doc.data)
    
    
  } else
  {
    print("ref NON HA frequenza massima")
  }
}


# verifico se type=DATE è quello maggiormente frequente
if (freq.date == freq.type.max)
{
  print("date HA frequenza massima")
  analisi.date(doc)
} else
{
  print("date NON HA frequenza massima")
}













