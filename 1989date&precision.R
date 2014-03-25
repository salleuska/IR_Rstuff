setwd("~/Scrivania/TipsterData")
heidel.dp <- read.table("~/Scrivania/TipsterData/esiti 1989/heidel_details&precision",
                        header=T, quote="\"")

data <- heidel.dp

# Numero di date uniche
length(unique(data$value[data$type == "DATE"]))
# se li tratto come interi isolo quelle che sono solo anni
anni <- as.integer(unique(data$value[data$type == "DATE"]))
# anni "sbagliati"
sum(anni > 2020)

data[(as.integer(data$value[data$type == "DATE"])>2000), ]

info <- function(data.temp)
{ 
  # data.temp <- vettore espressioni temporali dai dati (come variabili fattoriali)
  t.espr <- levels(data.temp)
  out <- list(list())
  for(i in 1:length(t.espr))
  {
    freq <- dim(data[which(data.temp == t.espr[i]), ])[1]
    terms <- unique(as.character(data$term[which(data.temp == t.espr[i])]))
    out[[t.espr[i]]] <- list(terms = terms, freq = freq)
  }
  out
}
length(data$value[data$type == "DATE"])
# info.date <- info(data$value[data$type == "DATE"])
# save(info.date, file = "espressioniDate1992.RData" )
load("espressioniDate1989.RData")
test <- info.date[1:100]
# print?
sapply(names(test),function(x) paste(x,paste(test[[x]],sep = " ", collapse=" "), collapse = " "))
#-------------------------------#

str(data)
summary(data$gran)

subset <- function(data, gran = "")
{
  # restituisce un sottoinsieme dei dati
  out <- data[which(data$gran == gran), ]
  out <- droplevels(out)  
  out
}

# Solo anno
year <- subset(data, "year")
str(year)
levels(year$value)
year.freq <- summary(year$value, max = Inf)/length(year$value)

hist(as.numeric(year.freq), breaks = 100)
plot(names(year.freq), year.freq, type = "l", xlab = "Anni", ylab =  "Frequenza",
     main = "Distribuzione anni nei documenti - 1989")
