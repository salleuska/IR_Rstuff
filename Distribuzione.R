# Rimane questo blocchetto di codice da controllare all'inzio dello script,
# non ho trovato niente di meglio (o meglio non volevo perderci troppo tempo)
# NOTA: getwd() restituisce la working directory corrente
#------------------------------------------------------------------#
# source("/home/alan/Documents/GIT/Rstuff/configurazione.R")
# config <- set.config(user = "alan")
# source("/home/sally/altracartella/IR_Rstuff/configurazione.R")
# source("configurazione.R")
config
#------------------------------------------------------------------#
source(paste(config[1], "FunzioniAnalisi.R", sep = ""))

startTimer()

# leggo esplosione temporale
setwd(config["txtpath"])
data <- read.table("heidel_dimensioneTemporale.txt", header=F)
data.read <- data
data <- data.read
names(data) <- c("id", "date")
levels(data$id)
data <- subset.data(data=data,  subset=T, ndoc=10)
levels(data$id)
levels(data$date)
summary(data)


data$date <- as.Date(data$date, '%Y-%m-%d')


plot.document <- function(n)
{
  prova <- data[which(data$id == levels(data$id)[n]), ]
  
  library(ggplot2)
  library(scales)
  freqs <- aggregate(prova$date, by=list(prova$date), FUN=length)
  freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")
  
  cat(paste("Dal", min(prova$date), "al", max(prova$date)))
  
  ggplot(freqs, aes(x=names, y=x)) + geom_bar(stat="identity") +
    scale_x_date(breaks="1 month", labels=date_format("%Y-%b"),
                 limits=c(as.Date(min(prova$date)),as.Date(max(prova$date)))) +
    ylab("Frequency") + xlab("Year and Month")
  
  
  
}

plot.document(10)

#




stopTimer()
