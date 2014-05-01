#-------------------------------------------------------------------#
source(paste(config[1], "TipsterFunzioni.R", sep = "")) # per start/stopTimer()
#-------------------------------------------------------------------#
# Calcolo frequenze pesate
prop.table.weighted <- function(table, w)
{
  for(i in 1:dim(table)[2])
  {
    table[, i] <- table[, i]*w[i]
  }
  prop.table(table, margin = 1)  
}
#------------------------------------------------------#
list.to.data.frame <- function(lista, colnames = NULL)
{
  # trasforma una lista di liste in un dataframe
  lista.data <- data.frame(cbind(names(lista), as.vector(sapply(lista, "[[", 1))))
  colnames(lista.data)<-colnames
  lista.data$Freq <- as.vector(sapply(lista, "[[", 2))
  
  droplevels(lista.data)
}
#------------------------------------------------------#
# Funzione che individua il tipo di espressione con frequenza massima, la mette in una lista
# insieme alla relativa frequenza pesata. Se ci sono dei ties vengono ritornati tutti
max.freq <- function(table)
{
  max <- apply(table, 1, function(x) list(colnames(table)[which(x == max(x))], max(x)))
  max  
}
#------------------------------------------------------#
ties <- function(lista)
{
  cond <- sapply((sapply(lista, "[[", 1)), function(x) length(x) > 1)
  cat("Numero di documenti con ties ", length(lista[cond]))
  lista[cond]
}  

#------------------------------------------------------#

tag <- function(data, id, tag, score)
{
  # Funzione che aggiorna i tag della classificazione
  # id: vettore degli id dei documenti da classificare tramite il tag
  
  data[which(data$id %in% id), ]$class <- tag
  data[which(data$id %in% id), ]$score <- score
  
  data  
}

#------------------------------------------------------#
stato.classificazione <- function() sort(summary(classificazione$class, maxsum=15), decreasing=T)

