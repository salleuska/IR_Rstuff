# Estrazione espressione - frequenza - termini associati
# data.temp <- vettore espressioni temporali dai dati (come variabili fattoriali)

info <- function(data.temp)
{
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
#----------------------------------------------------------------------#
typefor <- function(tipo, valore, plot = FALSE)
{
  par(mfrow = c(2, 2))
  #e.g. per ogni documento voglio il numero di espressioni per tipo
  # tipo = data$id, valore = data$type
  tipo.per.valore <- tapply(tipo, valore, summary, maxsum = Inf)
  for(i in 1:length(unique(valore)))
  {
    d <- data.frame(tipo.per.valore[[i]])
    
    cat("Distribuzione nei documenti", levels(data$type)[i], "\n")
    print(summary(d))
    cat("******************* \n")
    if(plot == TRUE)
    {
      hist(d$tip, breaks = 100, prob = T, main= paste("distribution", unique(data$type)[i]))
    }
  }
}
