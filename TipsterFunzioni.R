# Estrazione espressione - frequenza - termini associati
# data.temp <- vettore espressioni temporali dai dati (come variabili fattoriali)

info <- function(data, terms.frequency = FALSE)
{
  # data ha value e term
  t.espr <- levels(data$value)
  out <- list()
  for(i in 1:length(t.espr))
  {
    freq <- dim(data[which(data$value == t.espr[i]), ])[1]/dim(data)[1]
    terms <- unique(as.character(data$term[which(data$value== t.espr[i])]))
    terms.freq <- numeric(length(terms))
    if(terms.frequency)
    {
      terms.freq <- numeric(length(terms))
      terms.count <- numeric(length(terms))
      # Total term frequency (da migliorare.. tipo matrice)
      for(j in 1:length(terms))
      { 
        terms.freq[j] <- dim(data[which(data$term == terms[j]), ])[1]/dim(data)[1]
        terms.count[j] <- dim(data[which(data$term == terms[j]), ])[1]
      }
      out[[t.espr[i]]] <- list(terms = cbind(cbind(terms, terms.freq), terms.count), freq = freq)
    }
    else out[[t.espr[i]]] <- list(terms = terms, freq = freq)
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


startTimer <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

stopTimer <- function()
{
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  #print(toc - tic)
  
  diff <- toc - tic
  
  secs <- floor(diff)
  
  min <- floor(secs / 60)
  secs <- secs - (min * 60)
  
  
  
  
  
  milli <- floor((diff - secs) * 1000)
  
  tempo <- paste(milli, "milli")
  
  if (secs > 0)
  {
    tempo <- paste(secs, "s", tempo)
  }
  
  if (min > 0)
  {
    tempo <- paste(min, "m", tempo)
  }
  
  print(paste("Tempo trascorso: ", tempo))
  
  
  
  
  invisible(toc)
}
