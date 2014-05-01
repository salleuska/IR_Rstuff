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
  lista.data <- as.data.frame(cbind(names(lista), as.vector(sapply(lista, "[[", 1))), row.names = NULL)
  colnames(lista.data)<-colnames
  lista.data$Freq <- as.vector(sapply(lista, "[[", 2))
  
  droplevels(lista.data)
}
