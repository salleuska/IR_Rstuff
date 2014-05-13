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

tag <- function(matrice.class, id, tag, score)
{
  tmp <- data.frame(matrix(NA, nrow = length(id), ncol = 3))
  tmp[,1] <- id
  tmp[,2] <- tag
  tmp[,3] <- score
  
  colnames(tmp) <- c("id", "class", "score")
  rbind(matrice.class, tmp)
}

#------------------------------------------------------#
stato <- function(matrice.class)
{
  cat("Numero (indicativo) documenti classificati \t ", length(unique(matrice.class$id)), "\n")
  cat("#--------------------------------------------------------------------#\n")
  cat("Tag\n")
  sort(summary(matrice.class$class, maxsum=15), decreasing=T)
}
#-------------------------------------------------------#

matrix.ties <- function(lista, colnames)
{ 
  data <- data.frame(matrix(NA, nrow = length(lista), ncol = 3))
  colnames(data) <- colnames
  l <- 0
  for(i in 1:length(lista))
  { 
    list.element <- lista[i]
    score <- as.numeric(list.element[[1]][[2]])
    
    for(j in 1:length(list.element[[1]][[1]]))
    {
      data[(j + l), ] <- c(names(list.element),list.element[[1]][[1]][j], score) 
    }
    l <- l + length(list.element[[1]][[1]])
  }
  data
}
#---------------------------------------#
update.intervalli <- function(lista, matrice.class)
{  
  N = length(lista)*2
  tmp <- data.frame(matrix(NA, nrow = N, ncol = 3))
  l <- 0
  for(i in 1:length(lista))
  { 
    list.element <- lista[i]
    score <- as.numeric(class.noREF[which(class.noREF$id %in% names(list.element)), ]$Freq)
    
    for(j in 1:length(list.element[[1]]$tag))
    {
      tmp[(j + l), ] <- c(names(list.element),list.element[[1]]$tag[j], score) 
    }
    l <- l + length(list.element[[1]]$tag)
  }
  colnames(tmp) <- c("id", "class", "score")
  tmp <- tmp[-which(is.na(tmp$id)), ]
  rbind(matrice.class, tmp)
}
