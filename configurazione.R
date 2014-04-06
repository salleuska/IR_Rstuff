set.config <- function(user = "") 
{
  config <- numeric(3) # in caso di aggiunte di percorsi cambiare la lunghezza 
  #names(config) <- c("path per il source delle funzioni", 
  #                   "path dati", 
  #                   "path scrittura data_to_split")
  names(config) <- c("sourcepath", 
                     "datapath", 
                     "txtpath")
  if (user == "alan") 
  {
    config[1] = "/home/alan/Documents/GIT/Rstuff/"
    config[2] = "/home/alan/Documents/HeidelData/"
    config[3] = "/home/alan/Documents/Parser_R_txt/"
  }
  else if(user == "sally")
  {
    config[1] = "/home/sally/altracartella/IR_Rstuff/"
    config[2] = "/home/sally/Documents/HeidelData/"
    config[3] = "/home/sally/Documents/Parser_R_txt/"
  }
  else cat("Definisci user")
  
  config
}
