source("/home/sally/altracartella/IR_Rstuff/FunzioniAnalisi.R")

# prova scrittura in parallelo
setwd("~/Scrivania/heidel_data&precision - pulizia")
data.originali <- read.table("heidel_pulizia.def.txt")

check.valori.undefined(data.originali)#----------------------------------------------------#

set.seed(1234) #fissa il numero di partenza per il sample (in modo da estrarre "casualmente" 
# gli stessi dati)

data <- data.originali[which(data.originali$id %in% sample(levels(data.originali$id), 10000)),]
data <- droplevels(data)

#-------------------------------------------------#
setwd("/home/sally/Scrivania/TipsterData/prova")
install.packages("foreach") 
install.packages("doParallel") 
# Per info
# help(doParallel) 
# vignette("gettingstartedParallel"

write.files <- function(data)
{
  file.name = paste(data$id[1],"_",data$creation[1], ".txt", sep = "") 
  subset.to.write <- data[, c("type", "value", "gran")]
  write.table(subset.to.write, file = file.name,
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}

library(doParallel)

# Register parallel backend
# default
# registerDoParallel()

# si può scegliere il numero di copie di R che girano in parallelo con makeCluster
cl <- makeCluster(4)
registerDoParallel(cl)


library(plyr)

setwd("/home/sally/Scrivania/HeidelCollection/R_puliti_divisi_subset")
startTimer()
d_ply(data, "id", function(x) write.files(x), .parallel = TRUE, .inform = TRUE )

stopTimer() #[1] "Tempo trascorso:  2 m 31 s 120171 milli"

# chiusura socket aperti
stopCluster(cl)
#--------------------------------------------------#
# Prova su tutti i file da fare
# su più subset?
setwd("/home/sally/Scrivania/HeidelCollection/R_puliti_divisi")

length(levels(data.originali$id))

data.sub1 <- data.originali[which(data.originali$id %in% levels(data.originali$id)[1:round(113881/6)]),]
data.sub1 <- droplevels(data.sub1)

data.sub2 <- data.originali[which(data.originali$id %in% levels(data.originali$id)[((round(113881/6)+1):(2*round(113881/6)))]),]
data.sub2 <- droplevels(data.sub2)

data.sub3 <- data.originali[which(data.originali$id %in% levels(data.originali$id)[((2*round(113881/6)+1):(3*round(113881/6)))]),]
data.sub3 <- droplevels(data.sub3)

data.sub4 <- data.originali[which(data.originali$id %in% levels(data.originali$id)[((3*round(113881/6)+1):(4*round(113881/6)))]),]
data.sub4 <- droplevels(data.sub4)

data.sub5 <- data.originali[which(data.originali$id %in% levels(data.originali$id)[((4*round(113881/6)+1):(5*round(113881/6)))]),]
data.sub5 <- droplevels(data.sub5)

data.sub6 <- data.originali[which(data.originali$id %in% levels(data.originali$id)[(5*round(113881/6) +1):113881]), ]
data.sub6 <- droplevels(data.sub6)


dim(data.sub1)[1] + dim(data.sub2)[1] + dim(data.sub3)[1] + dim(data.sub4)[1] + dim(data.sub5)[1] + dim(data.sub6)[1]


cl <- makeCluster(8)
registerDoParallel(cl)
startTimer()
d_ply(data.sub1, "id", function(x) write.files(x), .parallel = TRUE)

stopTimer()
stopCluster(cl)
