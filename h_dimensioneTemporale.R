# Prova con i dati del 1992
#dim.temp<- read.delim("~/Scrivania/heidel_dimensioneTemporale (id+date).txt", header=F)

tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self"))
{
  type <- match.arg(type)
  assign(".type", type, envir=baseenv())
  if(gcFirst) gc(FALSE)
  tic <- proc.time()[type]         
  assign(".tic", tic, envir=baseenv())
  invisible(tic)
}

toc <- function()
{
  type <- get(".type", envir=baseenv())
  toc <- proc.time()[type]
  tic <- get(".tic", envir=baseenv())
  print(toc - tic)
  invisible(toc)
}




tic()
dim.temp<- read.delim("~/Documents/GIT/Rstuff/heidel_dimensioneTemporale.txt", header=F)
toc()
colnames(dim.temp) <- c("id", "day")
# Gestion date in R-base
tic()
dim.temp$day <- as.Date(dim.temp$day)
toc()
tic()
str(dim.temp)
summary(dim.temp$day)
toc()
hist(dim.temp$day, breaks = "year")

# Numero di documenti
tic()
length(unique(dim.temp$id))
toc()

# Prova con primo documento
tic()
prova <- dim.temp[which(dim.temp$id ==levels(dim.temp$id)[1]), ]
prova <- droplevels(prova)
toc()
summary(prova$day)
hist(prova$day, breaks = "months")

# Libreria "lubridate" per la gestione delle date
# install.packages("lubridate")
library("lubridate")
vignette("lubridate")
prova$day <- ymd(prova$day)
round_date(summary(prova$day), "day")

library(ggplot2)

qplot(prova$day, 0, data = prova)

#------------------------------------#
prova2 <-  dim.temp[which(dim.temp$id ==levels(dim.temp$id)[1:10]), ]
prova2 <- droplevels(prova2)
summary(prova2$day)
qplot(prova2$day, 0, data = prova2, col = prova2$id)