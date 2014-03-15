# setwd("~/Scrivania/TipsterData/esiti 1992 bis")

# Lettura dati
data <- read.delim("heidel_details.txt", header=F)
colnames(data) <- c("id", "type", "value", "term", "creation")

print(unique(data$value[data$type == "DATE"]))

data[data$value == 1169, ]
data[(as.integer(data$value)>2000), ]
for(i in 1:50)
{
  print(data[data$value ==levels(data$value)[i], ])
}

# Nuova colonna per differenziare gli anni
data$year <- data$value
levels(data$year) <- as.integer(levels(data$value))
str(data$year)
head(data)
# Pulizia 
# quali sono century
# quali non hanno senso, tipo gli ultimi livelli (2140, 2901.. )
##
print(unique(data$value[data$type == "DURATION"]))
print(unique(data$value[data$type == "TIME"]))
print(unique(data$value[data$type == "SET"]))
