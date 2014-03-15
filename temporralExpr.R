# setwd("~/Scrivania/TipsterData/esiti 1992 bis")

# Lettura dati
data <- read.delim("heidel_details.txt", header=F)
colnames(data) <- c("id", "type", "value", "term", "creation")

print(unique(data$value[data$type == "DATE"]))
print(unique(data$value[data$type == "DURATION"]))
print(unique(data$value[data$type == "TIME"]))
print(unique(data$value[data$type == "SET"]))

print(unique(data$value[data$type == "DATE"]))
data[data$value == 1169, ]
data[data$value == 1220, ]

check <- c("0", "00"," 01", "02", "03", 11)

for(i in check)
{
  print(data[data$value == 00, ])
}