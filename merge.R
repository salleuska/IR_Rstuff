#heidel_details <- read.delim("~/Scrivania/TipsterData/esiti tutti/heidel_details.txt", header=F)
#heidel_precision <- read.delim("~/Scrivania/TipsterData/esiti tutti/heidel_datePrecision.txt", header=F)
heidel_details <- read.delim("heidel_details.txt", header=F)
heidel_precision <- read.delim("heidel_datePrecision.txt", header=F)

colnames(heidel_details) <- c("id", "type", "value", "term", "creation")
colnames(heidel_precision) <- c("id", "value", "gran")

#---- check dimensioni ----#
length(levels(heidel_precision$value))
d <- heidel_details[which(heidel_details$type == "DATE"), ]
length(unique(d$value))
#--------------------------#
notd <- heidel_details[which(heidel_details$type != "DATE"), ]
notd$gran <- NA

tmp <- cbind(d, heidel_precision$gran)
colnames(tmp) <- c("id", "type", "value", "term", "creation", "gran")
str(tmp)

str(notd)

heidel <- rbind(tmp, notd)
str(heidel)
length(which(is.na(heidel$gran))) #ok
heidel$gran[which(is.na(heidel$gran))] <- "undefined"
#check
heidel[which((heidel$gran =="undefined")&(heidel$type != "DATE"))[1:100], ]

# Riordinamento
heidel <- heidel[order(heidel$id), ]

write.table(heidel, file = "heidel_details&precision.txt")
