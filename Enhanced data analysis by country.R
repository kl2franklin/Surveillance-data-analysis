# packages
library(dplyr)
library(reshape2)
library(png)
library(xlsx)

getwd()

# Analysis for Benin

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Benin.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("Benin",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
Beninsensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(Beninsensitivity10)
filename <- paste("Beninsensitivity10",".csv", sep="")
write.csv(Beninsensitivity10, filename)

#Analysis for Peak incidence of 5/100,000:
Beninsensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(Beninsensitivity5)
filename <- paste("Beninsensitivity5",".csv", sep="")
write.csv(Beninsensitivity5, filename)


#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
Beninsensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(Beninsensitivity805)
filename <- paste("Beninsensitivity805",".csv", sep="")
write.csv(Beninsensitivity805, filename)




#Analysis for Burkina Faso

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Burkina Faso.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("Burkina Faso",".csv", sep="")
write.csv(dta3, filename)

#Analysis for Peak incidence of 10/100,000:
BurkinaFasosensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(BurkinaFasosensitivity10)
filename <- paste("BurkinaFasosensitivity10",".csv", sep="")
write.csv(BurkinaFasosensitivity10, filename)


#Analysis for Peak incidence of 5/100,000:
BurkinaFasosensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(BurkinaFasosensitivity5)
filename <- paste("BurkinaFasosensitivity5",".csv", sep="")
write.csv(BurkinaFasosensitivity10, filename)


#Analysis for peak incidence 5/100,000 and cumulative 80/100,000:
BFsensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(BFsensitivity805)
filename <- paste("BFsensitivity805",".csv", sep="")
write.csv(BFsensitivity805, filename)





#Analysis for Cameroon

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Cameroon.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("Cameroon_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
Cameroonsensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(Cameroonsensitivity10)
filename <- paste("Cameroonsensitivity10",".csv", sep="")
write.csv(Cameroonsensitivity10, filename)

#Analysis for Peak incidence of 5/100,000:
Cameroonsensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(Cameroonsensitivity5)
filename <- paste("Cameroonsensitivity5",".csv", sep="")
write.csv(Cameroonsensitivity5, filename)

#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
Cameroonsensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(Cameroonsensitivity805)
filename <- paste("Cameroonsensitivity805",".csv", sep="")
write.csv(Cameroonsensitivity805, filename)



#Analysis for CIV

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/CIV.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("CIV_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
CIVsensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(CIVsensitivity10)
filename <- paste("CIVsensitivity10",".csv", sep="")
write.csv(CIVsensitivity10, filename)

#Analysis for Peak incidence of 10/100,000:
CIVsensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(CIVsensitivity5)
filename <- paste("CIVsensitivity5",".csv", sep="")
write.csv(CIVsensitivity5, filename)

#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
CIVsensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(CIVsensitivity805)
filename <- paste("CIVsensitivity805",".csv", sep="")
write.csv(CIVsensitivity805, filename)




#Analysis for CAR

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/CAR.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("CAR_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
CARsensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(CARsensitivity10)
filename <- paste("CARsensitivity10",".csv", sep="")
write.csv(CARsensitivity10, filename)

#Analysis for Peak incidence of 5/100,000:
CARsensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(CARsensitivity5)
filename <- paste("CARsensitivity5",".csv", sep="")
write.csv(CARsensitivity5, filename)

#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
CARsensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(CARsensitivity805)
filename <- paste("CARsensitivity805",".csv", sep="")
write.csv(CARsensitivity805, filename)






#Analysis for Ethiopia

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Ethiopia1.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("Ethiopia_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
Ethiopiasensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(Ethiopiasensitivity10)
filename <- paste("Ethiopiasensitivity10",".csv", sep="")
write.csv(Ethiopiasensitivity10, filename)


#Analysis for Peak incidence of 5/100,000:
Ethiopiasensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(Ethiopiasensitivity5)
filename <- paste("Ethiopiasensitivity5",".csv", sep="")
write.csv(Ethiopiasensitivity5, filename)


#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
Ethiopiasensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(Ethiopiasensitivity805)
filename <- paste("Ethiopiasensitivity805",".csv", sep="")
write.csv(Ethiopiasensitivity805, filename)






#Ghana
filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Ghana.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("Ghana_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
Ghanasensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(Ghanasensitivity10)
filename <- paste("Ghanasitivity10",".csv", sep="")
write.csv(Ghanasensitivity10, filename)


#Analysis for Peak incidence of 5/100,000:
Ghanasensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(Ghanasensitivity5)
filename <- paste("Ghanasitivity5",".csv", sep="")
write.csv(Ghanasensitivity5, filename)


#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
Ghanasensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(Ghanasensitivity805)
filename <- paste("Ghanasensitivity805",".csv", sep="")
write.csv(Ghanasensitivity805, filename)




#Gambia_Guinea

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Gambia_Guinea.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("Gambia_Guinea_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
Ghambia_Guineasensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(Ghambia_Guineasensitivity10)
filename <- paste("Ghambia_Guineasensitivity10",".csv", sep="")
write.csv(Ghambia_Guineasensitivity10, filename)


#Analysis for Peak incidence of 5/100,000:
Ghambia_Guineasensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(Ghambia_Guineasensitivity5)
filename <- paste("Ghambia_Guineasensitivity5",".csv", sep="")
write.csv(Ghambia_Guineasensitivity5, filename)


#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
Ghambia_Guineasensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(Ghambia_Guineasensitivity805)
filename <- paste("Ghambia_Guineasensitivity805",".csv", sep="")
write.csv(Ghambia_Guineasensitivity805, filename)






# Analysis for Kenya

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Kenya.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("Kenya_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
Kenyasensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(Kenyasensitivity10)
filename <- paste("Kenyasensitivity10",".csv", sep="")
write.csv(Kenyasensitivity10, filename)


#Analysis for Peak incidence of 10/100,000:
Kenyasensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(Kenyasensitivity5)
filename <- paste("Kenyasensitivity5",".csv", sep="")
write.csv(Kenyasensitivity5, filename)


#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
Kenyasensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(Kenyasensitivity805)
filename <- paste("Kenyasensitivity805",".csv", sep="")
write.csv(Kenyasensitivity805, filename)




#Analysis for Niger

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Enhanced DB Niger.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("Niger_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
Nigersensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(Nigersensitivity10)
filename <- paste("Nigersensitivity10",".csv", sep="")
write.csv(Nigersensitivity10, filename)


#Analysis for Peak incidence of 5/100,000:
Nigersensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(Nigersensitivity5)
filename <- paste("Nigersensitivity5",".csv", sep="")
write.csv(Nigersensitivity5, filename)


#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
Nigersensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(Nigersensitivity805)
filename <- paste("Nigersensitivity805",".csv", sep="")
write.csv(Nigersensitivity805, filename)





# Analysis for Nigeria
filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Enhanced DB Nigeria.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

View(dta)

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3 <- dta3[!is.na(dta3$POP),]

dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
view(dta3)

filename <- paste("Nigeria_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)

#Analysis for Peak incidence of 10/100,000:
nigeriasensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(nigeriasensitivity10)
filename <- paste("nigeriasensitivity10",".csv", sep="")
write.csv(nigeriasensitivity10, filename)


#Analysis for Peak incidence of 5/100,000:
nigeriasensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(nigeriasensitivity5)
filename <- paste("nigeriasensitivity5",".csv", sep="")
write.csv(nigeriasensitivity5, filename)


#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
Nigeriasensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(Nigeriasensitivity805)
filename <- paste("Nigeriasensitivity805",".csv", sep="")
write.csv(Nigeriasensitivity805, filename)



#Analysis countries R-S

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Enhanced DB RS.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("RS_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
rssensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(rssensitivity10)
filename <- paste("rssensitivity10",".csv", sep="")
write.csv(rssensitivity10, filename)


#Analysis for Peak incidence of 5/100,000:
rssensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(rssensitivity5)
filename <- paste("rssensitivity5",".csv", sep="")
write.csv(rssensitivity5, filename)


#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
RSsensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(RSsensitivity805)
filename <- paste("RSsensitivity805",".csv", sep="")
write.csv(RSsensitivity805, filename)



# Analysis for countries T-U

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Enhanced DB TU.csv"
dta <- read.csv(filename, colClasses = "character")
dta$CASES2 <- as.numeric(dta$CASES)
dta$CASES2[is.na(dta$CASES2)] <- 0

dta$Attack.rate..10 <- as.logical(dta$Attack.rate..10)
dta$Attack.rate..5 <- as.logical(dta$Attack.rate..5)
dta$Column1 <- as.numeric(dta$Column1)

dta1 <- dta %>% group_by(COUNTRY, DISTRICT, Year) %>% summarise(cases = sum(CASES2), max(Column1), sum(Attack.rate..10), sum(Attack.rate..5))

dta2 <- unique(dta[,c("COUNTRY","DISTRICT","Year","POP")])
dta2$POP <- as.numeric(dta2$POP)
dta2 <- dta2[!is.na(dta2$POP),]

dta3 <- merge(dta1, dta2, all = T)
dta3[is.na(dta3$POP),]
dta3$incidence <- dta3$cases / dta3$POP * 100000

#dataset with cumulative incidence in all regions reporting cases
View(dta3)

filename <- paste("TU_cumulative_incidence",".csv", sep="")
write.csv(dta3, filename)


#Analysis for Peak incidence of 10/100,000:
tusensitivity10 <- dta3[dta3$`max(Column1)` > 10 & !is.na(dta3$`max(Column1)`),]
View(tusensitivity10)
filename <- paste("tusensitivity10",".csv", sep="")
write.csv(tusensitivity10, filename)

#Analysis for Peak incidence of 5/100,000:
tusensitivity5 <- dta3[dta3$`max(Column1)` > 5 & !is.na(dta3$`max(Column1)`),]
View(tusensitivity5)
filename <- paste("tusensitivity5",".csv", sep="")
write.csv(tusensitivity5, filename)

#Analysis for peak incidence 5/100,000 and cumulative 80/100,000
TUsensitivity805 <- dta3[dta3$incidence > 80 & dta3$`max(Column1)` > 5 & !is.na(dta3$incidence) & !is.na(dta3$`max(Column1)`),]
View(TUsensitivity805)
filename <- paste("TUsensitivity805",".csv", sep="")
write.csv(TUsensitivity805, filename)











#Merging databases
# Merge w135 and analysis with 10/100,000 cut-off
filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Combinedsensitivity_2.csv"
outbreaks <- read.csv(filename, colClasses = "character")

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/W135_Imp_database by district Nov 2013 (analysis finished Jan 2013)_KF (Autosaved).csv"
w135 <- read.csv(filename, colClasses = "character")

merged_outbreaks <- merge(outbreaks, w135, all.x = T)
merged_outbreaks[is.na(merged_outbreaks)] <- ""

filename <- paste("merged_outbreaks",".csv", sep="")
write.csv(merged_outbreaks, filename)



#Merge w135 and analysis with Peak 5/10,000 cut-off and cumulative 80
filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Combinedsensitivity_805.csv"
outbreaks805 <- read.csv(filename, colClasses = "character")

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/W135_Imp_database by district Nov 2013 (analysis finished Jan 2013)_KF (Autosaved).csv"
w135 <- read.csv(filename, colClasses = "character")

#Merge datasets
merged_outbreaks805 <- merge(outbreaks805, w135, all.x = T)
merged_outbreaks805[is.na(merged_outbreaks805)] <- ""

filename <- paste("merged_outbreaks805",".csv", sep="")
write.csv(merged_outbreaks805, filename)



#Merge w135 and analysis with 5/100,000 cut-off
filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/Combinedsensitivity_5.csv"
outbreaks5 <- read.csv(filename, colClasses = "character")

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/W135_Imp_database by district Nov 2013 (analysis finished Jan 2013)_KF (Autosaved).csv"
w135 <- read.csv(filename, colClasses = "character")

#Merge datasets
merged_outbreaks5 <- merge(outbreaks5, w135, all.x = T)
merged_outbreaks5[is.na(merged_outbreaks5)] <- ""

filename <- paste("merged_outbreaks5",".csv", sep="")
write.csv(merged_outbreaks5, filename)



#Merging above with the ICG data
filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/merged_outbreaks805.csv"
outbreaks805 <- read.csv(filename, colClasses = "character")

filename <- "C:/Users/franklink.WIMS/Desktop/franklink/Data sets/Enhanced data/peak10_merged_w135_enhanced.csv"
peak10_merged_w135_enhanced <- read.csv(filename, colClasses = "character")

#Merge datasets
merged_outbreaks805_ICG <- merge(outbreaks805, peak10_merged_w135_enhanced, all.x = T)
merged_outbreaks805_ICG[is.na(merged_outbreaks805_ICG)] <- ""

filename <- paste("merged_outbreaks805_ICG",".csv", sep="")
write.csv(merged_outbreaks805_ICG, filename)

