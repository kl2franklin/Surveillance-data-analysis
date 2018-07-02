# packages
library(dplyr)
library(reshape2)
library(png)

# Reading data
filename <- filename <- "c:/users/franklink.WIMS/Desktop/franklink/Global_IBVPD_CB_Data.csv"
dta <- read.csv(filename, colClasses = "character")
setwd("C:/Users/franklink/Desktop/franklink/R")

# Backup loaded data
save <- dta

getwd()

View(dta)


# Change types of variables
#Filter and assign numeric:
dta$year <- as.numeric(dta$yy)

dta1 <- filter(dta, dta$year > "2016")
View(dta1)

dta4 <- dta %>% group_by(Region, ISO3_code, site_code, yearmth) %>% summarise(cases = n())
View(dta4) 

# Summarize data (using dplyr)
dta2 <- dta %>% group_by(Region, ISO3_code, site_code, gender) %>% summarise(cases = n())
View(dta2)  

# Reshape data (using reshape2)
dta3 <- dcast(dta2, Region+ISO3_code+site_code~gender, value.var = "cases")
View(dta3)

#Base code:
dates <- seq(as.Date(min(dta$adm_date)),as.Date(max(dta$adm_date)),1)
dates <- substr(dates,1,7)
dates <- unique(dates)

dta6 <- dcast(dta_ken_strep, Region+ISO3_code+site_code~yearmth, value.var = "cases")
for(mydate in dates){
  if(!mydate %in% colnames(dta6)){
    dta6[,mydate] <- NA
  }
}
dta6 <- melt(dta6, id.vars = c("Region","ISO3_code","site_code"), variable.name = "yearmth")
view(dta6)
#Strep cases per region
dta4 <- dta %>% filter(Region %in% "AFR") %>% group_by(Region, ISO3_code, site_code, yearmth, path) %>% summarise(strep_cases = n())
dta4 <- dta4[dta4$path %in% "1",]
View(dta4)

# Susected cases per region:
dta5 <- dta %>% filter(Region %in% "AFR") %>% group_by(Region, ISO3_code, site_code, yearmth) %>% summarise(suspect_cases = n())
View(dta5)

# Confirmed cases per region...
dta6 <- dta %>% filter(Region %in% "AFR") %>% group_by(Region, ISO3_code, site_code, yearmth, path) %>% summarise(conf_cases = n())
dta6 <- dta6[dta6$path %in% "1", "2", "3",]
View(dta6)
#Merge tables
temp <- merge(dta4, dta5)

temp <- merge(dta4, dta5, all.y = T)

filename <- paste("IBVPD",".csv", sep="")
write.csv(dta7, filename)

# Alternative analysis

countries <- c("GMB", "SEN", "MLI", "BFA", "NER", "NGA", "CMR", "CAF", "SDN", "UGA", "KEN", "ETH", "ERI", "GHA", "CIV", "BEN", "TGO", "COD")
for(country in countries){
  print(country)
  
  dta5 <- dta4[dta4$ISO3_code %in% country,]
  
  filename <- paste(country,".png", sep="")
  png(filename)
  plot(dta5$cases)  
  dev.off()  
  
  filename <- paste(country,".csv", sep="")
  write.csv(dta5, filename)
  
}





