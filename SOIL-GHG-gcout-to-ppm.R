## Script for extracting GHG concentrations from SOIL csv files
# And correcting them and writing a new csv with data
## Gavin McNicol
# Created 11.14.2017
# SOIL Haiti GHG Flux project

## UPDATED 17.12.09

# Set working directory
setwd('/Users/gavinmcnicol/Dropbox/SOIL GHG Ryals/Data/3 May 2017/Round 2 and 3 Reruns with Full N2O High CO2 CH4/')

## Read in csv file
data <- read.csv(file.choose())

# save week info
name <- data$X[2]
name

# number of columns
length(data)

#extract only GHG conc column
ghg.conc <- as.numeric(as.character(data$X.4))
ghg.conc <- ghg.conc[-which(is.na(ghg.conc))]

ghg.names <- data$X[!is.na(as.numeric(as.character(data$X.4)))]

# check these index values each time! 
all.ghg <- as.data.frame(cbind(c(ghg.names),ghg.conc))
head(all.ghg$V1,12)
all.ghg$V1[which(all.ghg$V1 == 159)] <- "CH4"
all.ghg$V1[which(all.ghg$V1 == 158)] <- "CO2"
all.ghg$V1[which(all.ghg$V1 == 161)] <- "N2O"

# subset for each gas
ghg.co2 <- all.ghg$ghg.conc[which(all.ghg$V1 == 'CO2')]
ghg.ch4 <- all.ghg$ghg.conc[which(all.ghg$V1 == 'CH4')]
ghg.n2o <- all.ghg$ghg.conc[which(all.ghg$V1 == 'N2O')]

# save plots
par(mar=c(5,5,2,2))
par(mfrow=c(3,1),bg='white')
plot(ghg.ch4)
plot(ghg.co2)
plot(ghg.n2o)
length(ghg)

diff_l <- length(ghg.ch4)-length(ghg.n2o)

### now just outputting the ghg data - was having problems with identifying standards
# combine data into frame
data2 <- as.data.frame(cbind(ghg.co2, ghg.ch4, c(rep(NA,diff_l),ghg.n2o)))
#data2 <- as.data.frame(cbind(ghg.co2.cor, ghg.ch4.cor,c(ghg.n2o.cor,rep(NA,abs(diff_l)))))
colnames(data2) <- c('CO2','CH4','N2O')

#write csv and save in new folder
write.csv(data2,'/Users/gavinmcnicol/Dropbox/SOIL GHG Ryals/Data/3 May 2017/Round 2 and 3 Reruns with Full N2O High CO2 CH4/Round 2 GHG Reprocessed/170707_Feb2017.csv')


# read in data now that order is standardized so std.vector will be correct
## Read in csv file
data <- read.csv(file.choose())

# index the location of standards
std.vector <- c(1,11,21,31,41,51,61,71,81,91,101,111,121,131)
# alt vector
#alt.std.vector <- std.vector[2:length(std.vector)]-1

# create std vector 
co2.std <- c(rep(c(5000,50000),7))
ch4.std <- c(rep(c(10,20000),7))
n2o.std <- c(rep(c(5,NA),7))

# calculate mean kfactor for each gas
k.co2 <- co2.std/as.numeric(as.character(data$CO2[std.vector]))
mean.k.co2 <- mean(k.co2,na.rm=TRUE)
mean.k.co2

k.ch4 <- ch4.std/as.numeric(as.character(data$CH4[std.vector]))
k.ch4.low <- k.ch4[which(k.ch4 < 0.8)]
k.ch4.high <- k.ch4[which(k.ch4 > 0.8)]
mean.k.ch4.low <- mean(k.ch4.low,na.rm=TRUE); mean.k.ch4.low
mean.k.ch4.high <- mean(k.ch4.high,na.rm=TRUE); mean.k.ch4.high

k.n2o <- n2o.std/as.numeric(as.character(data$N2O[std.vector]))
#k.n2o <- k.n2o[which(k.n2o > 0 & !is.na(k.n2o))]
mean.k.n2o <- mean(k.n2o, na.rm=TRUE); mean.k.n2o

# correct GHG concentrations with k factor
ghg.co2.cor <- as.numeric(as.character(data$CO2[-std.vector]))*mean.k.co2

ghg.ch4.cor <- as.numeric(as.character(data$CH4[-std.vector]))
ghg.ch4.cor[which(ghg.ch4.cor < 100)] <- ghg.ch4.cor[which(ghg.ch4.cor < 100)]*mean.k.ch4.low
ghg.ch4.cor[which(ghg.ch4.cor > 100)] <- ghg.ch4.cor[which(ghg.ch4.cor > 100)]*mean.k.ch4.high

ghg.n2o.cor <- as.numeric(as.character(data$N2O[-std.vector]))*mean.k.n2o


# combine data into frame
data2 <- as.data.frame(cbind(ghg.co2.cor, ghg.ch4.cor, ghg.n2o.cor))
#data2 <- as.data.frame(cbind(ghg.co2.cor, ghg.ch4.cor,c(ghg.n2o.cor,rep(NA,abs(diff_l)))))
colnames(data2) <- c('CO2','CH4','N2O')

#write csv and save in new folder
write.csv(data2,'/Users/gavinmcnicol/Dropbox/SOIL GHG Ryals/Data/3 May 2017/Round 2 and 3 Reruns with Full N2O High CO2 CH4/Round 2 GHG Reprocessed/170707_Feb2017_ppm_cor.csv')

