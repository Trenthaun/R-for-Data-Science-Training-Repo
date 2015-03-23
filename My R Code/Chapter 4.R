load("~/Documents/GitHub/R-for-Data-Science-Training-Repo/Custdata/exampleData.rData")
library(ggplot2)
library(scales)
#check for missing values:

summary(custdata[is.na(custdata$housing.type),c("recent.move","num.vehicles")])

#convert is.employed NA's to "missing" to retain them during calcs

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),"not in active workforce", ifelse(
  custdata$is.employed==T, "employed", "not employed"))

summary(as.factor(custdata$is.employed.fix))
ggplot(custdata) + geom_bar(aes(x=is.employed.fix), fill="gray")

#What about working with categorical variables?

summary(custdata$income)

meanIncome <- mean(custdata$income, na.rm=T)

#The na.rm=T tells the mean function to ignore NAs

meanIncome

Income.fix <- ifelse(is.na(custdata$Income), meanIncome, abs(custdata$Income))

summary(Income.fix)

#This is very basic and easy, to get more granular, you could leverage the relationship between other
#variables and income to replace with a more accurate guess

#Dealing with categorical values

breaks <-c(0,10000,50000,100000,250000,1000000)

Income.groups <-
  cut(custdata$Income, breaks=breaks, include.lowest=T)

summary(Income.groups)

Income.groups <- as.character(Income.groups)
Income.groups <- ifelse(is.na(Income.groups), "no income", Income.groups)
summary(as.factor(Income.groups))
ggplot(custdata) + geom_bar(aes(x=Income.groups,), fill='gray')

#track original NAs

missingIncome <- is.na(custdata$Income)
Income.Fix <- ifelse(is.na(custdata$Income), 0, custdata$Income)
summary(Income.Fix)

#Data Transformations

#Example: normalizing income to cost of living by state

summary(medianincome)

#merge(x,y,xkey,ykey)

custdata <- merge(custdata, medianincome, by.x="state.of.res", by.y="State")

summary(custdata[,c("state.of.res", "income", "Median.Income")])

#normalize

custdata$income.norm <- with(custdata, Income.fix/Median.Income)
summary(custdata$income.norm)

#continuous to discrete

custdata$income.lt.20k <- custdata$income < 20000
summary(custdata$income.lt.20k)

#add more than one bucket:

brks <- c(0,25,65,Inf)
custdata$age.range <- cut(custdata$age, breaks=brks, include.lowest=T)
summary(custdata$age.range)

#Normalizing and Scaling:

#let's relate customer's age to mean

summary(custdata$age)
meanage <- mean(custdata$age)
custdata$age.normalized <- custdata$age/meanage
summary(custdata$age.normalized)

#How much is very low or very high relative to mean? Look at standard deviation

stdage <- sd(custdata$age)
custdata$age.normalized <- ((custdata$age-meanage)/stdage)
summary(custdata$age.normalized)

#normalizing with signed log, which picks up the negative values that log() drops:

signedlog10 <- function(x) {
  ifelse(abs(x) <=1, 0, sign(x)*log10(abs(x)))
  
#Create a sample set

custdata$gp <- runif(dim(custdata)[1])
testSet <- subset(custdata, custdata$gp <=0.1)
trainingset <- subset(custdata, custdata$gp > 0.1)
dim(testSet)
dim(trainingset)

#create sets off of groupings:

hh <- unique(hhdata$household_id)
households <- data.frame(household_id = hh, gp = runif(length(hh)))
hhdata <- merge(hhdata, households, by="household_id")
View(hhdata)
