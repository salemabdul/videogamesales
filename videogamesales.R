library(tidyverse)
library(dplyr)
x= read.csv("C:/Users/SALEM/Downloads/data.csv")
attach(x)
genre = x[,1] 

cor(x[,"EU_Sales"],x[,"Other_Sales"])
cor(x[,"EU_Sales"], x[,"NA_Sales"])


plot(x[,7:11])
cor(x[,7:11])
                           
hist(table(x["EU_Sales"]))

#import data 
# descriptive stats
# corelation
#prob modelling




#this is about doing the questions

# lets do first question
colnames = colnames(x)


consoles = x[, "Platform"]
#hist(consoles)
#categorizing the data in form of table, for us to then make a list of how the game has developed 
consoleByName = table(consoles)
consoleNum = as.vector(consoleByName)
consoleName = names(consoleByName)


#consoles by year 


# lets do it for the year by 
years = x[, "Year"]
gamesMadeByYear = table(years)
plot(gamesMadeByYear)


# create empty object 
# my idea is to see how the consoles number changed
# number of sales of games happened in console per year... 
# this can show how consoles and games are related..
# should be directly proportional 
# so, what do we need? 
# console by year 
# get each year, 
# and in each year, categorize the console 

# first get each year... 
allYears = names(gamesMadeByYear)
allYears


generateAllConsole <- function(allYears){
  consoleList = matrix(nrow = length(allYears), ncol = length(consoleName))
  for(i in 1:length(allYears)){
    #for(i in 1:3){
    gameListforThatYear = x[x$Year == allYears[i], ]
    # hey but we just want consoles.. 
    gameConsolesForThatYear = table(gameListforThatYear[,"Platform"])
    gameListforThatYearInTable = table(gameConsolesForThatYear)
    #print(gameConsolesForThatYear)
    
    # add them all
    print(allYears[i]); 
    print(gameConsolesForThatYear)
    consoleList[i] = as.vector(gameConsolesForThatYear)
  }
  consoleList
}

#consoleList = generateAllConsole(allYears = allYears)
#consoleList 


#show psp in graph
barplot(table(Year[Platform == "PSP"]), col = rainbow(20))
barplot(table(Year[Platform == "XB"]), col = rainbow(20))
barplot(table(Year[Platform == "PS4"]), col = rainbow(20))
class(Global_Sales)
range(Global_Sales)
any(is.na(Global_Sales))
hist(Global_Sales[Platform == "PSP"], freq=F, xlab = "Global Sales of PSP in dollars", col = "blue", main = " ")
title("Global Sales of PSP games throughout the years")
ybar = mean(Global_Sales, na.rm  = T)
curve(dexp(x,rate=1/ybar),add=T,col="blue")
s2 = var(Global_Sales, na.rm=T)
b=s2/ybar
a=ybar/b
curve(dgamma(x,shape=a,scale=b), n = 500, add = T, col = "red")
legend(x=6,y=1.6,c("Exponential", "Gamma"), lty = 1, col = c("blue","red"),cex=.8)