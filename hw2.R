#1a
#Set work directory and load the datasets
setwd('//Users/lulu/Documents/06 Simon/11 Marketing Analytics Using R/')
consumerData = read.csv('consumerDataFrame.csv')
itemData = read.csv('itemDataFrame.csv')

#1b
#Remove irrelavant data
consumerData = consumerData[-1]
itemData = itemData[-1]

#Rename the columns if necessary
install.packages('plyr')
library(plyr)
names(consumerData)
names(itemData)
consumerData = rename(consumerData, c("units" = "unitsPurchased",
                                      "dollars" = "price",
                                      "STOREIdentifier" = "storeID",
                                      "panelistNumber" = "consumerID",
                                      "FLAVORID" = "flavorID"))
itemData = rename(itemData, c("PRIVATE.LABEL" = "hasPrivateLabel",
                              "IDOFFLAVOR" = "flavorID"))

#1c
#The Merge Method
allData = merge(x=consumerData, y= itemData, by = 'flavorID')

#The For Loop Method
newdata = NULL
for (i in 1:nrow(consumerData)) {
  newdata_row = itemData[itemData$flavorID == consumerData$flavorID[i],]
  newdata = rbind(newdata, newdata_row)
}
allData2 = cbind(consumerData, newdata)

#1d
allData$pricePerUnit = allData$price / allData$unitsPurchased
pricePerUnit = allData$pricePerUnit

allData$totalVolume = allData$unitsPurchased * allData$volumePerUnit
totalVolume = allData$totalVolume

#1e
summary(allData)
#Mean and standard deviation of the data
  sapply(allData,function(x) list(mean=mean(x,na.rm=TRUE), sd=sd(x,na.rm=TRUE)))

#Market shares for each store
storeMarketShare = aggregate(unitsPurchased~storeID, data=allData, FUN=sum)
totalUnits = sum(storeMarketShare$unitsPurchased)
storeMarketShare$percentage = storeMarketShare$unitsPurchased / totalUnits *100
storeMarketShare$percentage

#Market shares for each of the top ten flavors
flavorShare = aggregate(unitsPurchased~flavorID, data=allData, FUN=sum)
flavorShare$percentage = flavorShare$unitsPurchased / totalUnits *100
flavorShare = flavorShare[order(-flavorShare$percentage),]
topTenFlavor = flavorShare[1:10,]
topTenFlavor

#Market shares for the private label
privateLabelShare = aggregate(unitsPurchased~hasPrivateLabel, data=allData, FUN=sum)
privateLabelShare$percentage = privateLabelShare$unitsPurchased / totalUnits *100
privateLabelShare[privateLabelShare$hasPrivateLabel=="TRUE","percentage"]

#2a
avgPurchasePrice = aggregate(pricePerUnit~consumerID, data=allData, FUN = mean)

#2b
#Create blank data to store coefficients
priceCoefByConsumer = NULL
consumerID = c(1:max(allData$consumerID))
for (i in 1:max(allData$consumerID)) {
  currentConsumerData = subset(allData,allData$consumerID==i)
  if (nrow(currentConsumerData)>0) {
    currentConsumerLM = lm(currentConsumerData$unitsPurchased~currentConsumerData$pricePerUnit)
    priceCoefByConsumer[i] = currentConsumerLM$coefficients[2]
  }
}

regression = data.frame(consumerID,priceCoefByConsumer)
regression = na.omit(regression)

#2c
twoMetrics = merge(regression, avgPurchasePrice,by="consumerID",na.rm=T)
correlation = cor(twoMetrics$pricePerUnit,twoMetrics$priceCoefByConsumer)

library(ggplot2)
s=ggplot(twoMetrics, aes(pricePerUnit, priceCoefByConsumer))
s+geom_point()+labs(x="Average Purchase Price by Consumer", 
                    y="Price Coefficient by Consumer",
                    title=paste("Correlation: ",round(correlation,4))
)
  
#2e
regression = regression[order(regression$priceCoefByConsumer),]
top100Consumer = regression[1:100,]

#3a
weeklySales = aggregate(price~weekNum, data = allData, FUN=sum)

#3b
modelStat = data.frame(ar = rep(NA,16),ma =rep(NA,16),AIC = rep(NA,16))
rowNum = 1
for(arDegree in 0:3) {
  for(maDegree in 0:3) {
    currentFit = arima(weeklySales[2],
                       c(arDegree, 0, maDegree))
    modelStat[rowNum,]=c(arDegree,maDegree,AIC(currentFit))
    rowNum = rowNum + 1
  }
}  
#Select the model with lowest AIC
bestModel=modelStat[which(modelStat$AIC==min(modelStat$AIC)),]
bestModel


##################
#4a
#Compare the top 100 sensitive consumers with all consumers
#new = merge(allData, top100Consumer, by = 'consumerID')
#summary(new)