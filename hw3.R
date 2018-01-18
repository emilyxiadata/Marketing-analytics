#1a
#Load the dataset
setwd('//Users/lulu/Documents/06 Simon/11 Marketing Analytics Using R/')
data = read.csv("recommendDB.csv")
data = data[,-1]

#1b&1c
library(reshape)
data2 <- data[,c(1,3,2)]
data2 <- cast(data2, consumerID ~ rockyID, value='rating')
colnames(data2) = c('consumerID','Rocky.1','Rocky.2', 'Rocky.3', 'Rocky.4', 'Rocky.5')

#2a
correlation = cor(data2, use='complete.obs')
#Rocky 2 and Rocky 3 are most similar.

for (i in 1:5) {
  print((sum(correlation[i,])-1)/4)
}
#We can find the smallest one is for Rocky 1, so Rocky 1 is most different from others.

#2b
meanRatings = colMeans(data2, na.rm = TRUE)

#2c
dataRocky4 = subset(data2, !is.na(data2$Rocky.4))
meanRatings2 = colMeans(dataRocky4[2:6], na.rm = TRUE)

#3a
data3= subset(data2)
data3 = na.omit(data3)
simplelm = lm(Rocky.5 ~ Rocky.1 + Rocky.2 + Rocky.3 + Rocky.4, data=data3)
summary(simplelm)

#3b
library(rpart)
library(nnet)
rocky1Vec = c('','+Rocky.1','+poly(Rocky.1,2)','+log(Rocky.1)','+poly(Rocky.1,3)')
rocky2Vec = c('','+Rocky.2','+poly(Rocky.2,2)','+log(Rocky.2)','+poly(Rocky.2,3)')
rocky3Vec = c('','+Rocky.3','+poly(Rocky.3,2)','+log(Rocky.3)','+poly(Rocky.3,3)')
rocky4Vec = c('','+Rocky.4','+poly(Rocky.4,2)','+log(Rocky.4)','+poly(Rocky.4,3)')
formulaSet = paste('Rocky.5~1',apply(expand.grid(rocky1Vec,rocky2Vec,rocky3Vec,rocky4Vec),1,paste,collapse=''))

#3c
set.seed(90)

#3d
training = read.csv('rockyDB - trainingData.csv')
validation = read.csv('rockyDB - validationData.csv')

MSE = function(x){
  return(mean(x^2))
}

#Create dataframes to store the results
results_lm = data.frame(formulaSet)

results_rpart = data.frame(formulaSet)

results_nnet = data.frame(formulaSet)
size<- data.frame(c(1,2,3))
results_nnet <- merge(results_nnet,size)
colnames(results_nnet)=c('formulaSet','Size')

#Calculate MSE to evaluate models of lm, rpart and nnet
set.seed(90)
for(i in 1:length(formulaSet)){
  model_lm <- lm(as.formula(as.character(results_lm[i,1])),data=training)
  results_lm[i,'MSE'] <- MSE(validation$Rocky.5 - predict(model_lm,validation))
}

for(i in 2:length(formulaSet)){
  model_rpart <- rpart(as.formula(as.character(results_rpart[i,1])),data=training)
  results_rpart[i,'MSE'] <- MSE(validation$Rocky.5 - predict(model_rpart,validation))
}

for(i in 1:length(formulaSet))
  for (j in 1:3){
    model_nnet <- nnet(as.formula(as.character(results_nnet[i,1])),data=training,size=j,linout = 1,maxit = 1000)
    results_nnet[results_nnet$formulaSet == formulaSet[i] & results_nnet$Size == j,'MSE'] <- MSE(validation$Rocky.5 - predict(model_nnet,validation))
}

#3e
best_lm <- results_lm[which.min(results_lm$MSE), ]
best_rpart <- results_rpart[which.min(results_rpart$MSE), ]
best_nnet <- results_nnet[which.min(results_nnet$MSE), ]

best_lm$MSE
#0.9169
best_rpart$MSE
#0.9280
best_nnet$MSE
#0.8808

#store the best model
chosenModel = nnet(as.formula(as.character(best_nnet[1,'formulaSet'])),data=training,size=best_nnet[1,'Size'],linout = 1,maxit = 1000)
save(chosenModel, file = 'chosenModel.Rdata')

#3f
#With the full data set
order = order(runif(nrow(data3)))
training_full = subset(data3, order <= .6*nrow(data3))
validation_full = subset(data3, order >= .6*nrow(data3))

#Create dataframes to store the results
results_lm_full = data.frame(formulaSet)

results_rpart_full = data.frame(formulaSet)

results_nnet_full = data.frame(formulaSet)
results_nnet_full <- merge(results_nnet_full,size)
colnames(results_nnet_full)=c('formulaSet','Size')

#Calculate MSE to evaluate models of lm, rpart and nnet
set.seed(90)
for(i in 1:length(formulaSet)){
  model_lm <- lm(as.formula(as.character(results_lm_full[i,1])),data=training_full)
  results_lm_full[i,'MSE'] <- MSE(validation_full$Rocky.5 - predict(model_lm,validation_full))
}

for(i in 2:length(formulaSet)){
  model_rpart <- rpart(as.formula(as.character(results_rpart_full[i,1])),data=training_full)
  results_rpart_full[i,'MSE'] <- MSE(validation_full$Rocky.5 - predict(model_rpart,validation_full))
}

for(i in 1:length(formulaSet))
  for (j in 1:3){
    model_nnet <- nnet(as.formula(as.character(results_nnet_full[i,1])),data=training_full,size=j,linout = 1,maxit = 1000)
    results_nnet_full[results_nnet_full$formulaSet == formulaSet[i] & results_nnet_full$Size == j,'MSE'] <- MSE(validation_full$Rocky.5 - predict(model_nnet,validation_full))
  }

best_lm_full <- results_lm_full[which.min(results_lm_full$MSE), ]
best_rpart_full <- results_rpart_full[which.min(results_rpart_full$MSE), ]
best_nnet_full <- results_nnet_full[which.min(results_nnet_full$MSE), ]

best_lm_full$MSE
#0.9749
best_rpart_full$MSE
#0.9836
best_nnet_full$MSE
#0.9449

#store the best model
chosenModel_full = nnet(as.formula(as.character(best_nnet_full[1,'formulaSet'])),data=training_full,size=best_nnet_full[1,'Size'],linout = 1,maxit = 1000)
save(chosenModel_full, file = 'chosenModel_full.Rdata')