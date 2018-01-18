#Part 1
#Question a)
dataset<-read.csv(file.choose())  #Read the "Assignment 1 Data.csv" file

#Question b)
dataset$rDate<-as.Date(dataset$date,"%m/%d/%Y")  

#Question c)
TreatmentPeriod <- dataset[dataset$isTreatmentPeriod == 1, ]
min(TreatmentPeriod$rDate)  #2012-05-22

#Question d)
TreatmentGroup <- dataset[dataset$isTreatmentGroup == 1, ]
lm(log(TreatmentGroup$revenue) ~ TreatmentGroup$isTreatmentPeriod)
summary(lm(log(TreatmentGroup$revenue) ~ TreatmentGroup$isTreatmentPeriod))

#Question e)
PretreatmentPeriod <- dataset[dataset$isTreatmentPeriod == 0, ]
lm(log(PretreatmentPeriod$revenue) ~ PretreatmentPeriod$isTreatmentGroup)
summary(lm(log(PretreatmentPeriod$revenue) ~ PretreatmentPeriod$isTreatmentGroup))

#Question f)
lm(log(TreatmentPeriod$revenue) ~ TreatmentPeriod$isTreatmentGroup)  #Using the data in treatment period
summary(lm(log(TreatmentPeriod$revenue) ~ TreatmentPeriod$isTreatmentGroup))

#Question g)
TreatmentPeriod$month <- substr(TreatmentPeriod$date,1,1)
summary(lm(log(TreatmentPeriod$revenue) ~ TreatmentPeriod$isTreatmentGroup:TreatmentPeriod$month))


#Part 2
#Question f)
#Using revenue instead of log(revenue) in the regression
summary(lm(TreatmentGroup$revenue ~ TreatmentGroup$isTreatmentPeriod))
summary(lm(TreatmentPeriod$revenue ~ TreatmentPeriod$isTreatmentGroup))

