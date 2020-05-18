# Libraries
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

# Data

dataset = read.csv('Task_data.csv')
dataset
dataset$Class = factor(dataset$Class,
                       levels = c('PRAD' , 'LUAD' ,'BRCA','KIRC','COAD'),
                       labels = c(1,2,3,4,5))

data = dataset[, 1:100]
# Feature Selection
set.seed(111)
boruta <- Boruta(Class ~ ., data = data, doTrace = 2, maxRuns = 1000)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

#************************************************
confirmed = boruta$finalDecision[ boruta$finalDecision == "Confirmed"]
data_n=getConfirmedFormula(boruta)

data_n
data_new=data[,c(names(confirmed))] 
write.csv(data_new , file="new_dataaaaa.csv " , row.names=F )
#***********************************************























