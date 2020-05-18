# Decision Tree Classification template

# Importing the dataset
dataset = read.csv('new_data.csv')

# Encoding the target feature as factor
dataset$Class = factor(dataset$Class, levels= c('BRCA','COAD','KIRC','LUAD','PRAD'),
                       labels= c(1,2,3,4,5)) #3shan menf3sh at3aml m3 string lazm arkam 

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
#lo atnen 7to nfs al rkm hetl3o nfs training_set 
#w test_set 3shan hwa be5tarhom b shkl 3shwa2ii 
set.seed(123) 
split = sample.split(dataset$Class, SplitRatio = 0.9) #0.75  percentage of training_set
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting classifier to the Training set
library(rpart)
classifier=rpart(formula = Class ~ . , data=training_set)

# Predicting the Test set results
#type=class bet3aml m3 y_pred 3la anha factor ze class ali 
#f test_set al aslya 3shan a3raf akarnhom b b3d w ageb confussion matrix
y_pred = predict(classifier, newdata = test_set[,-83],type='class')

# Making the Confusion Matrix
cm = table(test_set[, 83], y_pred) #bkarn test set al 72e2ya w ali tk3ta mn modle

accuracy= (cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5])/ sum(cm)

#apply cross validation
library(lattice)
library(ggplot2)
library(caret)
folds=createFolds(dataset$Class,k=10)
cv=lapply(folds,function(x_fold){
  training_fold=dataset[-x_fold,]
  test_fold=dataset[x_fold,]
  classifier=rpart(formula=Class~. , data=training_fold)
  y_pred = predict(classifier, newdata = test_fold[-83],type='class')
  cm = table(test_fold[, 83], y_pred)
  accuracy=(cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5]/ sum(cm))
  return(accuracy)
})
tot_acc=mean(as.numeric(cv))



#plot tree hetb3li shkl tree bs mfrod m3mlsh feature scaling lma arsm shkl tree
plot(classifier)
text(classifier)

