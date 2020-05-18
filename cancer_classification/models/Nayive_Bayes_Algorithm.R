dataset=read.csv("Task_data.csv")
new = read.csv("new_data.csv")
new$class
new$class = dataset$Class

library(caTools)
set.seed(123)
split = sample.split(new$class, SplitRatio = 0.7)
training_set = subset(new, split == TRUE)
test_set = subset(new, split == FALSE)




library(e1071)
classifier =  naiveBayes(x = training_set[-83],
                         y = training_set$class)


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-83])

# Making the Confusion Matrix
cm = table(test_set[, 83], y_pred)
accuracy = ((cm[1,1] + cm[2,2] + cm[3,3]) + cm[4,4] + cm[5,5]) / (sum(cm)) 

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(new$class , k = 10)
cv = lapply(folds, function(x) {
  training_fold = new[-x, ]
  test_fold = new[x, ]
  classifier =  naiveBayes(x = training_fold[-83],
                           y = training_fold$class)
  
  y_pred = predict(classifier, newdata = test_fold[-83])
  cm = table(test_fold[, 83], y_pred)
  accuracy = (cm[1,1] + cm[2,2] + cm[3,3] + cm[4,4] + cm[5,5]) / 
    (sum(cm))
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy
