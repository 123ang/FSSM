library(caTools)
library(rpart)
library(randomForest)
set.seed(123)
######################decision tree######################

dataset1= read.csv("filter_data_2017.csv")
dataset1 <- cbind(
  dataset1$accepted,
  dataset1$number_terms,
  dataset1$monthly_cost,
  dataset1$credit_score,
  dataset1$offered_amount
)
dataset1<- dataset1[1:5000,]
colnames(dataset1) <- c("accepted", 
                      "number_terms",
                       "monthly_cost",
                      "credit_score",
                       "offered_amount")

dataset1 = as.data.frame(dataset1)
dataset1$accepted <- ifelse(dataset1$accepted=="true",1,0)

dataset1$accepted = factor(dataset1$accepted, levels = c(0,1))
dataset1$number_terms = as.numeric(dataset1$number_terms)
dataset1$monthly_cost = as.numeric(dataset1$monthly_cost)
dataset1$credit_score = as.numeric(dataset1$credit_score)
dataset1$offered_amount = as.numeric(dataset1$offered_amount)

split = sample.split(dataset1$accepted, SplitRatio = 0.7)
training_set = subset(dataset1, split == TRUE)
test_set = subset(dataset1, split == FALSE)

classfier = rpart(formula = accepted ~., data = training_set)
y_pred = predict(classfier, newdata = test_set[-1],type="class")

y_act = test_set$accepted
cm = table(test_set[,1],y_pred)
plot(classfier)
text(classfier)

# number of instances
n = sum(cm)
# number of classes
nc = nrow(cm) 
# number of correctly classified instances per class 
diag = diag(cm) 
# number of instances per class
rowsums = apply(cm, 1, sum)
# number of predictions per class
colsums = apply(cm, 2, sum) 
#distribution of instances over the actual classes
p = rowsums / n  
# distribution of instances over the predicted classes
q = colsums / n 
# accuracy
accuracy = sum(diag) / n 
#precision
precision = diag / colsums 
#recall
recall = diag / rowsums 
#f1
f1 = 2 * precision * recall / (precision + recall) 
#summarize
data.frame(accuracy,precision, recall, f1) 



###################### random forest###############################
#dataset1= read.csv("filter_data_1.csv")

#encoding target feature as factor
#dataset1$small_farmer = factor(dataset1$small_farmer, levels = c(0,1))
#split = sample.split(dataset1$small_farmer, SplitRatio = 0.7)
#training_set = subset(dataset1, split == TRUE)
#test_set = subset(dataset1, split == FALSE)
# classfier = randomForest(x = training_set[2],
#                          y = training_set$small_farmer,
#                          ntree=10)
classfier = randomForest(x = training_set[-1],
                         y = training_set$accepted,
                         ntree=10)
y_pred = predict(classfier, newdata = test_set[-1])
cm = table(test_set[,1],y_pred)

# number of instances
n = sum(cm)
# number of classes
nc = nrow(cm) 
# number of correctly classified instances per class 
diag = diag(cm) 
# number of instances per class
rowsums = apply(cm, 1, sum)
# number of predictions per class
colsums = apply(cm, 2, sum) 
#distribution of instances over the actual classes
p = rowsums / n  
# distribution of instances over the predicted classes
q = colsums / n 
# accuracy
accuracy = sum(diag) / n 
#precision
precision = diag / colsums 
#recall
recall = diag / rowsums 
#f1
f1 = 2 * precision * recall / (precision + recall) 
#summarize
data.frame(accuracy,precision, recall, f1)

#########################logistic regression###############################



# Fitting Logistic Regression to the Training set
# classifier = glm(formula = small_farmer ~ .,
#                  family = binomial,
#                  data = training_set)
classifier = glm(formula = accepted ~ .,
                 family = binomial,
                 data = training_set)
# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-1])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[,1], y_pred > 0.5)
.# number of instances
n = sum(cm)
# number of classes
nc = nrow(cm) 
# number of correctly classified instances per class 
diag = diag(cm) 
# number of instances per class
rowsums = apply(cm, 1, sum)
# number of predictions per class
colsums = apply(cm, 2, sum) 
#distribution of instances over the actual classes
p = rowsums / n  
# distribution of instances over the predicted classes
q = colsums / n 
# accuracy
accuracy = sum(diag) / n 
#precision
precision = diag / colsums 
#recall
recall = diag / rowsums 
#f1
f1 = 2 * precision * recall / (precision + recall) 
#summarize
data.frame(accuracy,precision, recall, f1)

