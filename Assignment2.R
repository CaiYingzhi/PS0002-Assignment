library(dplyr)
library(mlbench)
library(class)
library(e1071)
data("PimaIndiansDiabetes",package="mlbench")

#1 getting familiar with dataset by inspecting statistic and structure of dataset
dim(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)
str(PimaIndiansDiabetes)

#convert test outcome to binary variable & select predictors and Y for classification
pid <- PimaIndiansDiabetes[complete.cases(PimaIndiansDiabetes),]#removing missing values
dim(pid)
pid[,1:8] <- sapply(pid[,1:8], as.numeric)
pid <-pid%>%mutate(y=factor(ifelse(diabetes=="pos", 1,0)))%>% select(pregnant:age, y)
str(pid)

#2 split data into training and test set
set.seed(100)
training.idx <- sample(1: nrow(pid), size=nrow(pid)*0.8)
train.data <-pid[training.idx, ]
test.data <- pid[-training.idx, ]

#3 Apply logistic regression, KNN and SVM for classification
#logistic regression
mlogit <- glm(y ~., data = train.data, family = "binomial")
summary(mlogit)
#Significant predictors are: pregnant, glucose, pressure, mass, pedigree
Pred.p <-predict(mlogit, newdata =test.data, type = "response")
#letting threshold be 0.5
y_pred_num <-ifelse(Pred.p > 0.5, 1, 0)
y_pred <-factor(y_pred_num, levels=c(0, 1))
#Accuracy of the classification
mean(y_pred ==test.data$y )
#mean is 0.7597403
tab <-table(y_pred,actual = test.data$y)
tab
# Accuracy is (87+30)/(87+30+11+26) = 76.0%
# 26 cases of false negative, 11 cases of false positive
# False negative rate is 26/56 = 46.4%, false positive rate is 11/98 = 11.2%
# Misclassification rate is (26+11)/(87+30+26+11) = 24.0%

#KNN
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
pid[,1:8] <- sapply(pid[,1:8], nor)
str(pid)
#normalise dataset
set.seed(100)
training.idx <- sample(1: nrow(pid), size=nrow(pid)*0.8)
train.data  <-pid[training.idx, ]
test.data <- pid[-training.idx, ]
#try different k to find the best classifier
ac<-rep(0, 30)
for(i in 1:30){
  set.seed(101)
  knn.i<-knn(train.data[,1:8], test.data[,1:8], cl=train.data$y, k=i)
  ac[i]<-mean(knn.i ==test.data$y)
  cat("k=", i, " accuracy=", ac[i], "\n")
}
#Accuracy plot
plot(ac, type="b", xlab="K",ylab="Accuracy")
#best k value is 9,11,29, with highest accuracy. best k value gives accuracy = 77.3%
set.seed(101)
knn9<-knn(train.data[,1:8], test.data[,1:8], cl=train.data$y, k=9)
mean(knn9 ==test.data$y) 
table(pred_knn9 =knn9,actual=test.data$y)
# Accuracy is (90+29)/(90+29+8+27) = 77.3%
# 27 cases of false negative, 8 cases of false positive
# False negative rate is 27/56 = 48.2%, false positive rate is 8/98 = 8.2%
# Misclassification rate is (27+8)/(90+29+27+8) = 22.7%

set.seed(101)
knn11<-knn(train.data[,1:8], test.data[,1:8], cl=train.data$y, k=11)
mean(knn11 ==test.data$y) 
# 0.7727273

set.seed(101)
knn29<-knn(train.data[,1:8], test.data[,1:8], cl=train.data$y, k=29)
mean(knn29 ==test.data$y) 
# 0.7727273

#SVM
#linear kernel function
m.svm<-svm(y~., data = train.data, kernel = "linear")
summary(m.svm)
#
#predict newdata in test set
pred.svm <- predict(m.svm, newdata=test.data[,1:8])
#evaluate classification performance and check accuracy
table(pred.svm, test.data$y)
mean(pred.svm ==test.data$y)
#0.7597403
# With linear kernel, accuracy of classification is 76.0%, 24 false negative, 13 false positive
# False negative rate = 24/56 = 42.9%, False positive rate = 13/98 = 13.3%
# Misclassification rate = (24+13)/(85+24+13+32) = 24.0%

#non-linear kernel function
set.seed(123)
m.svm.tune<-tune.svm(y~., data=train.data, kernel="sigmoid", cost=10^(-1:2), gamma=c(.1,.5,1,2))
summary(m.svm.tune)
plot(m.svm.tune)
#predictions using best parameters: gamma = 0.1, cost = 0.1
#confusion matrix and accuracy
best.svm = m.svm.tune$best.model
pred.svm.tune = predict(best.svm, newdata=test.data[,1:8])
table(pred.svm.tune, test.data$y)
mean(pred.svm.tune ==test.data$y)
#0.7857143
#sigmoid kernel function give accuracy of 78.6%, 25 false negative, 8 false positive
#false negative rate = 25/56 = 44.6%, false positive rate = 8/98 = 8.2%
#Misclassification rate = (25+8)/(90+25+8+31) = 21.4%

#4
#Classification of accuracy sigmoid kernel of svm (best choice, accuracy = 78.6%) is highest.
#Followed by classification of accuracy for KNN (best k = 9,11,29) is (77.3%).
#logistic regression (76.0%) and linear kernel svm (76.0%) have similar accuracy.
#KNN and sigmoid of svm generates least false positive rate
#sigmoid of svm has least missclassification rate
#linear kernels svm has the highest false positive rate.
#All methods generates similar false negative rates,with linear kernel svm having least false negative rate (42.9%),
#and knn has highest false negative rate (48.2%).
#logistic regression has the highest miss classification rate (24%), followed by linear kernel of svm(24%), KNN(22.7%) and sigmoid kernel of svm(21.4%)

#However, using the definition provided by Prof Xiang
# Miss classification rate = False positive rate + False negative rate
# missclassification rate is 57.6% for logistic regression, 54.8% for KNN classification(k=9,11,29),
# 56.2% for linear kernel of svm, and 52.8% for sigmoid kernel of svm.
# we see that the highest is logistic regression and lowest is sigmoid kernel of svm.