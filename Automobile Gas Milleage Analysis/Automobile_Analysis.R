Auto1 <- read.table(file = "Auto.csv", sep = ",", header=T)


mpg01 = I(Auto1$mpg >= median(Auto1$mpg))
Auto = data.frame(mpg01, Auto1[,-1]) ## replace column "mpg" by "mpg01".
Auto$mpg01<-as.factor(as.integer(Auto$mpg01))
table(Auto$mpg01)
summary(Auto)

############# Exploratory Data Analysis ######################
# a)	'Cylinder' vs 'mpg01'
tb_cy= xtabs(~Auto$mpg01+Auto$cylinders)

barplot(prop.table(tb_cy), axes=T, space=0.3, horiz=T,
        xlab="Proportion of High gas mileage (blue) vs Low gas mileage (Red)",
        col=c("red","blue"), main="Gas mileage by Cylinder")

# b)  'Displacement'  vs 'mpg01'

library(ggplot2)
ggplot(data=Auto, aes(x = mpg01, y = displacement, fill=mpg01)) + 
  geom_boxplot()+
  labs(title="Gas mileage by Displacement")+theme(plot.title = element_text(hjust = 0.5))    

# c)  'Horsepower'  vs 'mpg01'

library(ggplot2)
ggplot(data=Auto, aes(x = mpg01, y = horsepower, fill=mpg01)) + 
  geom_boxplot()+
  labs(title="Gas mileage by Horsepower")+theme(plot.title = element_text(hjust = 0.5))   

# d)  'Weight'  vs 'mpg01'

library(ggplot2)
ggplot(data=Auto, aes(x = mpg01, y = weight, fill=mpg01)) + 
  geom_boxplot()+
  labs(title="Gas mileage by Weight")+theme(plot.title = element_text(hjust = 0.5))  

# e)  'Acceleration' vs 'mpg01'

library(ggplot2)
ggplot(data=Auto, aes(x = mpg01, y = acceleration, fill=mpg01)) + 
  geom_boxplot()+
  labs(title="Gas mileage by Acceleration")+theme(plot.title = element_text(hjust = 0.5))  

# f)	'Year' vs 'mpg01'
tb_year= xtabs(~Auto$mpg01+Auto$year)

barplot(prop.table(tb_year), axes=T, space=0.3, horiz=T,
        xlab="Proportion of High gas mileage (blue) vs Low gas mileage (Red)",
        col=c("red","blue"), main="Gas mileage by Year")

# g)	'Origin' vs 'mpg01'
tb_ori= xtabs(~Auto$mpg01+Auto$origin)

barplot(prop.table(tb_ori), axes=T, space=0.3, horiz=T,
        xlab="Proportion of High gas mileage (blue) vs Low gas mileage (Red)",
        col=c("red","blue"), main="Gas mileage by Origin")

########################### Methods ##############################
Auto<-Auto[,-6]
set.seed(19)
testRows = sample(nrow(Auto),0.2*nrow(Auto))
testData = Auto[testRows, ]
trainData = Auto[-testRows, ]
test_y=testData[,1]
test_x=testData[,2:7]
train_y=trainData[,1]
train_x=trainData[,2:7]

TrainErr <- NULL;
TestErr  <- NULL; 

### Method 1: LDA
library(MASS)
# fit1 <- lda( y ~ ., data= vowel.train, CV= TRUE)
mod1 <- lda(train_x, train_y)

## training error 
pred1 <- predict(mod1,train_x)$class
TrainErr <- c(TrainErr, mean( pred1  != train_y))
TrainErr

## testing error 
pred1test <- predict(mod1,test_x)$class; 
pred1test
TestErr <- c(TestErr,mean(pred1test != test_y))  
TestErr

# Confusion matrix
conmat1<-table(pred1test,  test_y) 
conmat1
# Accuracy
Acc1<-(conmat1[1,1]+conmat1[2,2])/(conmat1[1,1]+conmat1[2,2]+conmat1[1,2]+conmat1[2,1])
Acc1

## Method 2: QDA
mod2 <- qda(train_x, train_y)
## Training Error 
pred2 <- predict(mod2,train_x)$class
TrainErr <- c(TrainErr, mean( pred2!= train_y))
TrainErr


##  Testing Error 
pred2test <- predict(mod2,test_x)$class; 
TestErr <- c(TestErr, mean( pred2test != test_y))
TestErr

# Confusion matrix
conmat2<-table(pred2test,  test_y) 
conmat2
# Accuracy
Acc2<-(conmat2[1,1]+conmat2[2,2])/(conmat2[1,1]+conmat2[2,2]+conmat2[1,2]+conmat2[2,1])
Acc2

## Method 3: Naive Bayes
library(e1071)
mod3 <- naiveBayes( train_x, train_y)
## Training Error
pred3 <- predict(mod3, train_x);
TrainErr <- c(TrainErr, mean( pred3 != train_y))
TrainErr 

## Testing Error 
TestErr <- c(TestErr,  mean( predict(mod3,test_x) != test_y))
TestErr

# Confusion matrix
pred3test <- predict(mod3,test_x)

conmat3<-table(pred3test,  test_y) 
conmat3
# Accuracy
Acc3<-(conmat3[1,1]+conmat3[2,2])/(conmat3[1,1]+conmat3[2,2]+conmat3[1,2]+conmat3[2,1])
Acc3

### Method 4: Logistic regression 
library(nnet)
mod4 <- multinom( mpg01 ~., data=trainData) 

## Training Error  of (multinomial) logisitic regression
TrainErr <- c(TrainErr, mean( predict(mod4, train_x)  != train_y))
TrainErr
##  0.2215909 for miss.class.train.error
## Testing Error of (multinomial) logisitic regression
TestErr <- c(TestErr, mean( predict(mod4,test_x) != test_y) )
TestErr

# Confusion matrix
pred4test <- predict(mod4,test_x)

conmat4<-table(pred4test,  test_y) 
conmat4
# Accuracy
Acc4<-(conmat4[1,1]+conmat4[2,2])/(conmat4[1,1]+conmat4[2,2]+conmat4[1,2]+conmat4[2,1])
Acc4

### Method 5: KNN
library(class);  ## This R package includes the "KNN" algorithm
## KNN with kk=1,2,.,50 neighbors
kk <- 1:20; 

##The cross-validation error for this specific round for different k
cverror <- NULL;
for (i in 1:length(kk)){
  ypred.test <- knn(train_x, test_x, train_y, k = kk[i]);
  temptesterror <- mean(ypred.test  != test_y);
  cverror <- c(cverror, temptesterror); 
}


## This shows that KNN with k=1,3,5 ## yield the smallest CV error 
##  for this specific split 
plot(kk, cverror)

## K = 6
pred_knn6 <- knn(train_x, test_x, train_y, k = 6)

## Testing Error 
TestErr <- c(TestErr, mean( pred_knn6 != test_y) )
TestErr

# Confusion matrix
conmat_knn6<-table(pred_knn6,  test_y) 
conmat_knn6
# Accuracy
Acc_knn6<-(conmat_knn6[1,1]+conmat_knn6[2,2])/(conmat_knn6[1,1]+conmat_knn6[2,2]+conmat_knn6[1,2]+conmat_knn6[2,1])
Acc_knn6

## K = 11
pred_knn11 <- knn(train_x, test_x, train_y, k = 11)

## Testing Error 
TestErr <- c(TestErr, mean( pred_knn11 != test_y) )
TestErr

# Confusion matrix
conmat_knn11<-table(pred_knn11,  test_y) 
conmat_knn11
# Accuracy
Acc_knn11<-(conmat_knn11[1,1]+conmat_knn11[2,2])/(conmat_knn11[1,1]+conmat_knn11[2,2]+conmat_knn11[1,2]+conmat_knn11[2,1])
Acc_knn11

## K = 13
pred_knn13 <- knn(train_x, test_x, train_y, k = 13)

## Testing Error 
TestErr <- c(TestErr, mean( pred_knn13 != test_y) )
TestErr

# Confusion matrix
conmat_knn13<-table(pred_knn13,  test_y) 
conmat_knn13
# Accuracy
Acc_knn13<-(conmat_knn13[1,1]+conmat_knn13[2,2])/(conmat_knn13[1,1]+conmat_knn13[2,2]+conmat_knn13[1,2]+conmat_knn13[2,1])
Acc_knn13

################### Findings #######################
colnames(TestErr) <- c("LDA", "QDA", "Naive Bayes", "Logistic Regression", "KNN with K=11 or 13")
TestErr.df=as.data.frame(TestErr)
plot(as.vector(TestErr),xlab='Models',ylab='Testing errors', main='Testing errors of models')