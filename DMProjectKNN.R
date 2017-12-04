library(leaps)
library(readr)
creditcard <- read_csv("~/Desktop/Data Mining-758T/DATA/creditcard.csv")
creditcard$Time <- NULL
regfit.fwd = regsubsets(Class~., data = creditcard, nvmax = 29, method = "forward")

summary(regfit.fwd)
reg.summary = summary(regfit.fwd)

which.min(reg.summary$bic)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)

regfit.bck = regsubsets(Class~., data = creditcard, nvmax = 29, method = "backward")

reg.bck.summary=summary(regfit.bck)

which.min(reg.bck.summary$bic)
which.max(reg.bck.summary$adjr2)
which.min(reg.bck.summary$cp)

#### Based on Adjr2 and cp, for both backward and forward, we keep all 29 variables
# Now we need to normalize each variable
fun <- function(x){ 
  a <- mean(x) 
  b <- sd(x) 
  (x - a)/(b) 
} 
creditcard[,1:29] <- apply(creditcard[,1:29], 2, fun)

#
library(caret)


set.seed(12345)
inTrain <- createDataPartition(creditcard$Class, p=0.6, list=FALSE)
#
cd.train <- data.frame(creditcard[inTrain,])
cd.temp <- data.frame(creditcard[-inTrain,])

inVal <- createDataPartition(cd.temp$Class, p=0.6, list=FALSE)
cdvalidation <- data.frame(cd.temp[inVal,])
cdtest <- data.frame(cd.temp[-inVal,])


library(class)

#(
train_input <- as.matrix(cd.train[,-30])
train_output <- as.vector(cd.train[,30])
validate_input <- as.matrix(cdvalidation[,-30])
test_input <- as.matrix(cdtest[,-30])
#
#
kmax <- 5
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)
#
#for (i in 1:kmax){
  #prediction <- knn(train_input, train_input,train_output, k=i)
  #prediction2 <- knn(train_input, validate_input,train_output, k=i)
  #prediction3 <- knn(train_input, test_input,train_output, k=i)
  #
  # The confusion matrix for training data is:
  #CM1 <- table(prediction, cd.temp$Class)
  # The training error rate is:
  #ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for validation data is: 
 # CM2 <- table(prediction2, cd.validation$Class)
  #ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}


prediction <- knn(train = cd.train, test = cd.temp,cl = train_output, k=20)

CM1 <- table(prediction, cd.temp$Class)


#####

fit <- glm(Class~., data = cd.train, family = "binomial")

cutoff <- 0.7


logfit <- predict(fit, newdata = cd.temp)

Log.credcard.pred <- ifelse(logfit > cutoff, 1, 0)

(Log.credcard.conf <- table(cd.temp$Class, Log.credcard.pred))
CM1
