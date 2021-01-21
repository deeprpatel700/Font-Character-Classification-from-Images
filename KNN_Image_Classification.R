setwd("C:\\Users\\deepp\\Google Drive\\MSDS\\MATH 6350 Data Mining\\Selected fonts")
getwd()
font_style1 <- read.csv('ERAS.csv') # ERAS
font_style2 <- read.csv('GADUGI.csv')# GADUGI
font_style3 <- read.csv('PALATINO.csv') #PALATINO

### Preliminary treatment of the data set

# Deleting 9 columns
font_style1 <- font_style1[-c(2,3,6:12)]
font_style2 <- font_style2[-c(2,3,6:12)]
font_style3 <- font_style3[-c(2,3,6:12)]

# Checking if any rows have missing values
length(font_style1[!complete.cases(font_style1),])
length(font_style2[!complete.cases(font_style2),])
length(font_style3[!complete.cases(font_style3),])

# Define 3 classes
library(dplyr)
CL1 <- filter(font_style1, strength==0.4 & italic==0); dim(CL1)[1]
CL1$class <- 'CL1'
CL2 <- filter(font_style2, strength==0.4 & italic==0); dim(CL2)[1]
CL2$class <- 'CL2'
CL3 <- filter(font_style3, strength==0.4 & italic==0); dim(CL3)[1]
CL3$class <- 'CL3'

names(CL1)[4:403]<- paste("X", seq(1, 400), sep="")
names(CL2)[4:403]<- paste("X", seq(1, 400), sep="")
names(CL3)[4:403]<- paste("X", seq(1, 400), sep="")

# Full data set
DATA <- rbind(CL1,CL2,CL3)


### Part 0

# Mean and SD of the 400 columns (features)
DATA_features <- DATA[4:403]
DATA_mean_std <- sapply(DATA_features, function(DATA_features) c( "Stand dev" = sd(DATA_features,na.rm=TRUE), 
                         "Mean"= mean(DATA_features,na.rm=TRUE)))

DATA_features2<- DATA_features
library(LICORS)
normalize(data.matrix(DATA_features2))
# Creating "SDATA" - standardized data set / Rescalled data matrix
count = 0
for (Xj in DATA_features) {
  count = count + 1
  mean = mean(Xj)
  sd = sd(Xj)
  Yj = (Xj-mean)/sd
  
  if (count==1) {SDATA <- data.frame('r0c0'=Yj); next}
  SDATA <- cbind(SDATA,Yj)
}
colnames(SDATA) <- colnames(DATA_features)
head(SDATA)
rowSums(SDATA)

# Correlation matrix of 400 features
CORR <- round(cor(SDATA),3)
abs_CORR <- abs(CORR)

abs_CORR
# Extracting 10 pairs of features

# Make diagonals 'NA' instead of '1'
diag(abs_CORR) <- NA
# Get the actual top 20 values
top_20_values <- sort(abs_CORR, decreasing=TRUE)[0:20]
# Locate which values are greater than the 20th largest value
x <- which(abs_CORR>=sort(abs_CORR, decreasing = T)[20], arr.ind = T)
x.order <- order(abs_CORR[x], decreasing = T)
# Get the positions of the top 20 values
sorted_positions <- x[x.order, ]

cat(unique(top_20_values), sep="\n")
sorted_positions
SDATA <- cbind(SDATA,'class'=DATA$class)
### PART 1

# 1.0
SDATA_CL1 <- filter(SDATA, class == 'CL1')
n1 <- nrow(SDATA_CL1)
train1 <- sample(1:n1, 0.8*n1)
trainCL1 <- SDATA_CL1[train1,]
testCL1 <- SDATA_CL1[-train1,]

SDATA_CL2 <- filter(SDATA, class == 'CL2')
n2 <- nrow(SDATA_CL2)
train2 <- sample(1:n2, 0.8*n2)
trainCL2 <- SDATA_CL2[train2,]
testCL2 <- SDATA_CL2[-train2,]

SDATA_CL3 <- filter(SDATA, class == 'CL3')
n3 <- nrow(SDATA_CL3)
train3 <- sample(1:n3, 0.8*n3)
trainCL3 <- SDATA_CL3[train3,]
testCL3 <- SDATA_CL3[-train3,]

TRAINSET <- rbind(trainCL1,trainCL2,trainCL3)
x.TRAINSET <- TRAINSET[-401]
y.TRAINSET <- TRAINSET[,401]
TESTSET <- rbind(testCL1,testCL2,testCL3)
x.TESTSET <- TESTSET[-401]
y.TESTSET <- TESTSET[,401]

accuracy<- function(x){
  sum(diag(x)/(sum(rowSums(x))))*100
}

# 1.1
#install.packages('class')
library(class)
set.seed(1)
knn.pred <- knn(train = TRAINSET[-401],test = TRAINSET[-401],cl = TRAINSET[,401], k=12)
trainperf12 = mean(y.TRAINSET == knn.pred) # Training accuracy 81.08%
trainperf12


set.seed(1)
knn.pred <- knn(x.TRAINSET, x.TESTSET, y.TRAINSET, k=12)
testperf12 = mean(y.TESTSET == knn.pred) # Testing accuracy 73.65%
testperf12
test12conf<- table(knn.pred, y.TESTSET)
test12conf
sum(diag(test12conf))/sum(test12conf)
# 1.2
k_values <- c(5, 10 , 15, 20, 30, 40, 50 ,100)
testperfk <- numeric(length(k_values))
trainperfk <- numeric(length(k_values))

set.seed(1)
for (i in 1:length(k_values)) {
  knn.test.pred <- knn(x.TRAINSET, x.TESTSET, y.TRAINSET, k=k_values[i])
  testperfk[i] <-  (mean(y.TESTSET == knn.test.pred))*100
  
  knn.train.pred <- knn(x.TRAINSET, x.TRAINSET, y.TRAINSET, k=k_values[i])
  trainperfk[i] <-  (mean(y.TRAINSET == knn.train.pred))*100
  
  cat('For k=',k_values[i], ', Test Accuracy is',round(testperfk[i],2), '%   
        & Train Accuracy is', round(trainperfk[i],2), "%", '\n')
}


plot(k_values, trainperfk, ylim=range(c(trainperfk,testperfk)),type='l',col='blue',
     xlab = "K values", ylab = "Accuracy", main = 'Testing(Red) & Training(Blue) Accuracy')
par(new = TRUE)
plot(k_values, testperfk, ylim=range(c(trainperfk,testperfk)),type='l',
     axes = FALSE, xlab = "", ylab = "", col='red')


# 1.3
k_values <- 5:20
testperfk <- numeric(length(k_values))
trainperfk <- numeric(length(k_values))
error_rate_train<- numeric(length(k_values))
error_rate_test<- numeric(length(k_values))

for (i in 1:length(k_values)) {
  knn.pred <- knn(x.TRAINSET, x.TESTSET, y.TRAINSET, k=k_values[i])
  testperfk[i] <-  (mean(y.TESTSET == knn.pred))*100
  error_rate_test[i]<- 1-(testperfk[i]/100)
  
  knn.train.pred <- knn(x.TRAINSET, x.TRAINSET, y.TRAINSET, k=k_values[i])
  trainperfk[i] <-  (mean(y.TRAINSET == knn.train.pred))*100
  error_rate_train[i]<- 1-(trainperfk[i]/100)
  
  cat('For k=',k_values[i], ', Test Accuracy is',round(testperfk[i],2), '%   
        & Train Accuracy is', round(trainperfk[i],2), "%", '\n')
}

# To see the plot of test accuracy vs. K-value compared with train accuracy 
plot(k_values, trainperfk, ylim=range(c(trainperfk,testperfk)),type='l',col='blue',
     xlab = "K values", ylab = "Accuracy (%)", main = 'Testing(Red) & Training(Blue) Accuracy')
par(new = TRUE)
plot(k_values, testperfk, ylim=range(c(trainperfk,testperfk)),type='l',
     axes = FALSE, xlab = "", ylab = "", col='red')


# To see the plot of test error rate vs. K-value compared with train error rate 
plot(k_values,error_rate_test,type = 'l',xlab = 'K values', ylab = 'Error rate', 
     ylim=range(c(error_rate_test,error_rate_train)), col='red',
     main = 'Testing Error Rate (Red) & Training Error Rate (Blue) Accuracy vs. K-values')
par(new = TRUE)
plot(k_values,error_rate_train, type="l", 
     ylim=range(c(error_rate_test,error_rate_train)),axes=FALSE,
     xlab="",ylab="", col="blue")


# 1.4
max(testperfk) # Highest accuracy.
which.max(testperfk) # The index of best K.
kbest <- k_values[which.max(testperfk)]; kbest # The best K value.

# Training set confusion matrix
set.seed(1)
knn.train.pred_kbest <- knn(train = x.TRAINSET,test = x.TRAINSET,cl = y.TRAINSET, k=kbest)
trainperf_kbest = mean(y.TRAINSET == knn.train.pred_kbest)

trainconf = table('Prediction (Train)'=knn.train.pred_kbest, 'True (Train)'=y.TRAINSET)
trainconf   #Train confusion matrix in number of classes

confmat.train<- as.matrix(trainconf, rownames=TRUE, colnames=TRUE)
trainconf_percent<- scale(confmat.train, center=FALSE, scale=rowSums(confmat.train))*100
round(trainconf_percent,2)     #Training confusion matrix in percentage


N1<-sum(trainconf); N1
diag_train_sum<-sum(diag(trainconf)); diag_train_sum  #N of cases correctly classified
sum(trainconf)-sum(diag(trainconf))  #N of cases incorrectly classified
p1 <- (diag_train_sum/N1); 
trainconf_acc<- p1*100    #global accuracy of train confusion matrix
trainconf_acc

# Testing set confusion matrix
set.seed(1)
knn.test.pred_kbest <- knn(x.TRAINSET, x.TESTSET, y.TRAINSET, k=kbest)
testperf_kbest = mean(y.TESTSET == knn.test.pred_kbest) 

testconf = table('Prediction (Test)'=knn.test.pred_kbest, 'True (Test)'=y.TESTSET)
testconf    #Test confusion matrix in number of classes

confmat.test<- as.matrix(testconf, rownames=TRUE, colnames=TRUE)
testconf_percent<- scale(confmat.test, center=FALSE, scale=rowSums(confmat.test))*100
round(testconf_percent,2)       #Testing confusion matrix in percentage

N2<- as.numeric(sum(testconf)); N2
diag_test_sum<-sum(diag(testconf))    #N of cases correctly classified
sum(testconf)-sum(diag(testconf))  #N of cases incorrectly classified
p2 <- (diag_test_sum/N2)   
testconf_acc<- p2*100 #global accuracy of test confusion matrix
testconf_acc

## ------- 1.5------
#90% Confidence interval of each diagonal term individually
#(Please see below for 90% confidence interval for the sum of 3 diagonal terms)
trainconf
sum(trainconf)   #sum of the trainconf matrix
round(trainconf_percent,2) #accuracies of trainconf matrix
rowSums(confmat.train)   #row sums of trainconf matrix
Ntrain.CL1<- sum(confmat.train[1,])    #sum of train CL1 values
Ntrain.CL2<- sum(confmat.train[2,])   #sum of train CL2 values
Ntrain.CL3<- sum(confmat.train[3,])   #sum of train CL3 values
ptrain.CL1<- confmat.train[1,1]/Ntrain.CL1  #observed frequency of correct train CL1 classes
ptrain.CL2<- confmat.train[2,2]/Ntrain.CL2  #observed frequency of correct train CL2 classes
ptrain.CL3<- confmat.train[3,3]/Ntrain.CL3  #observed frequency of correct train CL3 classes

testconf
sum(testconf)     #sum of testconf matrix
round(testconf_percent,2) #accurancies % of testconf matrix
rowSums(confmat.test)   #row sums of testconf matrix

Ntest.CL1<- sum(confmat.test[1,])   #sum of test CL1 values
Ntest.CL2<- sum(confmat.test[2,])    #sum of test CL2 values
Ntest.CL3<- sum(confmat.test[3,])    #sum of test CL3 values
ptest.CL1<- confmat.test[1,1]/Ntest.CL1
ptest.CL2<- confmat.test[2,2]/Ntest.CL2
ptest.CL3<- confmat.test[3,3]/Ntest.CL3

#90% Confidence interval of each diagonal term individually
#(see below for 90% confidence interval for sum of 3 diagonal terms)

#TRAINSET
#CI for CL1 diagonal term
sigma_train.CL1<- sqrt(ptrain.CL1*(1-ptrain.CL1)/Ntrain.CL1) 
lower_train.CL1<- ptrain.CL1-(1.6*sigma_train.CL1); lower_train.CL1
upper_train.CL1<- ptrain.CL1 + (1.6*sigma_train.CL1); upper_train.CL1

#CI for CL2 diagonal term
sigma_train.CL2<- sqrt(ptrain.CL2*(1-ptrain.CL2)/Ntrain.CL2) 
lower_train.CL2<- ptrain.CL2-(1.6*sigma_train.CL2); lower_train.CL2
upper_train.CL2<- ptrain.CL2 + (1.6*sigma_train.CL2); upper_train.CL2

#CI for CL3 diagonal term
sigma_train.CL3<- sqrt(ptrain.CL3*(1-ptrain.CL3)/Ntrain.CL3) 
lower_train.CL3<- ptrain.CL3-(1.6*sigma_train.CL3); lower_train.CL3
upper_train.CL3<- ptrain.CL3 + (1.6*sigma_train.CL3); upper_train.CL3

#TESTSET
#CI for CL1 diagonal term
sigma_test.CL1<- sqrt(ptest.CL1*(1-ptest.CL1)/Ntest.CL1) 
lower_test.CL1<- ptest.CL1-(1.6*sigma_test.CL1); lower_test.CL1
upper_test.CL1<- ptest.CL1 + (1.6*sigma_test.CL1); upper_test.CL1

#CI for CL2 diagonal term
sigma_test.CL2<- sqrt(ptest.CL2*(1-ptest.CL2)/Ntest.CL2) 
lower_test.CL2<- ptest.CL2-(1.6*sigma_test.CL2); lower_test.CL2
upper_test.CL2<- ptest.CL2 + (1.6*sigma_test.CL2); upper_test.CL2

#CI for CL3 diagonal term
sigma_test.CL3<- sqrt(ptest.CL3*(1-ptest.CL3)/Ntest.CL3) 
lower_test.CL3<- ptest.CL3-(1.6*sigma_test.CL3); lower_test.CL3
upper_test.CL3<- ptest.CL3 + (1.6*sigma_test.CL3); upper_test.CL3

#---------

#Confidence interval for sum of 3 diagonal terms (TRAINSET(p1) & TESTSET(p2))

N1<-as.numeric(sum(trainconf)); N1
diag_train_sum<-sum(diag(trainconf)); diag_train_sum  #N of cases correctly classified
p1 <- (diag_train_sum/N1); 

N2<- as.numeric(sum(testconf)); N2
diag_test_sum<-sum(diag(testconf))    #N of cases correctly classified
p2 <- (diag_test_sum/N2)   
sigma_p1<- sqrt(p1*((1-p1)/N1))   
sigma_p2<- sqrt(p2*(1-p2)/N2)

#Trainset 
lower_limit.p1<- p1-(1.6*sigma_p1); lower_limit.p1
upper_limit.p1<- p1+(1.6*sigma_p1); upper_limit.p1

#Testset
lower_limit.p2<- p2-(1.6*sigma_p2); lower_limit.p2
upper_limit.p2<- p2+(1.6*sigma_p2); upper_limit.p2

#As there is no overlap, disjoint confidence intervals exists and q1>q2
# If there is overlap? Try below

std<- sqrt((sigma_p1)^2 +(sigma_p2)^2); std
lower_limit.q1subq2<- (p1-p2)-(1.6*std); lower_limit.q1subq2
upper_limit.q1subq2<- (p1-p2)+(1.6*std); upper_limit.q1subq2

lower_limit.q2subq1<- (p2-p1)-(1.6*std); lower_limit.q2subq1
upper_limit.q2subq1<- (p2-p1)+(1.6*std); upper_limit.q2subq1

## -------------1.6------

pack_pix <- function(pack_num) {
  if (pack_num == 1){a = 1; b = 200}
  else if (pack_num == 2){a = 11; b = 200}
  else if (pack_num == 3){a = 211; b = 400}
  else if (pack_num == 4){a = 201; b = 400}
  pack_vec = numeric(length=100)
  count = 0
  for (i in seq(a,b,20)) {
    for (j in 0:9){
      count = count+1
      pack_vec[count] = (i+j)
    }
  }
  return(pack_vec)
}
#pack_pix(1)

pack_pix(4)

PACK1_test <- TESTSET[c(pack_pix(1),401)]
PACK1_train <- TRAINSET[c(pack_pix(1),401)]
set.seed(1)
knn.test.PACK1 <- knn(train = PACK1_train[-101],test = PACK1_test[-101],cl = PACK1_train[,101], k=kbest)
w1 = mean(PACK1_test[,101] == knn.test.PACK1);w1

### 1.7
PACK2_test <- TESTSET[c(pack_pix(2),401)]
PACK2_train <- TRAINSET[c(pack_pix(2),401)]
set.seed(1)
knn.test.PACK2 <- knn(train = PACK2_train[-101],test = PACK2_test[-101],cl = PACK2_train[,101], k=kbest)
w2 = mean(PACK2_test[,101] == knn.test.PACK2);w2
                        
PACK3_test <- TESTSET[c(pack_pix(3),401)]
PACK3_train <- TRAINSET[c(pack_pix(3),401)]
set.seed(1)
knn.test.PACK3 <- knn(train = PACK3_train[-101],test = PACK3_test[-101],cl = PACK3_train[,101], k=kbest)
w3 = mean(PACK3_test[,101] == knn.test.PACK3);w3

PACK4_test <- TESTSET[c(pack_pix(4),401)]
PACK4_train <- TRAINSET[c(pack_pix(4),401)]
set.seed(1)
knn.test.PACK4 <- knn(train = PACK4_train[-101],test = PACK4_test[-101],cl = PACK4_train[,101], k=kbest)
w4 = mean(PACK4_test[,101] == knn.test.PACK4);w4

PACK2_test
### 1.8
# Normalizing the 4 weights so they sum to 1
library(LICORS) # normalize function gotten from here
weights <- c(w1,w2,w3,w4)
weights <- normalize(weights)
w1=weights[1];w2=weights[2];w3=weights[3];w4=weights[4]

sum(weights)

# Multiplying by sqrt(w)
PACK1_test_w <- cbind(PACK1_test[0:100]*(w1),PACK1_test[101])
PACK1_train_w <- cbind(PACK1_train[0:100]*(w1),PACK1_train[101])

PACK2_test_w <- cbind(PACK2_test[0:100]*(w2),PACK2_test[101])
PACK2_train_w <- cbind(PACK2_train[0:100]*(w2),PACK2_train[101])

PACK3_test_w <- cbind(PACK3_test[0:100]*(w3),PACK3_test[101])
PACK3_train_w <- cbind(PACK3_train[0:100]*(w3),PACK3_train[101])

PACK4_test_w <- cbind(PACK4_test[0:100]*(w4),PACK4_test[101])
PACK4_train_w <- cbind(PACK4_train[0:100]*(w4),PACK4_train[101])



library(plyr)
TESTSET_w <- data.frame(matrix(ncol=401,nrow=0, dimnames=list(NULL, names(TESTSET))))
TESTSET_w <- rbind.fill(TESTSET_w,cbind(PACK1_test_w,PACK2_test_w,PACK3_test_w,PACK4_test_w))
TRAINSET_w <- data.frame(matrix(ncol=401,nrow=0, dimnames=list(NULL, names(TRAINSET))))
TRAINSET_w <- rbind.fill(TRAINSET_w,cbind(PACK1_train_w,PACK2_train_w,PACK3_train_w,PACK4_train_w))


# KNN on the TEST data with normalized weights
set.seed(1)
knn.test.w <- knn(train = TRAINSET_w[-401],test = TESTSET_w[-401],cl = TRAINSET_w[,401], k=kbest)
testperfw = mean(TESTSET_w[,401] == knn.test.w); testperfw

# Confusion matrix of weighted data on TESTSET
conftest.weight<- table('Prediction'=knn.test.wn, 'True'=TESTSET_wn[,401])
conftest.weight

confmat.testweight<- as.matrix(conftest.weight, rownames=TRUE, colnames=TRUE)
conftest_weightpercent<- scale(confmat.testweight, center=FALSE, 
                               scale=rowSums(confmat.testweight))*100
round(conftest_weightpercent,2)

global_acc.testw<- (sum(diag(conftest.weight))/sum(conftest.weight))*100 ;
global_acc.testw


