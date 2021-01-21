setwd("C:\\Users\\deepp\\Google Drive\\Selected fonts")
getwd()


# New standardized Dataframe post-PCA with reduced features (obtained from KNN_with_PCA)
NFDATA<- read.csv('new_features.csv')
NFDATA$class<- as.factor(NFDATA$class)
head(NFDATA[,1:5])
# Define 3 classes
library(dplyr)
C1 <- filter(NFDATA, class=='C1'); 
C2 <- filter(NFDATA, class=='C2'); 
C3 <- filter(NFDATA, class=='C3'); 

r95= 109
n1 = nrow(C1)
n2 = nrow(C2)
n3 = nrow(C3)

#### Part 1 #####
set.seed(1)
random_sets<- function(NFDATA){
  NFDATA_CL1 <- filter(NFDATA, class == 'C1')
  train1 <- sample(1:n1, 0.8*n1)
  trainCL1 <- NFDATA_CL1[train1,]
  testCL1 <- NFDATA_CL1[-train1,]
  
  NFDATA_CL2 <- filter(NFDATA, class == 'C2')
  train2 <- sample(1:n2, 0.8*n2)
  trainCL2 <- NFDATA_CL2[train2,]
  testCL2 <- NFDATA_CL2[-train2,]
  
  NFDATA_CL3 <- filter(NFDATA, class == 'C3')
  train3 <- sample(1:n3, 0.8*n3)
  trainCL3 <- NFDATA_CL3[train3,]
  testCL3 <- NFDATA_CL3[-train3,]
  
  TRAINSET <- rbind(trainCL1,trainCL2,trainCL3)
  x.TRAINSET <- TRAINSET[-(r95+1)]
  y.TRAINSET <- TRAINSET[,(r95+1)]
  TESTSET <- rbind(testCL1,testCL2,testCL3)
  x.TESTSET <- TESTSET[-(r95+1)]
  y.TESTSET <- TESTSET[,(r95+1)]
  
  result<- list('TRAINSET'=TRAINSET, 'x.TRAINSET'=x.TRAINSET, 
                'y.TRAINSET'=y.TRAINSET, 'TESTSET'=TESTSET, 
                'x.TESTSET'=x.TESTSET, 'y.TESTSET'=y.TESTSET)
  return(result)
}

TRAIN_TEST<- random_sets(NFDATA)
newTRAIN_TEST<- random_sets(NFDATA)

# Set sizes and class distribution
nrow(TRAIN_TEST$TRAINSET)
nrow(TRAIN_TEST$TESTSET)
nrow(newTRAIN_TEST$TRAINSET)
nrow(newTRAIN_TEST$TESTSET)
table(TRAIN_TEST$y.TRAINSET)
table(TRAIN_TEST$y.TESTSET)
table(newTRAIN_TEST$y.TRAINSET)
table(newTRAIN_TEST$y.TESTSET)


######################### Part 2 ###################################
#install.packages('randomForest')
require(randomForest)
set.seed(1)
rf.train<- randomForest(class~., data=TRAIN_TEST$TRAINSET, 
                        ntree=100, mtry=sqrt(r95), importance=TRUE)
train_conf<- rf.train$confusion[,-4]
train_conf 
rf.train
rf.test<- predict(rf.train,TRAIN_TEST$x.TESTSET)
test_conf<- table('TRUE'=TRAIN_TEST$y.TESTSET, 'PREDICTION'=rf.test)
test_conf


# New Training/Test Set
set.seed(1)
rf.new_train<- randomForest(class~., data=newTRAIN_TEST$TRAINSET, 
                            ntree=100, mtry=sqrt(r95), importance=TRUE)
new_train_conf<- rf.new_train$confusion[,-4]
#new_train_conf

rf.new_test<- predict(rf.new_train,newTRAIN_TEST$x.TESTSET)
new_test_conf<- table('TRUE'=newTRAIN_TEST$y.TESTSET, 'PREDICTION'=rf.new_test)
#new_test_conf


# function for confusion matrix in percentage
conf_percentage<- function(conf_name){
  confmat<- as.matrix(conf_name, rownames=TRUE, colnames=TRUE)
  conf_percent<- (prop.table(confmat,1))*100
  round(conf_percent,2)     
}

#Confusion Matrices
conf_percentage(train_conf)
conf_percentage(test_conf)
conf_percentage(new_train_conf)
conf_percentage(new_test_conf)

# function for Global accuracies
accuracy<- function(x){
  sum(diag(x)/(sum(rowSums(x))))*100
}

# Global Accuracy 
accuracy(train_conf)  
accuracy(test_conf)
accuracy(new_train_conf)
accuracy(new_test_conf)

#### 3.2 ####
set.seed(1)
RF_func<- function(ntree){
  train_RF<- randomForest(class~., data=TRAIN_TEST$TRAINSET, 
                          ntree=ntree, mtry=sqrt(r95), importance=TRUE)
  test_RF<- predict(train_RF,TRAIN_TEST$x.TESTSET)
  test_conf<- table('TRUE'=TRAIN_TEST$y.TESTSET, 'PREDICTION'=test_RF)
  return(test_conf)
}

# Confusion matrices in number of cases
set.seed(1)
RF10<- RF_func(10)
set.seed(1)
RF50<- RF_func(50)
set.seed(1)
RF100<- RF_func(100)
set.seed(1)
RF200<- RF_func(200)
set.seed(1)
RF300<- RF_func(300)
set.seed(1)
RF400<- RF_func(400)
set.seed(1)
RF500<- RF_func(500)
set.seed(1)
RF750<- RF_func(750)
set.seed(1)
RF1000<- RF_func(1000)

set.seed(1)
RF1<- RF_func(1)

# Confusion matrices in percentage
conf1<- conf_percentage(RF10); conf1
conf2<- conf_percentage(RF50); conf2
conf3<- conf_percentage(RF100); conf3
conf4<- conf_percentage(RF200); conf4
conf5<- conf_percentage(RF300); conf5
conf6<- conf_percentage(RF400); conf6
conf0<- conf_percentage(RF1);conf0
conf7<- conf_percentage(RF500)
conf8<- conf_percentage(RF750)
conf9<- conf_percentage(RF1000)
#Gloabal accuracies
a0= accuracy(RF1)
a1= accuracy(RF10)
a2= accuracy(RF50)
a3= accuracy(RF100)
a4= accuracy(RF200)
a5= accuracy(RF300)
a6= accuracy(RF400)
a7= accuracy(RF500)
a8= accuracy(RF750)
a9= accuracy(RF1000)


ntrees<- c(10,50,100,200,300,400)#,500,750,1000)
acc<- c(a1,a2,a3,a4,a5,a6)#,a7,a8,a9)

# Accuracy vs. ntrees plot
plot(ntrees,acc,ylim=c(65,85),type='b', col='red',
     main='Accuracy vs. ntrees', ylab='accuracy')

conf_C1<- c(conf1[1,1],conf2[1,1],conf3[1,1],conf4[1,1],conf5[1,1],conf6[1,1]) 
            #,conf7[1,1],conf8[1,1],conf9[1,1])
conf_C2<- c(conf1[2,2],conf2[2,2],conf3[2,2],conf4[2,2],conf5[2,2],conf6[2,2])
            #,conf7[2,2],conf8[2,2],conf9[2,2])
conf_C3<- c(conf1[3,3],conf3[3,3],conf3[3,3],conf4[3,3],conf5[3,3],conf6[3,3])
            #,conf7[3,3],conf8[3,3],conf9[3,3])

# 3 diagonal coefficients of confusion matrices (Each Class accuracy) vs. ntrees plot
plot(ntrees,conf_C1,type='b', ylim=range(c(conf_C1,conf_C2,conf_C3)),
     col='blue', xlab='Number of trees', ylab='Class Accuracy', 
     main='Class Accuracy vs. Number of trees')
par(new=TRUE)
plot(ntrees,conf_C2, type='b', ylim=range(c(conf_C1,conf_C2,conf_C3)),
     col='red',xlab="",ylab="")
par(new=TRUE)
plot(ntrees,conf_C3, type='b', ylim=range(c(conf_C1,conf_C2,conf_C3)),
     col='darkgreen',xlab="",ylab="")
legend("bottomright", legend=c("C1","C2","C3"),
       col=c("blue","red","darkgreen"),lwd=1, inset=c(0.02,0.03))

################### Part 3 ###################
max(acc) # Highest accuracy.
#bntr <- ntrees[which.max(acc)]; bntr # The best ntrees.
bntr=300

NFDATA_num<- NFDATA[,-110]
CORR<- cor(NFDATA_num)
L<- (eigen(CORR))$values #eigenvalues
head(L)
set.seed(1)
bestRF<- randomForest(class~., data=TRAIN_TEST$TRAINSET, 
                      ntree=bntr, mtry=sqrt(r95), importance=TRUE)
bestRF.test<- predict(bestRF,TRAIN_TEST$x.TESTSET)
bestRF.test_conf<- table('TRUE'=TRAIN_TEST$y.TESTSET,'PREDICTION'=bestRF.test)
bestRF_conf<- conf_percentage(bestRF.test_conf)  #Confusion matrix of bestRF


imp<- as.data.frame(importance(bestRF))
imp_eigen<- data.frame(imp$MeanDecreaseGini,imp$MeanDecreaseAccuracy, L)
head(imp_eigen)
varImpPlot(bestRF) #shows that X1 is the most important feature followed by X3
plot(imp_eigen$L, imp_eigen$imp.MeanDecreaseGini, 
     xlab='Eigenvalues (L)', ylab='Importance by Mean Decrease Gini',
     main="Importance of Features (PC's)", col='red', pch=3)
plot(imp_eigen$L, imp_eigen$imp.MeanDecreaseAccuracy, 
     xlab='Eigenvalues (L)', ylab='Importance by Mean Decrease Accuracy',
     main="Importance of Features (PC's)", col='blue', pch=4)

# Mean Decrease Accuracy - How much the model accuracy decreases if we drop that variable.
# Mean Decrease Gini - Measure of variable importance based on the Gini impurity index 
# used for the calculation of splits in trees.
# Higher the value of mean decrease accuracy or mean decrease gini score , 
# higher the importance of the feature in the model. 

###### Part 4 ###############
set.seed(1)
newRF<-randomForest(class~., data=newTRAIN_TEST$TRAINSET, 
                    ntree=bntr, mtry=sqrt(r95), importance=TRUE)
newRF.test<- predict(newRF,newTRAIN_TEST$x.TESTSET)
newRF.test_conf<- table('TRUE'=newTRAIN_TEST$y.TESTSET, 'PREDICTION'=newRF.test)
newRF_conf<- conf_percentage(newRF.test_conf)  #Confusion matrix of newRF

accuracy(bestRF.test_conf)
accuracy(newRF.test_conf)
bestRF_conf
newRF_conf

#---------Part 5 -----------#
# Confidence Intervals
#90% Confidence interval of each diagonal term individually- bestRF
cv= 1.6  #change cv to 1.96 for 95%CI
bestRF_conf
rowSums(bestRF_conf)   #row sums of bestRF testconf matrix

Ntest.CL1<- sum(bestRF_conf[1,])   #sum of test CL1 values
Ntest.CL2<- sum(bestRF_conf[2,])    #sum of test CL2 values
Ntest.CL3<- sum(bestRF_conf[3,])    #sum of test CL3 values
ptest.CL1<- bestRF_conf[1,1]/Ntest.CL1
ptest.CL2<- bestRF_conf[2,2]/Ntest.CL2
ptest.CL3<- bestRF_conf[3,3]/Ntest.CL3

#CI for CL1 diagonal term
sigma_test.CL1<- sqrt(ptest.CL1*(1-ptest.CL1)/Ntest.CL1) 
lower_test.CL1<- ptest.CL1-(cv*sigma_test.CL1); lower_test.CL1
upper_test.CL1<- ptest.CL1 + (cv*sigma_test.CL1); upper_test.CL1

#CI for CL2 diagonal term
sigma_test.CL2<- sqrt(ptest.CL2*(1-ptest.CL2)/Ntest.CL2) 
lower_test.CL2<- ptest.CL2-(cv*sigma_test.CL2); lower_test.CL2
upper_test.CL2<- ptest.CL2 + (cv*sigma_test.CL2); upper_test.CL2

#CI for CL3 diagonal term
sigma_test.CL3<- sqrt(ptest.CL3*(1-ptest.CL3)/Ntest.CL3) 
lower_test.CL3<- ptest.CL3-(cv*sigma_test.CL3); lower_test.CL3
upper_test.CL3<- ptest.CL3 + (cv*sigma_test.CL3); upper_test.CL3

#------------------------------------------------------------------------
#90% Confidence interval of each diagonal term individually- newRF
newRF_conf
rowSums(newRF_conf)   #row sums of newRF testconf matrix
Ntest.CL1_new<- sum(newRF_conf[1,])   #sum of test CL1 values
Ntest.CL2_new<- sum(newRF_conf[2,])    #sum of test CL2 values
Ntest.CL3_new<- sum(newRF_conf[3,])    #sum of test CL3 values
ptest.CL1_new<- newRF_conf[1,1]/Ntest.CL1_new
ptest.CL2_new<- newRF_conf[2,2]/Ntest.CL2_new
ptest.CL3_new<- newRF_conf[3,3]/Ntest.CL3_new

#CI for CL1 diagonal term
sigma_test.CL1_new<- sqrt(ptest.CL1_new*(1-ptest.CL1_new)/Ntest.CL1_new) 
lower_test.CL1_new<- ptest.CL1_new-(cv*sigma_test.CL1_new); lower_test.CL1_new
upper_test.CL1_new<- ptest.CL1_new + (cv*sigma_test.CL1_new); upper_test.CL1_new

#CI for CL2 diagonal term
sigma_test.CL2_new<- sqrt(ptest.CL2_new*(1-ptest.CL2_new)/Ntest.CL2_new) 
lower_test.CL2_new<- ptest.CL2_new-(cv*sigma_test.CL2_new); lower_test.CL2_new
upper_test.CL2_new<- ptest.CL2_new + (cv*sigma_test.CL2_new); upper_test.CL2_new

#CI for CL3 diagonal term
sigma_test.CL3_new<- sqrt(ptest.CL3_new*(1-ptest.CL3_new)/Ntest.CL3_new) 
lower_test.CL3_new<- ptest.CL3_new-(cv*sigma_test.CL3_new); lower_test.CL3_new
upper_test.CL3_new<- ptest.CL3_new + (cv*sigma_test.CL3_new); upper_test.CL3_new

# BEST RF: C1- 83% to 93%, C2- 70% to 83%, C3- 79% to 90%
# NEW RF: C1- 84% to 94%, C2- 73% to 85%, C3-80% to 91%

# Overlap among both CI and sharing same range indicates no significant variation 
# between bestRF and newRF. The model split is stable.
#--------------------------------------------------------------------------------
#### Part 6 ####
NFDATA2_TRAIN = TRAIN_TEST$TRAINSET
NFDATA2_TEST = TRAIN_TEST$TESTSET

NFDATA2_TRAIN$class<- as.character(NFDATA2_TRAIN$class)
NFDATA2_TEST$class<- as.character(NFDATA2_TEST$class)

NFDATA2_TRAIN$class[NFDATA2_TRAIN$class=='C1']<- 1
NFDATA2_TEST$class[NFDATA2_TEST$class=='C1']<- 1

NFDATA2_TRAIN$class[NFDATA2_TRAIN$class=='C2']<- 2
NFDATA2_TEST$class[NFDATA2_TEST$class=='C2']<- 2

NFDATA2_TRAIN$class[NFDATA2_TRAIN$class=='C3']<- 3
NFDATA2_TEST$class[NFDATA2_TEST$class=='C3']<- 3

RF_sets<- function(x,y,z){
  
  RFdata_train<- NFDATA2_TRAIN
  RFdata_test<- NFDATA2_TEST
  RFdata_train$class<- as.character(RFdata_train$class)
  RFdata_test$class<- as.character(RFdata_test$class)
  
  RFdata_train$class[RFdata_train$class==x]<- 'C1'
  RFdata_test$class[RFdata_test$class==x]<- 'C1'
  RFtrain_clone<- filter(RFdata_train, class=='C1')
  RFtest_clone<- filter(RFdata_test, class=='C1')
 
  RF_subTRAIN<- rbind(RFtrain_clone, RFtrain_clone)
  RF_subTEST<- rbind(RFtest_clone, RFtest_clone)
  
  RFdata_train$class[RFdata_train$class==y| RFdata_train$class==z]<-'C23'
  RFdata_test$class[RFdata_test$class==y| RFdata_test$class==z]<-'C23'
  RF_subTRAIN2<- filter(RFdata_train, class=='C23')
  RF_subTEST2<- filter(RFdata_test, class=='C23')
  
  RF_TRAIN<- rbind(RF_subTRAIN, RF_subTRAIN2)
  RF_TEST<- rbind(RF_subTEST,RF_subTEST2)
  
  RF_TRAIN$class<- as.factor(RF_TRAIN$class)
  RF_TEST$class<- as.factor(RF_TEST$class)
  x.RFTRAINSET <- RF_TRAIN[-(r95+1)]
  y.RFTRAINSET <- as.factor(RF_TRAIN[,(r95+1)])
  x.RFTESTSET <- RF_TEST[-(r95+1)]
  y.RFTESTSET <- as.factor(RF_TEST[,(r95+1)])
  
  result<- list('TRAINSET'=RF_TRAIN, 'x.TRAINSET'=x.RFTRAINSET, 
                'y.TRAINSET'=y.RFTRAINSET, 'TESTSET'=RF_TEST, 
                'x.TESTSET'=x.RFTESTSET, 'y.TESTSET'=y.RFTESTSET)
  return(result)
}
RF1<- RF_sets(1,2,3)
RF2<- RF_sets(2,1,3)
RF3<- RF_sets(3,1,2)

# RF sizes & distribution
table(RF1$y.TRAINSET)
table(RF1$y.TESTSET)
table(RF2$y.TRAINSET)
table(RF2$y.TESTSET)
table(RF3$y.TRAINSET)
table(RF3$y.TESTSET)
table(TRAIN_TEST$y.TRAINSET)
table(TRAIN_TEST$y.TESTSET)

# RF1 Random Forest Classifier (C1 vs. {C2+C3})
set.seed(1)
RF1.rf<-randomForest(class~., data=RF1$TRAINSET, 
                     ntree=bntr, mtry=sqrt(r95), importance=TRUE)
RF1.test<- predict(RF1.rf,RF1$x.TESTSET)
RF1test_conf<- table('TRUE'=RF1$y.TESTSET, 'PREDICTION'=RF1.test)
RF1test_conf
M1<- conf_percentage(RF1test_conf)
A1<- accuracy(RF1test_conf)

# RF2 Random Forest Classifier (C2 vs. {C1+C2})
set.seed(1)
RF2.rf<-randomForest(class~., data=RF2$TRAINSET, 
                     ntree=bntr, mtry=sqrt(r95), importance=TRUE)
RF2.test<- predict(RF2.rf,RF2$x.TESTSET)
RF2test_conf<- table('TRUE'=RF2$y.TESTSET, 'PREDICTION'=RF2.test)
RF2test_conf
M2<- conf_percentage(RF2test_conf)
A2<- accuracy(RF2test_conf)

# RF3 Random Forest Classifier (C3 vs. {C1+C2})

set.seed(1)
RF3.rf<-randomForest(class~., data=RF3$TRAINSET, 
                     ntree=bntr, mtry=sqrt(r95), importance=TRUE)
RF3.test<- predict(RF3.rf,RF3$x.TESTSET)
RF3test_conf<- table('TRUE'=RF3$y.TESTSET, 'PREDICTION'=RF3.test)
RF3test_conf
M3<- conf_percentage(RF3test_conf)
A3<- accuracy(RF3test_conf)

a6
conf6
# Accuracies of RF Classifiers
A1
A2
A3

# Confusion Matrices of RF Classifiers
M1
M2
M3

################ Part 7  ###########################
NFDATA3_TRAIN= TRAIN_TEST$TRAINSET
NFDATA3_TEST= TRAIN_TEST$TESTSET
NFDATA3_TEST$Vn= bestRF.test

# C1 vs. {C2+C3}
v1<- ifelse(NFDATA3_TEST$Vn=='C1','C1',NA)
v23<- ifelse(NFDATA3_TEST$Vn=='C2'|NFDATA3_TEST$Vn=='C3','C23',NA)
NFDATA3_TEST$V1<- v1
NFDATA3_TEST$V23<- v23

NFDATA3_TEST$V1.true<- NFDATA3_TEST$class
NFDATA3_TEST$V1.true<- as.character(NFDATA3_TEST$V1.true)
NFDATA3_TEST$V1.true[NFDATA3_TEST$V1.true=='C2'|NFDATA3_TEST$V1.true=='C3']<- 'C23'
NFDATA3_TEST$V1.true<- as.factor(NFDATA3_TEST$V1.true)
NFDATA3_TEST$V1.Pred<- coalesce(NFDATA3_TEST$V1,NFDATA3_TEST$V23)


# C2 vs. {C1+C3}
v2<- ifelse(NFDATA3_TEST$Vn=='C2','C2',NA)
v13<- ifelse(NFDATA3_TEST$Vn=='C1'|NFDATA3_TEST$Vn=='C3','C13',NA)
NFDATA3_TEST$V2<- v2
NFDATA3_TEST$V13<- v13

NFDATA3_TEST$V2.true<- NFDATA3_TEST$class
NFDATA3_TEST$V2.true<- as.character(NFDATA3_TEST$V2.true)
NFDATA3_TEST$V2.true[NFDATA3_TEST$V2.true=='C1'|NFDATA3_TEST$V2.true=='C3']<- 'C13'
NFDATA3_TEST$V2.true<- as.factor(NFDATA3_TEST$V2.true)
NFDATA3_TEST$V2.Pred<- coalesce(NFDATA3_TEST$V2,NFDATA3_TEST$V13)

# C3 vs. {C1+C2}
v3<- ifelse(NFDATA3_TEST$Vn=='C3','C3',NA)
v12<- ifelse(NFDATA3_TEST$Vn=='C1'|NFDATA3_TEST$Vn=='C2','C12',NA)
NFDATA3_TEST$V3<- v3
NFDATA3_TEST$V12<- v12

NFDATA3_TEST$V3.true<- NFDATA3_TEST$class
NFDATA3_TEST$V3.true<- as.character(NFDATA3_TEST$V3.true)
NFDATA3_TEST$V3.true[NFDATA3_TEST$V3.true=='C1'| NFDATA3_TEST$V3.true=='C2']<- 'C12'
NFDATA3_TEST$V3.true<- as.factor(NFDATA3_TEST$V3.true)
NFDATA3_TEST$V3.Pred<- coalesce(NFDATA3_TEST$V3,NFDATA3_TEST$V12)

table(NFDATA2_TEST$V1.true)

C1_vs_C23<- table('TRUE'=NFDATA3_TEST$V1.true,'PREDICTION'=NFDATA3_TEST$V1.Pred)
BM1<- conf_percentage(C1_vs_C23)
C2_vs_C13<- table('TRUE'=NFDATA3_TEST$V2.true,'PREDICTION'=NFDATA3_TEST$V2.Pred)
BM2<- conf_percentage(C2_vs_C13)
C3_vs_C12<- table('TRUE'=NFDATA3_TEST$V3.true,'PREDICTION'=NFDATA3_TEST$V3.Pred)
BM3<- conf_percentage(C3_vs_C12)

# Accuracies with bestRF classifier
B1<- accuracy(C1_vs_C23); B1
B2<- accuracy(C2_vs_C13); B2
B3<- accuracy(C3_vs_C12); B3

# Confusion Matrices with bestRF classifier
BM1
BM2
BM3
# Accuracy improved for each C1, C2, C3 and decreased for C12,C23,C13








