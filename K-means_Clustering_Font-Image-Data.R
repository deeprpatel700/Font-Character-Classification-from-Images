setwd("C:\\Users\\deepp\\Google Drive\\Selected fonts")
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

#names(CL1)[4:403]<- paste("X", seq(1, 400), sep="")
#names(CL2)[4:403]<- paste("X", seq(1, 400), sep="")
#names(CL3)[4:403]<- paste("X", seq(1, 400), sep="")

# Full data set
DATA <- rbind(CL1,CL2,CL3)


# Mean and SD of the 400 columns (features)
DATA_features <- DATA[4:403]
DATA_mean_std <- sapply(DATA_features, 
                        function(DATA_features) c( "Stand dev" = sd(DATA_features,na.rm=TRUE), 
                                                                  "Mean"= mean(DATA_features,
                                                                               na.rm=TRUE)))


# Creating "SDATA" - standardized data set / Rescaled data matrix
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


# Correlation matrix of 400 features
CORR <- round(cor(SDATA),3)
#abs_CORR <- abs(CORR)
head(CORR[,1:6])
write.csv(CORR, file="HW3_Hora_Patel_Neopaney_Cor_matrix.csv")
#write.csv(abs_CORR, file="HW3_Hora_Patel_Neopaney_abs_Cor_matrix.csv")

ev<- eigen(CORR)
L<- ev$values; head(L)
W<- ev$vectors; head(round((W[,1:6]),3))

rownames(W)<- paste("X", seq(1, 400), sep="")
colnames(W)<- paste("V", seq(1, 400), sep="")
head(round((W[,1:6]),3))

#library(psych)
#tr(CORR)     #checking sum of trace of CORR matrix

write.csv(L, file="eigenvalues.csv")
write.csv(W, file="eigenvectors.csv")

# plot(1:400, L, type='l', main="Eigenvalues(Lr) vs. r",
#      xlab="r", ylab="eigenvalues(L)", col="red")


ev_data<- data.frame(L)
ev_data$new<- cumsum(L)
head(ev_data)
PVE = round((ev_data$new)/sum(L),4)
head(PVE)
ev_data$PVE<- PVE
ev_data$r<- 1:400

plot(PVE*100, xlab="r", 
     ylab="PVE(%)",
     main="Percentage of Variance Explained vs. r",
     ylim=c(0,100),type='l',col='blue')
r95 = which(ev_data$PVE >= .95)[1]   #directly finds features with 95% variance
r95

coeff_newY<- t(W); head(coeff_newY, n=2)  #matrix of coefficients of new features


new_features<- t(W) %*% t(SDATA)   
new_features = t(new_features)
NFDATA = as.data.frame(new_features) # Ken - created dataframe version of new features
dim(NFDATA)
#names(NFDATA)[1:400]<- paste("Y(", seq(1, 400), sep="",")")
head(round((NFDATA[,1:6]),3))
write.csv(new_features, file="new_features_Yn.csv")

dim(new_features)

###############################################################################
n1 = nrow(CL1)
n2 = nrow(CL2)
n3 = nrow(CL3)

# This is the new feature data with only the principal components
NFDATA = NFDATA[1:r95]

# Adding class to NFDATA
NFDATA$class = 'CL1'
NFDATA$class[(1+n1):(n1+n2)] = 'CL2'
NFDATA$class[(1+n1+n2):(n1+n2+n3)] = 'CL3'


NFDATA_CL1 <- filter(NFDATA, class == 'CL1')
train1 <- sample(1:n1, 0.8*n1)
trainCL1 <- NFDATA_CL1[train1,]
testCL1 <- NFDATA_CL1[-train1,]

NFDATA_CL2 <- filter(NFDATA, class == 'CL2')
train2 <- sample(1:n2, 0.8*n2)
trainCL2 <- NFDATA_CL2[train2,]
testCL2 <- NFDATA_CL2[-train2,]

NFDATA_CL3 <- filter(NFDATA, class == 'CL3')
train3 <- sample(1:n3, 0.8*n3)
trainCL3 <- NFDATA_CL3[train3,]
testCL3 <- NFDATA_CL3[-train3,]


TRAINSET <- rbind(trainCL1,trainCL2,trainCL3)
x.TRAINSET <- TRAINSET[-(r95+1)]
y.TRAINSET <- TRAINSET[,(r95+1)]
TESTSET <- rbind(testCL1,testCL2,testCL3)
x.TESTSET <- TESTSET[-(r95+1)]
y.TESTSET <- TESTSET[,(r95+1)]

accuracy<- function(x){
  sum(diag(x)/(sum(rowSums(x))))*100
}


k_values <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 25, 30, 35, 
              40, 45, 50)
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


plot(k_values, trainperfk, ylim=range(c(trainperfk,testperfk)),type='l',col='cyan',
     xlab = "K values", ylab = "Accuracy", main = 'Testing & Training Accuracy, Pre/Post PCA')
par(new = TRUE)
plot(k_values, testperfk, ylim=range(c(trainperfk,testperfk)),type='l',
     axes = FALSE, xlab = "", ylab = "", col='tomato')

which.max(testperfk) # The index of best K.
#kbest <- k_values[which.max(testperfk)]; kbest # The best K value.
kbest=1
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
testconf    #Test confusion matrix in number of cases

confmat.test<- as.matrix(testconf, rownames=TRUE, colnames=TRUE)
testconf_percent<- scale(confmat.test, center=FALSE, scale=rowSums(confmat.test))*100
round(testconf_percent,2)       #Testing confusion matrix in percentage

N2<- as.numeric(sum(testconf)); N2
diag_test_sum<-sum(diag(testconf))    #N of cases correctly classified
sum(testconf)-sum(diag(testconf))  #N of cases incorrectly classified
p2 <- (diag_test_sum/N2)   
testconf_acc<- p2*100 #global accuracy of test confusion matrix
testconf_acc



############Scatterplots####################################################################

p3DATA = NFDATA[1:3]
par(mfrow=c(3,2))
plot1<- plot(p3DATA$V1[1:n1], p3DATA$V2[1:n1], col="blue", 
             ylab = "Y(2)", xlab = "Y(1)", main="Y1 vs. Y2")
points(p3DATA$V1[(1+n1):(n1+n2)], p3DATA$V2[(1+n1):(n1+n2)], col="red")
points(p3DATA$V1[(1+n1+n2):(n1+n2+n3)], p3DATA$V2[(1+n1+n2):(n1+n2+n3)], col="green")
legend("topright",legend=c("CL1","CL2","CL3"),col=c("blue","red","green"),inset =.02,
       fill=,cex=0.8)
nrow(p3DATA)
plot(p3DATA$V1[1:n1], p3DATA$V3[1:n1], col="blue", ylab = "Y(3)", xlab = "Y(1)")
points(p3DATA$V1[(1+n1):(n1+n2)], p3DATA$V3[(1+n1):(n1+n2)], col="red")
points(p3DATA$V1[(1+n1+n2):(n1+n2+n3)], p3DATA$V3[(1+n1+n2):(n1+n2+n3)], col="green")

plot(p3DATA$V2[1:n1], p3DATA$V3[1:n1], col="blue", ylab = "Y(3)", xlab = "Y(2)")
points(p3DATA$V2[(1+n1):(n1+n2)], p3DATA$V3[(1+n1):(n1+n2)], col="red")
points(p3DATA$V2[(1+n1+n2):(n1+n2+n3)], p3DATA$V3[(1+n1+n2):(n1+n2+n3)], col="green")


p3DATA2 = NFDATA[1:3]
p3DATA2$class = NFDATA$class

library(lattice)
cloud(V3 ~ V1 + V2, p3DATA2[1:n1,], col = "blue", scales = list(arrows = FALSE), 
      main = "3D plot")
cloud(V3 ~ V1 + V2, p3DATA2[(1+n1):(n1+n2),], col ="red", scales = list(arrows = FALSE), 
      main = "3D plot")
cloud(V3 ~ V1 + V2, p3DATA2[(1+n1+n2):(n1+n2+n3),], col = "green", scales = list(arrows = FALSE), 
      main = "3D plot")


###############Scatterplots using ggplot######################################################################
CL1_SelectYn<- NFDATA_CL1[,c(1:4,110)]
CL2_SelectYn<- NFDATA_CL2[,c(1:4,110)]
CL3_SelectYn<- NFDATA_CL3[,c(1:4,110)]

CL1_Y1<- data.frame((CL1_SelectYn[,c(1,5)]))
CL1_Y2<- data.frame((CL1_SelectYn[,c(2,5)]))
CL1_Y3<- data.frame((CL1_SelectYn[,c(3,5)]))
CL1_Y4<- data.frame((CL1_SelectYn[,c(4,5)]))

CL2_Y1<- data.frame((CL2_SelectYn[,c(1,5)]))
CL2_Y2<- data.frame((CL2_SelectYn[,c(2,5)]))
CL2_Y3<- data.frame((CL2_SelectYn[,c(3,5)]))
CL2_Y4<- data.frame((CL2_SelectYn[,c(4,5)]))

CL3_Y1<- data.frame((CL3_SelectYn[,c(1,5)]))
CL3_Y2<- data.frame((CL3_SelectYn[,c(2,5)]))
CL3_Y3<- data.frame((CL3_SelectYn[,c(3,5)]))
CL3_Y4<- data.frame((CL3_SelectYn[,c(4,5)]))

Y1_combined<- rbind(CL1_Y1,CL2_Y1,CL3_Y1)
Y2_combined<- rbind(CL1_Y2,CL2_Y2,CL3_Y2)
Y3_combined<- rbind(CL1_Y3,CL2_Y3,CL3_Y3)
Y4_combined<- rbind(CL1_Y4,CL2_Y4,CL3_Y4)
Y_COMB<- cbind(Y1_combined[,1],Y2_combined[,1],Y3_combined[,1],Y4_combined)
rownames(Y_COMB)<-c()
colnames(Y_COMB)<-c("Y1","Y2","Y3","Y4","Class")
tail(Y_COMB)


install.packages("ggthemes")
library(ggthemes)

require("ggplot2")
#qplot(data=Y_COMB, Class)
Plot1<- qplot(Y1,Y2, data=Y_COMB, color=Class, 
              #ylim=range(c(Y_COMB$Y1,Y_COMB$Y2)),
              xlim=range(c(Y_COMB$Y1,Y_COMB$Y2)),
              main="Y(1) vs. Y(2)",alpha=I(0.4),size=I(1.9)); Plot1
p1<- Plot1+theme_stata(); p1+theme(legend.position=c(0.92,0.86))

Plot2<- qplot(Y1,Y3, data=Y_COMB, color=Class, 
              #ylim=range(c(Y_COMB$Y1,Y_COMB$Y3)),
              xlim=range(c(Y_COMB$Y1,Y_COMB$Y3)),
              main="Y(1) vs. Y(3)", alpha=I(0.5),size=I(1.9)); Plot2
p2<- Plot2+theme_stata(); p2+theme(legend.position=c(0.92,0.86))

Plot3<- qplot(Y1,Y4, data=Y_COMB, color=Class, 
              ylim=range(c(Y_COMB$Y1,Y_COMB$Y4)),
              xlim=range(c(Y_COMB$Y1,Y_COMB$Y4)),
              main="Y(1) vs. Y(4)", alpha=I(0.5),size=I(1.9)); Plot3
p3<- Plot3+theme_stata(); p3

Plot4<- qplot(Y2,Y3, data=Y_COMB, color=Class, 
              #ylim=range(c(Y_COMB$Y2,Y_COMB$Y3)),
              xlim=range(c(Y_COMB$Y2,Y_COMB$Y3)),
              main="Y(2) vs. Y(3)", alpha=I(0.5),size=I(1.9)); Plot4
p4<- Plot4+theme_stata(); p4+theme(legend.position=c(0.92,0.86))

Plot5<- qplot(Y2,Y4, data=Y_COMB, color=Class, 
              ylim=range(c(Y_COMB$Y2,Y_COMB$Y4)),
              xlim=range(c(Y_COMB$Y2,Y_COMB$Y4)),
              main="Y(2) vs. Y(4)", alpha=I(0.5),size=I(1.9)); Plot5
p5<- Plot5+theme_stata(); p5+theme(legend.position=c(0.1,0.86))

Plot6<- qplot(Y3,Y4, data=Y_COMB, color=Class, 
              ylim=range(c(Y_COMB$Y3,Y_COMB$Y4)),
              xlim=range(c(Y_COMB$Y3,Y_COMB$Y4)),
              main="Y(3) vs. Y(4)", alpha=I(0.4),size=I(1.9)); Plot6
p6<- Plot6+theme_stata(); p6+theme(legend.position=c(0.1,0.86))

install.packages("gridExtra")
library(gridExtra)
grid.arrange(Plot1,Plot2,Plot3,Plot4,Plot5,Plot6,ncol=2) #without theme
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2) #with theme

grid.arrange(p1,p2,ncol=2)
