autodata<- read.csv("C:\\Users\\deepp\\Google Drive\\MSDS\\MATH 6350 Data Mining\\cleanDataAuto.csv",
                    header=T,na.strings="?")
names(autodata)
head(autodata)
dim(autodata)
autodata<-na.omit(autodata)  #verifies no row contains misssing numerical data
dim(autodata)         #gives total number of rows & columns
nrow(autodata)        #gives total number of N cases= number of numerical rows

## mpg= miles per gallon will be target variable
## the 5 explanatory variables or features F1, F2, F3, F4, F5 given by columns 2 3 4 5 6
# we will denote them F1= cyl, F2= dis, F3=hor, F4=wei, F5=acc

Y = mpg <- autodata$mpg        #Target/Response variable
F1 = cyl <- autodata$cylinders
F2 = dis <- autodata$displacement
F3 = hor <- autodata$horsepower
F4 = wei <- autodata$weight
F5 = acc <- autodata$acceleration

#_------------------------------------------------------------------------------------
###### Question 1 mean & sd ######
# mean and standard deviation of cylinders
mean(F1)
sd(F1)

#mean and standard deviation of displacement
mean(F2)
sd(F2)

#mean and standard deviation of horsepower
mean(F3)
sd(F3)

#mean and standard deviation of weight
mean(F4)
sd(F4)

#mean and standard deviation of acceleration
mean(F5)
sd(F5)

#-------------------------------------------------------------------------------------------------
dev.off()
###### Question 2 Histograms #######
par(mfrow = c(3, 2))
hist(F1, breaks = 10,
     main = "Histogram of cylinders",
     xlab = "cyl (F1)",
     col = "light blue")   #histogram of cylinders
hist(F2, breaks = 10,
     main = "Histogram of displacement",
     xlab = "dis (F2)",
     col = "orange")   #histogram of displacement
hist(F3, breaks = 10,
     main = "Histogram of horsepower",
     xlab = "hor (F3)",
     col = "light yellow")   #histogram of horsepower
hist(F4, breaks = 10,
          main = "Histogram of weight",
          xlab = "wei (F4)",
          col = "purple")  #histogram of weight
hist(F5, breaks = 10,
     main = "Histogram of acceleration",
     xlab = "acc (F5)",
     col = "light pink")   #histogram of acceleration

hist(mpg, breaks = 10,
     main = "Miles per gallon (mpg)",
     xlab = "mpg",
     col = "light green")  #histogram of miles per gallon

#NOTE: reprint hist F4 & F5 - error in labels
#probability density function- trying to draw curve over histogram (refer to pnormplot in 6358 Quiz 1?)
#x1<- dnorm(F1, mean = 5.47, sd=1.706)
#plot(F1,x1)
#lines(F1,x1)

#-----------------------------------------------------------------------------------------

######## Question #3 & 4 Scatterplots ########
#Scatterplot of mpg vs. cylinder
plot(cyl,mpg,  
     pch = 3,      # plus sign
     cex = 1,        # 100% size
     col = "blue",     # Blue color
     main = "mpg vs. cylinder",
     xlab = "cylinders (cyl)",
     ylab = "miles per gallon (mpg)")

#Scatterplot of mpg vs. displacement
plot(dis,mpg,  
     pch = 2,      # triangle
     cex = 1,        # 100% size
     col = "orange",     #Orange color
     main = "mpg vs. displacement",
     xlab = "displacement (dis)",
     ylab = "miles per gallon (mpg)")


#Scatterplot of mpg vs. horsepower
plot(hor,mpg,  
     pch = 1,      # circles
     cex = 1.2,        # 120% size
     col = "gold1",     #Golden yellow color
     main = "mpg vs. horsepower",
     xlab = "horsepower (hor)",
     ylab = "miles per gallon (mpg)")


#Scatterplot of mpg vs. weight
plot(wei,mpg,  
     pch = 4,      # x marks
     cex = 1.5,        # 150% size
     col = "purple",     #purple color
     main = "mpg vs. weight",
     xlab = "weight (wei)",
     ylab = "miles per gallon (mpg)")


#Scatterplot of mpg vs. acceleration
plot(acc,mpg,  
     pch = 5,      # diamonds
     cex = 1.5,        # 150% size
     col = "deeppink3",     #pink color
     main = "mpg vs. acceleration",
     xlab = "acceleration (acc)",
     ylab = "miles per gallon (mpg)")

#---------------------------------------------------------------------------------------------

#### Question #5 Correlations ####
cor(cyl,mpg)
cor(dis,mpg)
cor(hor,mpg)
cor(wei,mpg)
cor(acc,mpg)


#-------------------------------------------------------------------------------------------------

### Question #6 Correlation Matrix
install.packages("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
library(corrplot)
head(autodata)


features<- autodata[,c("cylinders","displacement","horsepower","weight","acceleration")]
head(features)
rquery.cormat(features, type="full")

#NOTE: execute commands, check console, not sure if correlation matrix is correct
#-----------------------------------------------------------------------------------------

###### Question #7: Quantile plot #########
percentile<- seq(0.01,1,by=0.01)
qmpg<- quantile(mpg, percentile)
qmpg
Y = mpg
n=length(Y)
plot((1:n-1)/(n-1),sort(Y),type="l",
     col="red",
     main="Quantile curve for miles per gallon (mpg)",
     xlab="Percentile x%",
     ylab="quantile value for mpg")

#-----------------------------------------------------------------------------------------

##### Question 8: Extracting Data ####
percentile<- seq(0.01,1,by=0.01)
qmpg<- quantile(mpg, percentile) #sorts mpg data from 1% to 100% percentile
qmpg.data<- data.frame(qmpg)  
head(qmpg.data)
dim(qmpg.data)

q33<- qmpg.data[33,]     #recalls value at 33% percentile
#as the mpg data is sorted 1% to 100%, 33rd entry is equivalent to 33%percentile
q66<- qmpg.data[66,]     #recalls value at 66% percentile

filter.q33<- autodata$mpg<=q33
filter.q33
LOWmpg<- autodata[filter.q33,]
head(LOWmpg)

filter.q66<- autodata$mpg>q66
HIGHmpg<- autodata[filter.q66,]
head(HIGHmpg)

#--------------------------------------------------------------------------------------

#### Question 9 & 10: Histogram of LOWmpg & HIGHmpg ##########

Y.low<- LOWmpg$mpg
F1low= cyl.low <- LOWmpg$cylinders
F2low= dis.low <- LOWmpg$displacement
F3low= hor.low <- LOWmpg$horsepower
F4low= wei.low <- LOWmpg$weight
F5low= acc.low <- LOWmpg$acceleration

Y.high<- HIGHmpg$mpg
F1high= cyl.high <- HIGHmpg$cylinders
F2high= dis.high <- HIGHmpg$displacement
F3high= hor.high <- HIGHmpg$horsepower
F4high= wei.high <- HIGHmpg$weight
F5high= acc.high <- HIGHmpg$acceleration

dev.off()
# Put graphs in 5 rows and 2 column for comparison
par(mfrow = c(5, 2))


hist(F1low, breaks = 10,
     main = "Histogram of cylinders with LOWmpg cases",
     xlab = "cyl",
     col = "light blue")   #LOWmpg cases histogram of cylinders
hist(F1high, breaks = 10,
     main = "Histogram of cylinders with HIGHmpg cases",
     xlab = "cyl",
     col = "light blue")   #HIGHmpg cases histogram of cylinders

hist(F2low, breaks = 10,
     main = "Histogram of displacement with LOWmpg cases",
     xlab = "dis",
     col = "orange")   #LOWmpg cases histogram of displacement
hist(F2high, breaks = 10,
     main = "Histogram of displacement with HIGHmpg cases",
     xlab = "dis",
     col = "orange")   #HIGHmpg cases histogram of displacement


hist(F3low, breaks = 10,
     main = "Histogram of horsepower with LOWmpg cases",
     xlab = "hor",
     col = "light yellow")   #LOWmpg cases histogram of horsepower
hist(F3high, breaks = 10,
     main = "Histogram of horsepower with HIGHmpg cases",
     xlab = "hor",
     col = "light yellow")   #HIGHmpg cases histogram of horsepower


hist(F4low, breaks = 10,
     main = "Histogram of weight with LOWmpg cases",
     xlab = "wei",
     col = "purple")  #LOWmpg cases histogram of weight
hist(F4high, breaks = 10,
     main = "Histogram of weight with HIGHmpg cases",
     xlab = "wei",
     col = "purple")  #HIGHmpg cases histogram of weight


hist(F5low, breaks = 10,
     main = "Histogram of acceleration with LOWmpg cases",
     xlab = "acc",
     col = "light pink")   #LOWmpg cases histogram of acceleration
hist(F5high, breaks = 10,
     main = "Histogram of acceleration with HIGHmpg cases",
     xlab = "acc",
     col = "light pink")   #HIGHmpg cases histogram of acceleration

#---------------------------------------------------------------------------------------

##### Question #12: mean mL, mH & standard dev stdL, stdH for LOW & HIGH mpg ####

# mean and standard deviation of cylinders with LOWmpg cases
mLF1<- mean(F1low); mLF1
stdL1<- sd(F1low); stdL1
# mean and standard deviation of cylinders with HIGHmpg cases
mHF1<- mean(F1high); mHF1
stdH1<- sd(F1high); stdH1


#mean and standard deviation of displacement with LOWmpg cases
mLF2<- mean(F2low); mLF2
stdL2<- sd(F2low); stdL2
# mean and standard deviation of displacement with HIGHmpg cases
mHF2<- mean(F2high); mHF2
stdH2<- sd(F2high); stdH2

#mean and standard deviation of horsepower with LOWmpg cases
mLF3<- mean(F3low); mLF3
stdL3<- sd(F3low); stdL3
# mean and standard deviation of horsepower with HIGHmpg cases
mHF3<- mean(F3high); mHF3
stdH3<- sd(F3high); stdH3

#mean and standard deviation of weight with LOWmpg cases
mLF4<- mean(F4low); mLF4
stdL4<- sd(F4low); stdL4
# mean and standard deviation of weight with HIGHmpg cases
mHF4<- mean(F4high); mHF4
stdH4<- sd(F4high); stdH4

#mean and standard deviation of acceleration with LOWmpg cases
mLF5<- mean(F5low); mLF5
stdL5<- sd(F5low); stdL5
# mean and standard deviation of acceleration with HIGHmpg cases
mHF5<- mean(F5high); mHF5
stdH5<- sd(F5high); stdH5


#-------------------------------------------------------------------------------------
# for quick calculations of mean & sd
install.packages("psych")
library(psych)
describe(LOWmpg)
describe(HIGHmpg)

#--------------------------------------------------------------------------------------
NL<-nrow(LOWmpg); NL                #number of cases in LOWmpg
NH<- nrow(HIGHmpg); NH              #number of cases in HIGHmpg
M<-(NL+NH)/2 ; M                    #Denote M = NL ??? NH . 
#Instructions: For each feature F compute   s(F) = square root [ (stdL2  + stdH2 )/M ]

####### Question 12 Formula ############
sF1<- sqrt(((stdL1)^2 + (stdH1)^2)/M)      
sF1
sF2<- sqrt(((stdL2)^2 + (stdH2)^2)/M)     
sF2
sF3<- sqrt(((stdL3)^2 + (stdH3)^2)/M)     
sF3
sF4<- sqrt(((stdL4)^2 + (stdH4)^2)/M)     
sF4
sF5<- sqrt(((stdL5)^2 + (stdH5)^2)/M)     
sF5

#Rough evaluation of discriminating power of features F to distinguish bewtween LOW mpg & HIGH mpg

discr.F1<- abs(mHF1 - mLF1)/sF1   #discriminating power of cylinders between LOWmpg & HIGH mpg cases
discr.F1

discr.F2<- abs(mHF2 - mLF2)/sF2   #discriminating power of displacement between LOWmpg & HIGH mpg cases
discr.F2

discr.F3<- abs(mHF3 - mLF3)/sF3   #discriminating power of horsepower between LOWmpg & HIGH mpg cases
discr.F3

discr.F4<- abs(mHF4 - mLF4)/sF4   #discriminating power of weight between LOWmpg & HIGH mpg cases
discr.F4

discr.F5<- abs(mHF5 - mLF5)/sF5   #discriminating power of acceleration between LOWmpg & HIGH mpg cases
discr.F5


#-------------------------------------------------------------------------------------------------

###### Question 13 thershold calculations ######

thr.F1<- ((mLF1*stdH1)+(mHF1*stdL1))/(stdH1+stdL1)  #threshold of cylinders
thr.F1

thr.F2<- ((mLF2*stdH2)+(mHF2*stdL2))/(stdH2+stdL2)  #threshold of displacement
thr.F2

thr.F3<- ((mLF3*stdH3)+(mHF3*stdL3))/(stdH3+stdL3)  #threshold of horsepower
thr.F3

thr.F4<- ((mLF4*stdH4)+(mHF4*stdL4))/(stdH4+stdL4)   #threshold of weight
thr.F4

thr.F5<- ((mLF5*stdH5)+(mHF5*stdL5))/(stdH5+stdL5)   #threshold of acceleration
thr.F5


condition1a<- c(mHF1>mLF1,mHF2>mLF2,mHF3>mLF3,mHF4>mLF4,mHF5>mLF5)
condition1a             #condition 1a is when mH>mL, score F(n)=1 when F(n)>thrF and F(n)=-1 when F(n)<=thrF
condition1b<- c(mHF1<mLF1,mHF2<mLF2,mHF3<mLF3,mHF4<mLF4,mHF5<mLF5)
condition1b             #condition 1b is when mH<mL, score F(n)=1 when F(n)<thrF and F(n)=-1 when F(n)>=thrF




f5score<- ifelse(autodata$acceleration>thr.F5, 1, -1)
f5score

f1score<- ifelse(autodata$cylinders<thr.F1,1,-1)
f1score

f2score<- ifelse(autodata$displacement<thr.F2,1,-1)
f2score

f3score<- ifelse(autodata$horsepower<thr.F3, 1, -1)
f3score

f4score<- ifelse(autodata$weight<thr.F4, 1, -1)
f4score
#-------------------------------------------------------------------------------------
#Question 14


fullscore<- f1score+f2score+f3score+f4score+f5score
fullscore

autodata2<- autodata
head(autodata2)
autodata2$fullscore<- fullscore    #created duplicate dataset & added column fullscore

autodata3<- autodata
autodata3$fullscore<- fullscore   #created duplicate dataset & added column fullscore for backup

library(dplyr) #to use coalesce function

#True class
mpg.median<-median(autodata2$mpg); mpg.median
filter.true_high<- ifelse(autodata2$mpg>=mpg.median,"HIGHmpg", NA)
filter.true_low<- ifelse(autodata2$mpg<mpg.median,"LOWmpg", NA)
autodata2$scorehigh_true<- filter.true_high
autodata2$scorelow_true<- filter.true_low
head(autodata2)
autodata2$class.true<- coalesce(autodata2$scorehigh_true, autodata2$scorelow_true)
head(autodata2,n=25)
head(autodata2,n=25)
autodata2$scorehigh_true<-NULL
autodata2$scorelow_true<-NULL
autodata2$class.pred<-NULL
head(autodata2,n=25)

qmpg.data
#Predicted class
qmpg.data<- data.frame(qmpg); qmpg.data
q33<- qmpg.data[33,]
q66<- qmpg.data[66,] 

autodata3<- autodata2
filter.q66_high<- ifelse(autodata3$mpg>q66,"HIGHmpg", NA)
filter.q33_low<- ifelse(autodata3$mpg<=q33,"LOWmpg", NA)
autodata3$TRAIN.Pred_high<- filter.q66_high
autodata3$TRAIN.Pred_low<- filter.q33_low
autodata3$class.Pred_Train<- coalesce(autodata3$TRAIN.Pred_high, autodata3$TRAIN.Pred_low)
autodata3$TRAIN.Pred_high<- NULL
autodata3$TRAIN.Pred_low<- NULL
head(autodata3,n=25) 


autodata4_Train<-autodata3[!is.na(autodata3$class.Pred_Train),] # dataset without NA cases <=q33 & >q66
autodata5_Test<- autodata3[is.na(autodata3$class.Pred_Train),] #dataset with NA cases between q33 & q66


autodata4_Train$class.Pred_Train<- NULL #to remove Highmpg/LOWmpg column based on quantiles
autodata5_Test$class.Pred_Train<- NULL #to remove NA column based on quantiles

head(autodata4_Train,n=25)
head(autodata5_Test)

#TRAIN DATA SET CLASSIFIER 
autodata4_Train$class.Pred_A<- NULL
A=0
filter.high<- ifelse(autodata4_Train$fullscore>=A, "HIGHmpg",NA)
filter.low<- ifelse(autodata4_Train$fullscore<A, "LOWmpg",NA)
library(dplyr)
autodata4_Train$Pred_train.high<- filter.high
autodata4_Train$Pred_train.low<- filter.low
autodata4_Train$class.Pred_A<- coalesce(autodata4_Train$Pred_train.high,autodata4_Train$Pred_train.low)
autodata4_Train$Pred_train.high<- NULL
autodata4_Train$Pred_train.low<- NULL
head(autodata4_Train)

#TEST DATA SET CLASSIFIER
autodata5_Test$class.Pred_A<- NULL    #execute before changing A value
A=2
filter.high<- ifelse(autodata5_Test$fullscore>=A, "HIGHmpg",NA)
filter.low<- ifelse(autodata5_Test$fullscore<A, "LOWmpg",NA)

autodata5_Test$Pred_train.high<- filter.high
autodata5_Test$Pred_train.low<- filter.low
autodata5_Test$class.Pred_A<- coalesce(autodata5_Test$Pred_train.high,autodata5_Test$Pred_train.low)
autodata5_Test$Pred_train.high<- NULL
autodata5_Test$Pred_train.low<- NULL
head(autodata5_Test)


#confusion matrix


library(caret)

#Confusion matrix for TRAIN Data set
confusion.matrix_TRAIN<- with(autodata4_Train,table("True"=class.true/(nrow(autodata4_Train)), "Prediction"=class.Pred_A/(nrow(autodata4_Train)))
confusion.matrix_TRAIN 
prop.table(confusion.matrix_TRAIN)*100
#Confusion matrix for TRAIN Data set when A=2
sum(confusion.matrix_TRAIN)

#Confusion matrix for TEST Data set
confusion.matrix_TEST<- with(autodata5_Test,table("True"=class.true/nrow(), "Prediction"=class.Pred_A))
confusion.matrix_TEST
#Confusion matrix for TEST Data set when A=2
sum(confusion.matrix_TEST)

#-------------------------------------------------------------------------------------------------------------------------------

# #handwritten code for confusion matrix
# qll<-0
# qlh<-0
# qhl<-0
# qhh<-0
# 
# for (i in 1:length(autodata4_Train$mpg)){
#         if ((autodata4_Train$class.true[i]=="LOWmpg") & (autodata4_Train$class.Pred_A[i]=="LOWmpg")){
#                 qll<-qll+1
#         }
#         if ((autodata4_Train$class.true[i]=="LOWmpg") & (autodata4_Train$class.Pred_A[i]=="HIGHmpg")){
#                 qlh<-qlh+1
#         }
#         if ((autodata4_Train$class.true[i]=="HIGHmpg") & (autodata4_Train$class.Pred_A[i]=="LOWmpg")){
#                 qhl<-qhl+1
#         }
#         if ((autodata4_Train$class.true[i]=="HIGHmpg") & (autodata4_Train$class.Pred_A[i]=="HIGHmpg")){
#                 qhh<- qhh+1
#         }
# }
# 
# confusion.matrixA1<- matrix(c(qll,qhl,qlh,qhh),nrow=2,ncol =2,
#                           dimnames = list(c("True LOW", "True HIGH"), c("Pred LOW", "Pred HIGH")))
# confusion.matrixA1
# sum(confusion.matrixA1)

