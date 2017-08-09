## Get the data 
library(readxl)
library(rpart)
library(rpart.plot)
creditCardData <- read_excel("c://Users/mjaviya/Desktop/ccc.xls")
## check for missing values
summary(creditCardData)
sum(is.na.data.frame(creditCardData))

normalizeRange <- function(values) {
  (values-min(values))/(max(values)-min(values))
}

## Assign proper names to column
colnames(creditCardData) <- c("Id", "Limit", "Gender", "EducationStatus", "MarritalStatus", "Age", "SeptemberStatus",
                              "AugustStatus","JulyStatus", "JuneStatus", "MayStatus", "AprilStatus", "SeptemberBill",
                              "AugustBill","JulyBill", "JuneBill", "MayBill", "AprilBill", "SeptemberPaid",
                              "AugustPaid","JulyPaid", "JunePaid", "MayPaid", "AprilPaid", "DefaultPayment")


## Code column values properly to make them redable
creditCardData$Gender <- factor(creditCardData$Gender, levels = c(1, 2), labels = c("Male", "Female"))
creditCardData$EducationStatus <- factor(creditCardData$EducationStatus, levels = c(1, 2, 3, 4), labels = c("Graduate School", "University", "High School", "Others"))
creditCardData$MarritalStatus <- factor(creditCardData$MarritalStatus, levels = c(1, 2, 3), labels = c("Married", "Single", "Other"))

creditCardData$SeptemberStatus <- as.numeric(as.character(creditCardData$SeptemberStatus))
creditCardData$SeptemberStatus <-as.factor(ifelse(creditCardData$SeptemberStatus<=0,'Pay Duly',
                                   ifelse(creditCardData$SeptemberStatus==1,'Delay 1 Month',
                                   ifelse(creditCardData$SeptemberStatus==2,'Delay 2 Month',
                                   ifelse(creditCardData$SeptemberStatus==3,'Delay 3 Month',
                                   ifelse(creditCardData$SeptemberStatus==4,'Delay 4 Month',
                                          ifelse(creditCardData$SeptemberStatus==5,'Delay 5 Month','Delay 6+ Month')))))))


creditCardData$AugustStatus <- as.numeric(as.character(creditCardData$AugustStatus))
creditCardData$AugustStatus <-as.factor(ifelse(creditCardData$AugustStatus<=0,'Pay Duly',
                                                  ifelse(creditCardData$AugustStatus==1,'Delay 1 Month',
                                                         ifelse(creditCardData$AugustStatus==2,'Delay 2 Month',
                                                                ifelse(creditCardData$AugustStatus==3,'Delay 3 Month',
                                                                       ifelse(creditCardData$AugustStatus==4,'Delay 4 Month',
                                                                              ifelse(creditCardData$AugustStatus==5,'Delay 5 Month','Delay 6+ Month')))))))


creditCardData$JulyStatus <- as.numeric(as.character(creditCardData$JulyStatus))
creditCardData$JulyStatus <-as.factor(ifelse(creditCardData$JulyStatus<=0,'Pay Duly',
                                                  ifelse(creditCardData$JulyStatus==1,'Delay 1 Month',
                                                         ifelse(creditCardData$JulyStatus==2,'Delay 2 Month',
                                                                ifelse(creditCardData$JulyStatus==3,'Delay 3 Month',
                                                                       ifelse(creditCardData$JulyStatus==4,'Delay 4 Month',
                                                                              ifelse(creditCardData$JulyStatus==5,'Delay 5 Month','Delay 6+ Month')))))))


creditCardData$JuneStatus <- as.numeric(as.character(creditCardData$JuneStatus))
creditCardData$JuneStatus <-as.factor(ifelse(creditCardData$JuneStatus<=0,'Pay Duly',
                                                  ifelse(creditCardData$JuneStatus==1,'Delay 1 Month',
                                                         ifelse(creditCardData$JuneStatus==2,'Delay 2 Month',
                                                                ifelse(creditCardData$JuneStatus==3,'Delay 3 Month',
                                                                       ifelse(creditCardData$JuneStatus==4,'Delay 4 Month',
                                                                              ifelse(creditCardData$JuneStatus==5,'Delay 5 Month','Delay 6+ Month')))))))


creditCardData$MayStatus <- as.numeric(as.character(creditCardData$MayStatus))
creditCardData$MayStatus <-as.factor(ifelse(creditCardData$MayStatus<=0,'Pay Duly',
                                                  ifelse(creditCardData$MayStatus==1,'Delay 1 Month',
                                                         ifelse(creditCardData$MayStatus==2,'Delay 2 Month',
                                                                ifelse(creditCardData$MayStatus==3,'Delay 3 Month',
                                                                       ifelse(creditCardData$MayStatus==4,'Delay 4 Month',
                                                                              ifelse(creditCardData$MayStatus==5,'Delay 5 Month','Delay 6+ Month')))))))


creditCardData$AprilStatus <- as.numeric(as.character(creditCardData$AprilStatus))
creditCardData$AprilStatus <-as.factor(ifelse(creditCardData$AprilStatus<=0,'Pay Duly',
                                                  ifelse(creditCardData$AprilStatus==1,'Delay 1 Month',
                                                         ifelse(creditCardData$AprilStatus==2,'Delay 2 Month',
                                                                ifelse(creditCardData$AprilStatus==3,'Delay 3 Month',
                                                                       ifelse(creditCardData$AprilStatus==4,'Delay 4 Month',
                                                                              ifelse(creditCardData$AprilStatus==5,'Delay 5 Month','Delay 6+ Month')))))))



creditCardData$DefaultPayment <- factor(creditCardData$DefaultPayment, levels = c(1,0), labels = c("Yes", "No"))

## Look at the summary of data
str(creditCardData)

## Check the distribution
sum(creditCardData$DefaultPayment == "Yes")
sum(creditCardData$DefaultPayment == "No")

##Visualization

## Histogram age
hist(creditCardData$Age)

## bar chart education
plot(creditCardData$EducationStatus)



## correlation plot
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(~creditCardData$Limit + creditCardData$SeptemberStatus+ creditCardData$AugustStatus + creditCardData$JuneStatus + creditCardData$DefaultPayment, lower.panel=panel.smooth, upper.panel=panel.cor)

## Create derived variables
creditCardData$SeptemberRem <- creditCardData$SeptemberBill - creditCardData$SeptemberPaid
creditCardData$AugustRem <- creditCardData$AugustBill - creditCardData$AugustPaid
creditCardData$JulyRem <- creditCardData$JulyBill - creditCardData$JulyPaid
creditCardData$JuneRem <- creditCardData$JuneBill - creditCardData$JunePaid
creditCardData$MayRem <- creditCardData$MayBill - creditCardData$MayPaid
creditCardData$AprilRem <- creditCardData$AprilBill - creditCardData$AprilPaid
creditCardData$TotalRem <- creditCardData$SeptemberRem + creditCardData$AugustRem + creditCardData$JulyRem + creditCardData$JuneRem + creditCardData$MayRem + creditCardData$AprilRem
creditCardData$RemToLimit <- (creditCardData$TotalRem / creditCardData$Limit) * 100

## Remove outliers based on Limit
x <- creditCardData$Limit
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
creditCardData <- creditCardData[!(creditCardData$Limit < (qnt[1]-H)) & !(creditCardData$Limit > (qnt[2]+H)), ]

## Visualize outliers based on remToLimit and cap the remToLimit accordingly
boxplot(creditCardData$RemToLimit ~ creditCardData$DefaultPayment, main="Box plot to find outliers")
x <- creditCardData$RemToLimit
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
creditCardData$RemToLimitAdjusted <- x


## Distribution by gender
noOfMale <- sum(creditCardData$Gender=="Male")
noOfFemale <- sum(creditCardData$Gender=="Female")
noOfMaleDefault <- sum(creditCardData$Gender=="Male" & creditCardData$DefaultPayment=="Yes")
noOfFemaleDefault <- sum(creditCardData$Gender=="Female" & creditCardData$DefaultPayment=="Yes")
pctOfMaleDefault <- (noOfMaleDefault * 100)/noOfMale
pctOfFemaleDefault <- (noOfFemaleDefault * 100)/noOfFemale



## Normalize Bill and limit variables
creditCardData$Limit <- normalizeRange(creditCardData$Limit)
creditCardData$SeptemberRem <- normalizeRange(creditCardData$SeptemberRem)
creditCardData$AugustRem <- normalizeRange(creditCardData$AugustRem)
creditCardData$JulyRem <- normalizeRange(creditCardData$JulyRem)
creditCardData$JuneRem <- normalizeRange(creditCardData$JuneRem)
creditCardData$MayRem <- normalizeRange(creditCardData$MayRem)
creditCardData$AprilRem <- normalizeRange(creditCardData$AprilRem)
creditCardData$RemToLimit <- normalizeRange(creditCardData$RemToLimit)


## Model 1
## Split data into 2 parts by ratio of 7:3
temp <- sort(sample(nrow(creditCardData), nrow(creditCardData)*0.7))

## create training and test sets using above created samples
trainingSet <- creditCardData[temp,]
testSet <- creditCardData[-temp,]


## build logistic model. 
logisticModel1 <- glm(DefaultPayment ~ Gender + EducationStatus + MarritalStatus + Age + SeptemberStatus 
                    + AugustStatus + JulyStatus + JuneStatus + MayStatus + AprilStatus + SeptemberRem + AugustRem
                    + JulyRem + JuneRem + MayRem + AprilRem + RemToLimit, data=trainingSet, family="binomial")
summary(logisticModel1)

## Building revised model after removing non-significant variables from model 1
logisticModel2 <- glm(DefaultPayment ~ Gender + EducationStatus + MarritalStatus + SeptemberStatus 
                      + AugustStatus + JuneStatus + MayStatus + AprilStatus + RemToLimit, data=trainingSet, family="binomial")
summary(logisticModel2)

## Model 3
logisticModel3 <- glm(DefaultPayment ~ Gender + EducationStatus + MarritalStatus + SeptemberStatus 
                      + AugustStatus + MayStatus + AprilStatus, data=trainingSet, family="binomial")
summary(logisticModel3)

## Model 4
logisticModel4 <- glm(DefaultPayment ~ Gender + EducationStatus + MarritalStatus + SeptemberStatus 
                      + AugustStatus + MayStatus + AprilStatus, data=trainingSet, family="binomial")
summary(logisticModel4)

## We got all the effect as significant now. So our model seems to fit the data
## Lets try to predict the probability score for testSet
testSet$probabilityScore <- predict(logisticModel4, newdata = testSet, type = "response")

## calculating top 3 variables affecting result
top3<-predict(logisticModel4,type='terms',testSet)

findtopReasons <- function(x,top=3){
  res=names(x)[order(x, decreasing = TRUE)][1:top]
  paste(res,collapse=";",sep="")
}

top3Reasons=apply(top3,1,findtopReasons,top=3)
testSet<-cbind(testSet, top3Reasons)

## Accuracy Measure
correctNoPrediction <- sum(testSet$DefaultPayment == "No" & testSet$probabilityScore>0.5, na.rm = TRUE) #6488
wrongNoPrediction <- sum(testSet$DefaultPayment == "No" & testSet$probabilityScore<=0.5, na.rm = TRUE) #357

wrongYesPrediction <- sum(testSet$DefaultPayment == "Yes" & testSet$probabilityScore>0.5, na.rm = TRUE) #1234
correctYesPrediction <- sum(testSet$DefaultPayment == "Yes" & testSet$probabilityScore<=0.5, na.rm = TRUE) #738

totalCorrectPrediction <- correctYesPrediction + correctNoPrediction
totalWrongPrediction <- wrongYesPrediction + wrongNoPrediction
dim(testSet)[1] #8950
Accuracy <- totalCorrectPrediction / dim(testSet)[1] 
Accuracy*100 #80.73743






## Decision Tree
tree1<-rpart(DefaultPayment ~ Gender + EducationStatus + MarritalStatus + Age + SeptemberStatus 
             + AugustStatus + JulyStatus + JuneStatus + MayStatus + AprilStatus + SeptemberRem + AugustRem
             + JulyRem + JuneRem + MayRem + AprilRem + RemToLimit,data=trainingSet, method = "class")
prp(tree1);
testSet$tscore1<-predict(tree1, newdata = testSet, type="class")

## Accuracy Measure
correctNoPrediction <- sum(testSet$DefaultPayment == "No" & testSet$tscore1=="No", na.rm = TRUE) #6680
wrongNoPrediction <- sum(testSet$DefaultPayment == "No" & testSet$tscore1=="Yes", na.rm = TRUE) #287

wrongYesPrediction <- sum(testSet$DefaultPayment == "Yes" & testSet$tscore1=="No", na.rm = TRUE) #1338
correctYesPrediction <- sum(testSet$DefaultPayment == "Yes" & testSet$tscore1=="Yes", na.rm = TRUE) #645

totalCorrectPrediction <- correctYesPrediction + correctNoPrediction
totalWrongPrediction <- wrongYesPrediction + wrongNoPrediction
dim(testSet)[1] #8950
Accuracy <- totalCorrectPrediction / dim(testSet)[1] 
Accuracy*100 #81.84358

## checkout cp from tree1
printcp(tree1)

tree2<-rpart(DefaultPayment ~ Gender + EducationStatus + MarritalStatus + Age + SeptemberStatus 
             + AugustStatus + JulyStatus + JuneStatus + MayStatus + AprilStatus + SeptemberRem + AugustRem
             + JulyRem + JuneRem + MayRem + AprilRem + RemToLimit,data=trainingSet, method = "class",cp=0.001)
prp(tree2)
testSet$tscore2<-predict(tree2, newdata = testSet, type='class')

## Accuracy Measure for tree 2
CNPred <- sum(testSet$DefaultPayment == "No" & testSet$tscore2=="No", na.rm = TRUE) #6610
WNPred <- sum(testSet$DefaultPayment == "No" & testSet$tscore2=="Yes", na.rm = TRUE) #357

WYPred <- sum(testSet$DefaultPayment == "Yes" & testSet$tscore2=="No", na.rm = TRUE) #1268
CYPred <- sum(testSet$DefaultPayment == "Yes" & testSet$tscore2=="Yes", na.rm = TRUE) #715

totalCorrectPrediction <- CYPred + CNPred
totalWrongPrediction <- WYPred + WNPred
dim(testSet)[1] #8950
Accuracy <- totalCorrectPrediction / dim(testSet)[1] 
Accuracy*100 # 81.84358

## The result is similar as tree1









## Accuracy is roughly around 81% which is very good. But if we look at the predictions, it predicted No's correctly
## as our dataset has about 70% No values. But the predictions for "Yes" was bad. Infact very bad. More than half
## the prediction for "Yes" were wrong. We can improve this if we have more values in our dataset with "Yes" to 
## train our model properly. Also, this might have something to do with encoding of variables that i followed for status
## feature. I even tried different encoding but result was similar. Different encoding is as below just for reference:

creditCardData$SeptemberStatus <- factor(creditCardData$SeptemberStatus, levels = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                         labels = c("Pay Advance", "Pay Duly", "Delay 0 Month", "Delay 1 Month", "Delay 2 Months", "Delay 3 Months",
                                                    "Delay 4 Months", "Delay 5 Months", "Delay 6 Months",
                                                    "Delay 7 Months", "Delay 8 Months", "Delay 9 or more Months"))

creditCardData$AugustStatus <- factor(creditCardData$AugustStatus, levels = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                      labels = c("Pay Advance","Pay Duly", "Delay 0 Month", "Delay 1 Month", "Delay 2 Months", "Delay 3 Months",
                                                 "Delay 4 Months", "Delay 5 Months", "Delay 6 Months",
                                                 "Delay 7 Months", "Delay 8 Months", "Delay 9 or more Months"))

creditCardData$JulyStatus <- factor(creditCardData$JulyStatus, levels = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                    labels = c("Pay Advance","Pay Duly", "Delay 0 Month", "Delay 1 Month", "Delay 2 Months", "Delay 3 Months",
                                               "Delay 4 Months", "Delay 5 Months", "Delay 6 Months",
                                               "Delay 7 Months", "Delay 8 Months", "Delay 9 or more Months"))

creditCardData$JuneStatus <- factor(creditCardData$JuneStatus, levels = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                    labels = c("Pay Advance","Pay Duly", "Delay 0 Month", "Delay 1 Month", "Delay 2 Months", "Delay 3 Months",
                                               "Delay 4 Months", "Delay 5 Months", "Delay 6 Months",
                                               "Delay 7 Months", "Delay 8 Months", "Delay 9 or more Months"))

creditCardData$MayStatus <- factor(creditCardData$MayStatus, levels = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                   labels = c("Pay Advance","Pay Duly", "Delay 0 Month", "Delay 1 Month", "Delay 2 Months", "Delay 3 Months",
                                              "Delay 4 Months", "Delay 5 Months", "Delay 6 Months",
                                              "Delay 7 Months", "Delay 8 Months", "Delay 9 or more Months"))

creditCardData$AprilStatus <- factor(creditCardData$AprilStatus, levels = c(-2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                     labels = c("Pay Advance","Pay Duly", "Delay 0 Month", "Delay 1 Month", "Delay 2 Months", "Delay 3 Months",
                                                "Delay 4 Months", "Delay 5 Months", "Delay 6 Months",
                                                "Delay 7 Months", "Delay 8 Months", "Delay 9 or more Months"))



