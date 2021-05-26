# Practical Machine Learning Coursera Assignment


#### **Background**

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

#### **Data**

The training data for this project are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv  
The test data are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

#### **What you should submit**

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

#### Required Packages

```r
library(caret)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
```
#### Getting and Loading the Data

```r
# save url path
urlTrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
# check if file pml-training.csv already exist in home dir, download if not
if (!file.exists("./pml-training.csv")) {
      download.file(urlTrain, destfile = "./pml-training.csv")
}
# check if file pml-testing.csv already exist in home dir, download if not
if (!file.exists("./pml-testing.csv")) {
      download.file(urlTest, destfile = "./pml-testing.csv")
}
# load data into R
trainDF <- read.csv("./pml-training.csv")
testDF <- read.csv("./pml-testing.csv")
```


```r
# set seed for reproducibility
set.seed(885)
```
#### Cleaning the data

```r
# check dimensions of data sets
dim(trainDF)
```

```
## [1] 19622   160
```

```r
dim(testDF)
```

```
## [1]  20 160
```

```r
# remove Near Zero Variance variables from `train` data and `test` data
nzv <- nearZeroVar(trainDF, saveMetrics = T)
trainDFclean <- trainDF[, !nzv$nzv] 
testDFclean <- testDF[, !nzv$nzv]
dim(trainDFclean)
```

```
## [1] 19622   100
```

```r
dim(testDFclean)
```

```
## [1]  20 100
```

```r
# remove columns that contain NA's
nas <- (colSums(is.na(trainDFclean)) == 0)
trainDFclean <- trainDFclean[, nas]
testDFclean <- testDFclean[, nas]
dim(trainDFclean)
```

```
## [1] 19622    59
```

```r
dim(testDFclean)
```

```
## [1] 20 59
```


```r
# remove 1 to 5 columns from train data (we don't need user info)
trainDFclean <- trainDFclean[ , -c(1:5)]
# remove 1 to 5 columns from test data (we don't need user info)
testDFclean <- testDFclean[ , -c(1:5)]
dim(trainDFclean)
```

```
## [1] 19622    54
```

```r
dim(testDFclean)
```

```
## [1] 20 54
```

#### Data Partitioning and Prediction Model Building

```r
# Split the clean training data to 70% training and 30% testing set
inTrain <- createDataPartition(trainDFclean$classe, p = 0.70, list = F)
training <- trainDFclean[inTrain, ]
testing <- trainDFclean[-inTrain, ]
```

**- Random Forest Model**



```r
# build Random Forest model
modelRF <- randomForest(as.factor(classe) ~ ., data = training)
# make prediction on testing dataset
predictRF <- predict(modelRF, testing, type = "class")
# build the confusion matrix to see the accuracy of the model
confusionMatrix(predictRF, testing$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    0    0    0    0
##          B    0 1139    3    0    0
##          C    0    0 1023    7    0
##          D    0    0    0  956    2
##          E    0    0    0    1 1080
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9978          
##                  95% CI : (0.9962, 0.9988)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9972          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   0.9971   0.9917   0.9982
## Specificity            1.0000   0.9994   0.9986   0.9996   0.9998
## Pos Pred Value         1.0000   0.9974   0.9932   0.9979   0.9991
## Neg Pred Value         1.0000   1.0000   0.9994   0.9984   0.9996
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2845   0.1935   0.1738   0.1624   0.1835
## Detection Prevalence   0.2845   0.1941   0.1750   0.1628   0.1837
## Balanced Accuracy      1.0000   0.9997   0.9978   0.9956   0.9990
```

*out-of-sample error: 100 - 99.78 = 0.22%*

**- Decision Tree**


```r
# build Decision Tree model
modelDT <- rpart(as.factor(classe) ~ ., data = training, method = "class")
# plot the model
fancyRpartPlot(modelDT,sub = "")
```

```
## Warning: labs do not fit even at cex 0.15, there may be some overplotting
```

![](FinalAssignmentML_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
# make prediction on testing dataset
predictDT <- predict(modelDT, testing, type = "class")
# build the confusion matrix to see the accuracy of the model
confusionMatrix(predictDT, testing$classe) 
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1453   85    1    7    5
##          B  116  876   70   78   58
##          C    1   53  848   39    5
##          D   90   51   98  759   65
##          E   14   74    9   81  949
## 
## Overall Statistics
##                                           
##                Accuracy : 0.8301          
##                  95% CI : (0.8202, 0.8396)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.7857          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8680   0.7691   0.8265   0.7873   0.8771
## Specificity            0.9767   0.9322   0.9798   0.9382   0.9629
## Pos Pred Value         0.9368   0.7312   0.8964   0.7140   0.8421
## Neg Pred Value         0.9490   0.9439   0.9640   0.9575   0.9720
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2469   0.1489   0.1441   0.1290   0.1613
## Detection Prevalence   0.2636   0.2036   0.1607   0.1806   0.1915
## Balanced Accuracy      0.9224   0.8506   0.9032   0.8628   0.9200
```

*out-of-sample error: 100 - 83.01 = 16.99%*

#### **Conclusion**
We choose Random Forest Model for our prediction because his Accuracy is 0.9978 and Decision Tree's Accuracy is only 0.8301.

```r
# predict the `class` of the tesdDFclean data
predict(modelRF, newdata = testDFclean)
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
