#God is Holy

library(psych)
library(RColorBrewer)

#Loading the Data
diab = read.csv("C:/Users/tonyk/Documents/DSP 34/Task/LogisticRegression/diabetes.csv",
                stringsAsFactors = TRUE)

#Dimensions of the Data
dim(diab)
'768   9'

#Names of the Variables
colnames(diab)
'
[1] "Pregnancies"              "Glucose"                 
[3] "BloodPressure"            "SkinThickness"           
[5] "Insulin"                  "BMI"                     
[7] "DiabetesPedigreeFunction" "Age"                     
[9] "Outcome" '

#Viewing the first 5 observations of the data
head(diab)

#Checking the missing values
sapply(diab, function(x) sum(is.na(x)))
'No missing values'

#Target Variable - Outcome
str(diab$Outcome)
'int [1:768] 1 0 1 0 1 0 1 0 1 1 ...'

#Converting to factor
diab$Outcome = as.factor(diab$Outcome)

str(diab$Outcome)
'Factor w/ 2 levels "0","1": 2 1 2 1 2 1 2 2 2 1 ...'

out_tab = table(diab$Outcome)
print(out_tab)
'  0   1 
 500 268'

#Barplot
barplot(out_tab,
        col = c('violet', 'wheat3'),
        main = 'Barplot of Person Having Diabetes or Not')

#___________________________Pregnancies
str(diab$Pregnancies)
'int [1:768] 6 1 8 1 0 5 3 10 2 8 ...'

summary(diab$Pregnancies)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.000   1.000   3.000   3.845   6.000  17.000 '

#Histogram
hist(diab$Pregnancies,
     col = brewer.pal(9, 'Set1'),
     xlim = c(0,20),
     xlab = 'Number of times pregnant',
     main = 'Histogram of Pregnancies')

#Boxplot
boxplot(diab$Pregnancies,
        horizontal = TRUE,
        col = 'orchid',
        main = 'Boxplot of Pregnancies')

preg_ub = quantile(diab$Pregnancies, 0.75)+1.5*IQR(diab$Pregnancies)
print(preg_ub)
' 75% 
 13.5'

length(diab$Pregnancies[diab$Pregnancies > preg_ub])
'4'
#Ignoring the outliers

#Outcome vs Pregnancies
plot(diab$Pregnancies, diab$Outcome,
     xlab = 'Pregnancies',
     ylab = 'Diabetes Outcome',
     main = 'Diabetes Outcome vs Pregnancies',
     col = c('darkgreen','brown4','blue4','darkred'))

#Dividing the data into person having diabetes and not
diab_no = diab[diab$Outcome == 0,]
dim(diab_no)
'500   9'

diab_yes = diab[diab$Outcome == 1,]
dim(diab_yes)
'268   9'

#Dividing the plot area as 1 rows 2 columns
par(mfrow = c(1,2))

#Histogram
hist(diab_no$Pregnancies,
     col = brewer.pal(8, 'Accent'),
     xlim = c(0,20),
     xlab = 'Number of times pregnant',
     main = 'Without Diabetes')

hist(diab_yes$Pregnancies,
     col = brewer.pal(8, 'Dark2'),
     xlim = c(0,20),
     xlab = 'Number of times pregnant',
     main = 'With Diabetes')

#Making plot area as 1 row as 1 column
par(mfrow = c(1,1)) 

#___________________________Glucose
str(diab$Glucose)
'int [1:768] 148 85 183 89 137 116 78 115 197 125 ...'

summary(diab$Glucose)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.0    99.0   117.0   120.9   140.2   199.0 '

#Histogram
hist(diab$Glucose,
     col = brewer.pal(9, 'Set1'),
     xlab = 'Glucose',
     main = 'Histogram of Glucose')

#Boxplot
boxplot(diab$Glucose,
        horizontal = TRUE,
        col = 'maroon4',
        main = 'Boxplot of Glucose')

"Glucose can't be 0 so removing it"

#Removing the observation having glucose value 0
length(diab$Glucose[diab$Glucose == 0])
'5'

#Removing outliers
diab = diab[diab$Glucose != 0,]

dim(diab)
' 763   9'

#Boxplot
boxplot(diab$Glucose,
        horizontal = TRUE,
        col = 'maroon4',
        main = 'Boxplot of Glucose')

#Outcome vs Glucose
plot(diab$Glucose, diab$Outcome,
     xlab = 'Glucose',
     ylab = 'Diabetes Outcome',
     main = 'Diabetes Outcome vs Glucose',
     col = c('darkgreen','brown4','blue4','darkred'))

#Dividing the data into person having diabetes and not
diab_no = diab[diab$Outcome == 0,]
dim(diab_no)
'497   9'

diab_yes = diab[diab$Outcome == 1,]
dim(diab_yes)
'266   9'

#Dividing the plot area as 1 rows 2 columns
par(mfrow = c(1,2))

#Histogram
hist(diab_no$Glucose,
     col = brewer.pal(8, 'Accent'),
     xlab = 'Glucose',
     main = 'Without Diabetes')

hist(diab_yes$Glucose,
     col = brewer.pal(8, 'Dark2'),
     xlab = 'Glucose',
     main = 'With Diabetes')

#Making plot area as 1 row as 1 column
par(mfrow = c(1,1)) 

#___________________________BloodPressure
str(diab$BloodPressure)
'int [1:763] 72 66 64 66 40 74 50 0 70 96 ...'

summary(diab$BloodPressure)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.00   62.00   72.00   69.12   80.00  122.00 '

#Histogram
hist(diab$BloodPressure,
     col = brewer.pal(9, 'Set1'),
     xlab = 'BloodPressure',
     main = 'Histogram of BloodPressure')

#Boxplot
boxplot(diab$BloodPressure,
        horizontal = TRUE,
        col = 'goldenrod4',
        main = 'Boxplot of BloodPressure')

"BloodPressure can't be 0, so removing 0 and keeping the other values"

#Checking the  no of observation with Blood Pressure 0
length(diab$BloodPressure[diab$BloodPressure == 0])
'35'

#Removing Blood Pressure with value zero
diab = diab[diab$BloodPressure != 0, ]

dim(diab)
' 728   9'

#Boxplot
boxplot(diab$BloodPressure,
        horizontal = TRUE,
        col = 'orangered2',
        main = 'Boxplot of BloodPressure')

#Outcome vs BloodPressure
plot(diab$BloodPressure, diab$Outcome,
     xlab = 'BloodPressure',
     ylab = 'Diabetes Outcome',
     main = 'Diabetes Outcome vs Glucose',
     col = c('orangered3','orchid4','palegreen4','purple4'))

#Dividing the data into person having diabetes and not
diab_no = diab[diab$Outcome == 0,]
dim(diab_no)
'478   9'

diab_yes = diab[diab$Outcome == 1,]
dim(diab_yes)
'250   9'

#Dividing the plot area as 1 rows 2 columns
par(mfrow = c(1,2))

#Histogram
hist(diab_no$BloodPressure,
     col = brewer.pal(8, 'Accent'),
     xlab = 'BloodPressure',
     main = 'Without Diabetes')

hist(diab_yes$BloodPressure,
     col = brewer.pal(8, 'Dark2'),
     xlab = 'BloodPressure',
     main = 'With Diabetes')

#Making plot area as 1 row as 1 column
par(mfrow = c(1,1)) 

#___________________________SkinThickness
str(diab$SkinThickness)
'int [1:728] 35 29 0 23 35 0 32 45 0 0 ...'

summary(diab$SkinThickness)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00   24.00   21.39   33.00   99.00 '

#Histogram
hist(diab$SkinThickness,
     col = brewer.pal(9, 'Set1'),
     xlab = 'SkinThickness',
     main = 'Histogram of SkinThickness')

#Boxplot
boxplot(diab$SkinThickness,
        horizontal = TRUE,
        col = 'tan3',
        main = 'Boxplot of SkinThickness')

194/728

#Checking the  no of outliers
st_ub = quantile(diab$SkinThickness, 0.75)+1.5*IQR(diab$SkinThickness)
print(st_ub)
' 75% 
82.5'

#Checking the No of outliers
length(diab$SkinThickness[diab$SkinThickness > st_ub])

#Assigning the oultier value with upper boundary value
diab$SkinThickness[diab$SkinThickness > st_ub] = st_ub

#Boxplot
boxplot(diab$SkinThickness,
        horizontal = TRUE,
        col = 'yellow4',
        main = 'Boxplot of SkinThickness')

#Outcome vs SkinThickness
plot(diab$SkinThickness, diab$Outcome,
     xlab = 'SkinThickness',
     ylab = 'Diabetes Outcome',
     main = 'Diabetes Outcome vs SkinThickness',
     col = c('yellow4','tan4','navy','green4','firebrick4'))

#Dividing the plot area as 1 rows 2 columns
par(mfrow = c(1,2))

#Histogram
hist(diab_no$SkinThickness,
     col = brewer.pal(8, 'Accent'),
     xlab = 'SkinThickness',
     main = 'Without Diabetes')

hist(diab_yes$SkinThickness,
     col = brewer.pal(8, 'Dark2'),
     xlab = 'SkinThickness',
     main = 'With Diabetes')

#Making plot area as 1 row as 1 column
par(mfrow = c(1,1)) 

#___________________________Insulin 
str(diab$Insulin)
'int [1:728] 0 0 0 94 168 0 88 543 0 0 ...'

summary(diab$Insulin)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00   48.00   84.15  130.00  846.00  '

#Histogram
hist(diab$Insulin,
     col = brewer.pal(9, 'Set1'),
     xlab = 'Insulin',
     main = 'Histogram of Insulin')

#Boxplot
boxplot(diab$Insulin,
        horizontal = TRUE,
        col = 'seagreen',
        main = 'Boxplot of Insulin')

#Checking the  no of outliers
ins_ub = quantile(diab$Insulin, 0.75)+1.5*IQR(diab$Insulin)
print(ins_ub)
'75% 
 325'

#Checking the No of outliers
length(diab$Insulin[diab$Insulin > ins_ub])
'30'

for (i in seq(ins_ub,max(diab$Insulin),50)){
  j = length(diab$Insulin[diab$Insulin > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}

'
[1] "No of outliers with ub as 325 is 30"
[1] "No of outliers with ub as 375 is 22"
[1] "No of outliers with ub as 425 is 18"
[1] "No of outliers with ub as 475 is 15"
[1] "No of outliers with ub as 525 is 8"
[1] "No of outliers with ub as 575 is 5"
[1] "No of outliers with ub as 625 is 3"
[1] "No of outliers with ub as 675 is 3"
[1] "No of outliers with ub as 725 is 2"
[1] "No of outliers with ub as 775 is 1"
[1] "No of outliers with ub as 825 is 1"'

'Not done anything with the outliers____________________________________________'

#Boxplot
boxplot(diab$Insulin,
        horizontal = TRUE,
        col = 'orange3',
        main = 'Boxplot of Insulin')

#Outcome vs Insulin
plot(diab$Insulin, diab$Outcome,
     xlab = 'Insulin',
     ylab = 'Diabetes Outcome',
     main = 'Diabetes Outcome vs Insulin',
     col = c('blue4','brown4','darkgreen','purple4'))

#Dividing the plot area as 1 rows 2 columns
par(mfrow = c(1,2))

#Histogram
hist(diab_no$Insulin,
     col = brewer.pal(8, 'Accent'),
     xlab = 'Insulin',
     main = 'Without Diabetes')

hist(diab_yes$Insulin,
     col = brewer.pal(8, 'Dark2'),
     xlab = 'Insulin',
     main = 'With Diabetes')

#Making plot area as 1 row as 1 column
par(mfrow = c(1,1)) 

#___________________________BMI                     
str(diab$BMI)
'num [1:728] 33.6 26.6 23.3 28.1 43.1 25.6 31 30.5 0 37.6 ...'

summary(diab$BMI)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   27.40   32.30   32.29   36.60   67.10 '

#Histogram
hist(diab$BMI,
     col = brewer.pal(9, 'Set1'),
     xlab = 'BMI',
     main = 'Histogram of BMI')

#Boxplot
boxplot(diab$BMI,
        horizontal = TRUE,
        col = 'springgreen4',
        main = 'Boxplot of BMI')

#Checking the  Upper Boundary & Lower Boundary
bmi_ub = quantile(diab$BMI, 0.75)+1.5*IQR(diab$BMI)
print(bmi_ub)
' 75% 
 50.4'

bmi_lb = quantile(diab$BMI, 0.25)-1.5*IQR(diab$BMI)
print(bmi_lb)

' 25% 
 13.6'

#Checking the No of outliers
length(diab$BMI[diab$BMI > bmi_ub])
'7'

length()diab$BMI[diab$BMI < bmi_lb]
'4'

"BMI can't be zero, so removing zero observations"

diab = diab[diab$BMI != 0, ]

dim(diab)
'724   9'
#Boxplot
boxplot(diab$BMI,
        horizontal = TRUE,
        col = 'limegreen',
        main = 'Boxplot of BMI')

#Outcome vs BMI
plot(diab$BMI, diab$Outcome,
     xlab = 'BMI',
     ylab = 'Diabetes Outcome',
     main = 'Diabetes Outcome vs BMI',
     col = c('blue4','brown4','darkgreen','purple4'))

#Dividing the plot area as 1 rows 2 columns
par(mfrow = c(1,2))

#Histogram
hist(diab_no$BMI,
     col = brewer.pal(8, 'Accent'),
     xlab = 'BMI',
     main = 'Without Diabetes')

hist(diab_yes$BMI,
     col = brewer.pal(8, 'Dark2'),
     xlab = 'BMI',
     main = 'With Diabetes')

#Making plot area as 1 row as 1 column
par(mfrow = c(1,1)) 

#___________________________DiabetesPedigreeFunction 
str(diab$DiabetesPedigreeFunction)
' num [1:728] 0.627 0.351 0.672 0.167 2.288 ...'

summary(diab$DiabetesPedigreeFunction)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0780  0.2450  0.3800  0.4765  0.6295  2.4200 '

#Histogram
hist(diab$DiabetesPedigreeFunction,
     col = brewer.pal(9, 'Set1'),
     xlab = 'DiabetesPedigreeFunction',
     main = 'Histogram of DiabetesPedigreeFunction')

#Boxplot
boxplot(diab$DiabetesPedigreeFunction,
        horizontal = TRUE,
        col = 'greenyellow',
        main = 'Boxplot of DiabetesPedigreeFunction')

#Checking the  no of outliers
dpf_ub = quantile(diab$DiabetesPedigreeFunction, 0.75)+1.5*IQR(diab$DiabetesPedigreeFunction)
print(dpf_ub)
'    75% 
 1.20625 '

#Checking the No of outliers
length(diab$DiabetesPedigreeFunction[diab$DiabetesPedigreeFunction > dpf_ub])
'29'

'Keeping the outliers'

#Outcome vs DiabetesPedigreeFunction
plot(diab$DiabetesPedigreeFunction, diab$Outcome,
     xlab = 'DiabetesPedigreeFunction',
     ylab = 'Diabetes Outcome',
     main = 'Diabetes Outcome vs DiabetesPedigreeFunction',
     col = c('darkred', 'darkgreen','darkblue'))

#Dividing the plot area as 1 rows 2 columns
par(mfrow = c(1,2))

#Histogram
hist(diab_no$DiabetesPedigreeFunction,
     col = brewer.pal(8, 'Accent'),
     xlab = 'DiabetesPedigreeFunction',
     main = 'Without Diabetes')

hist(diab_yes$DiabetesPedigreeFunction,
     col = brewer.pal(8, 'Dark2'),
     xlab = 'DiabetesPedigreeFunction',
     main = 'With Diabetes')

#Making plot area as 1 row as 1 column
par(mfrow = c(1,1)) 

#___________________________Age
str(diab$Age)
'int [1:728] 50 31 32 21 33 30 26 53 54 30 ...'

summary(diab$Age)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  21.00   24.00   29.00   33.39   41.00   81.00 '

#Histogram
hist(diab$Age,
     col = brewer.pal(9, 'Set1'),
     xlab = 'Age',
     main = 'Histogram of Age')

#Boxplot
boxplot(diab$Age,
        horizontal = TRUE,
        col = 'darkslateblue',
        main = 'Boxplot of Age')

#Checking the  no of outliers
age_ub = quantile(diab$Age, 0.75)+1.5*IQR(diab$Age)
print(age_ub)
' 75% 
 66.5 '

#Checking the No of outliers
length(diab$Age[diab$Age > age_ub])
'7'

'Keeping the outliers'

#Outcome vs Age
plot(diab$Age, diab$Outcome,
     xlab = 'Age',
     ylab = 'Diabetes Outcome',
     main = 'Diabetes Outcome vs Age',
     col = brewer.pal(8,'Set1'))

#Dividing the plot area as 1 rows 2 columns
par(mfrow = c(1,2))

#Histogram
hist(diab_no$Age,
     col = brewer.pal(8, 'Accent'),
     xlab = 'Age',
     main = 'Without Diabetes')

hist(diab_yes$Age,
     col = brewer.pal(8, 'Dark2'),
     xlab = 'Age',
     main = 'With Diabetes')

#Making plot area as 1 row as 1 column
par(mfrow = c(1,1)) 

str(diab)
#Hypothesis Testing
'H0 mean diffference of the groups is zero
Alternative Hypothesis mean difference of the groups is not equal to 0'

t.test(diab$Pregnancies ~ diab$Outcome) #Good Predictor
'p-value <= 0.05 therefore, Null Hypothesis rejected'

t.test(diab$Glucose ~ diab$Outcome) #Good Predictor
'p-value <= 0.05 therefore, Null Hypothesis rejected'

t.test(diab$BloodPressure ~ diab$Outcome) #Good Predictor
'p-value <= 0.05 therefore, Null Hypothesis rejected'

t.test(diab$SkinThickness ~ diab$Outcome) #Good Predictor
'p-value <= 0.05 therefore, Null Hypothesis rejected'

t.test(diab$Insulin ~ diab$Outcome) #Good Predictor
'p-value <= 0.05 therefore, Null Hypothesis rejected'

t.test(diab$BMI ~ diab$Outcome) #Good Predictor
'p-value <= 0.05 therefore, Null Hypothesis rejected'

t.test(diab$DiabetesPedigreeFunction ~ diab$Outcome) #Good Predictor
'p-value <= 0.05 therefore, Null Hypothesis rejected'

t.test(diab$Age ~ diab$Outcome) #Good Predictor
'p-value <= 0.05 therefore, Null Hypothesis rejected'

#Splitting the data into train & test
set.seed(123)
select_rows_80 = sample(1:nrow(diab), round(0.8*nrow(diab)), replace = F)

#Train Data
diab_train = diab[select_rows_80,]

#Test Data
diab_test = diab[-select_rows_80,]

#Building the Model
model = glm(Outcome~., data = diab_train, family = 'binomial')
summary(model)

#Prediction - training data
logreg_pred = predict(model, type = 'response')
head(logreg_pred)

#Rounding the values
logreg_pred = ifelse(logreg_pred > 0.5, 1, 0)
head(logreg_pred)

pred_tabl = table(diab_train$Outcome, logreg_pred)
print(pred_tabl)
'   logreg_pred
      0   1
  0 334  42
  1  87 116'

#Accuracy - Training data
sum(diag(pred_tabl))/sum(pred_tabl)
'0.7772021'

#Prediction - Test Data
logreg_pred1 = predict(model, diab_test, type = 'response')
head(logreg_pred1)

#Rounding the values
logreg_pred1 = ifelse(logreg_pred1 > 0.5, 1, 0)
head(logreg_pred1)

pred_tabl1 = table(diab_test$Outcome, logreg_pred1)
print(pred_tabl1)
'   logreg_pred1
     0  1
  0 84 15
  1 21 25'

#Accuracy
sum(diag(pred_tabl1))/sum(pred_tabl1)
'0.7517241'

library(ROCR)
#ROC - AUC Curve
pred_roc = prediction(logreg_pred1, diab_test$Outcome)
pred_roc_per = performance(pred_roc, measure = 'tpr', x.measure = 'fpr')

auc = performance(pred_roc, measure = 'auc')
auc = auc@y.values[[1]]
auc
'0.6959816'

#Plotting curve
plot(pred_roc_per)
plot(pred_roc_per, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1 ),
     main = 'ROC Curve')
abline(a = 0, b = 1)
auc = round(auc, 4)
legend(0, 1, auc, title = 'AUC', cex = 0.7)

#Removing variables with p value more than 0.05
model1 = glm(Outcome~Pregnancies+Glucose+BMI+DiabetesPedigreeFunction,
             data = diab_train, family = 'binomial')
summary(model1)

#Prediction - training data
logreg_pred = predict(model1, type = 'response')
head(logreg_pred)

#Rounding the values
logreg_pred = ifelse(logreg_pred > 0.5, 1, 0)
head(logreg_pred)

pred_tabl = table(diab_train$Outcome, logreg_pred)
print(pred_tabl)
'   logreg_pred
      0   1
  0 333  43
  1  86 117'

#Accuracy - Training data
sum(diag(pred_tabl))/sum(pred_tabl)
'0.7772021'

#Prediction - Test Data
logreg_pred1 = predict(model1, diab_test, type = 'response')
head(logreg_pred1)

#Rounding the values
logreg_pred1 = ifelse(logreg_pred1 > 0.5, 1, 0)
head(logreg_pred1)

pred_tabl1 = table(diab_test$Outcome, logreg_pred1)
print(pred_tabl1)
'   logreg_pred1
     0  1
  0 84 15
  1 22 24'

#Accuracy
sum(diag(pred_tabl1))/sum(pred_tabl1)
' 0.7448276'

library(ROCR)
#ROC - AUC Curve
pred_roc = prediction(logreg_pred1, diab_test$Outcome)
pred_roc_per = performance(pred_roc, measure = 'tpr', x.measure = 'fpr')

auc = performance(pred_roc, measure = 'auc')
auc = auc@y.values[[1]]
auc
'0.685112'

#Plotting curve
plot(pred_roc_per)
plot(pred_roc_per, colorize = TRUE,
     print.cutoffs.at = seq(0.1, by = 0.1 ),
     main = 'ROC Curve')
abline(a = 0, b = 1)
auc = round(auc, 4)
legend(0, 1, auc, title = 'AUC', cex = 0.7)

