Diabetes = read.csv("Diabetes_work.csv", header = TRUE)
head(Diabetes)
library(plyr)
library(dplyr)
library(stats)
Diabetes$test <- revalue(Diabetes$test, c("positive"=1))
Diabetes$test <- revalue(Diabetes$test, c("negative"=0))
head(Diabetes$test)
Diabetes$test = as.factor(Diabetes$test)
test <- Diabetes$test
head(test)
head(Diabetes)
head(Diabetes$test)

#detecting missing values
library(faraway)
data(pima)
head(pima,5)
help(pima)
dim(pima)
summary(pima)
d<-pima
d
d$diastolic[d$diastolic==0]=NA
d$diastolic
d$glucose[d$glucose==0]=NA
d$triceps[d$triceps==0]=NA
d$bmi[d$bmi==0]=NA
d$insulin[d$insulin==0]=NA
summary(d)
hist(d$diastolic,xlab="Diastolic",main="",col = "green",border="red")#histogram gives us the distribution of our data
boxplot(d$diastolic,ylab="Diastolic",main="",col="blue")#main is the name of the diagram here it doesnt have any names
library(plotrix)
d$test<-as.factor(d$test)
levels(d$test)=c("negative","positive")
posCount<-length(which(d$test=="positive"))
posCount
sum(d$test=="positive")#this does the same code as the last one
negCount<-length(which(d$test=="negative"))
negCount
x<-c(negCount, posCount)
x
lbl<-c("negative","positive")
lbl
pie3D(x,labels= lbl,explode= 0.1, main = "Pie Chart of Test Results")
plot(d$bmi,d$triceps,xlab="BMI",ylab="Triceps",main="")#it shows us the relation between our data
plot(d$bmi,d$triceps,xlab="BMI",ylab="Triceps",main="",type='b')
plot(d$bmi,d$triceps,xlab="BMI",ylab="Triceps",main="",type='l')
plot(d$bmi,d$triceps,xlab="BMI",ylab="Triceps",main="",pch=5)
plot(d$bmi,d$triceps,xlab="BMI",ylab="Triceps",main="",col='red')
plot(d$test,d$diastolic,ylab="Diastolic",main="")
pairs(d[,c("glucose","bmi","triceps","insulin")])
par(mfrow=c(2,2))#meaning our page has 2 rows and 2 columns #this code is to determine the dimension of our page
plot(d$bmi,d$triceps,xlab="BMI",ylab="Triceps",main="")
plot(pima$test,pima$diastolic,ylab="Diastolic",main="")
hist(d$diastolic,xlab="Diastolic",main="",col = "green",border= "red")
pie3D(x,labels= lbl,explode= 0.1, main = "Pie Chart of Test Results")
#here we want to know about missing values
library(mice)
library(VIM)
aggr<-aggr(d, col=c('black','red'), numbers=TRUE, sortVars=TRUE, labels=names(d), cex.axis=.7, gap=1, ylab=c("Barplotof missing data","Patterns"))#sortvars=TRUE organizes data
#cex.axis is font #gap is the distance between diagrams
md.pattern(d)
md.pairs(d)
marginplot(d[,c(2,5)])#2nd and 5th column
#the red spots is the distribution diagram of glucose for when insulin is missed
#the blue boxplot is the boxplot of glucose for when insulin is observed and the red one is for when insulin is missed
#5 is the missing value numbers of glucose and 4 is the common number of missing values for glucose and insulin and 374 is the missing value number of insulin
marginplot(d[,c(4,5)])
marginplot(d[,c(8,4)])
missnum<-function(x){a<-sum(is.na(x))/length(x); return(a)}#it takes a row and tells us how many of them are missed and it would divide it to the length of our row
missnum
missnum(d[1,])
miss<-apply(d,1,missnum)#here we applied the function to all rows #1 means row if it was 2 means column
miss
which.max(miss)
which(miss>0.3)
which.max(miss)
d$test=='positive'
mean(miss[d$test=='positive'])
d$test=='negative'
mean(miss[d$test=='negative'])
tapply(miss,d$test,mean)#we want mean of every different category in test
tapply(miss,d$test,sd)
impu<-mice(d,m=5,maxit=50,meth='pmm')#meth is short for method here we choose our method #maxit is short for max iteration #m is the number of times we want the imputation
#the code gives us a matrix with rows which is the number of our missing value and columns which is the number of m
impu$imp$glucose#it gives us the rows which were missed and now they are imputed
com<-complete(impu,1)#the second argument means we want the first iteration #this function gives us the whole dataset by subtituting the imputed missing values
summary(com)
head(d)
xyplot(impu, insulin ~ triceps+glucose+diastolic| .imp, pch= 20, cex= 1.4)#blue spots are the observed ones and th red spots are the imputed missing values
stripplot(impu, pch= 20, cex= 1.2)
densityplot(impu)

#Histograms
hist(pregnant,
     xlab = "pregnancy",
     main = "Histogram of pregnancy",
     breaks = sqrt(nrow(DataSet))
)

hist(glucose,
     xlab = "glucose",
     main = "Histogram of glucose",
     breaks = sqrt(nrow(DataSet))
)

hist(diastolic,
     xlab = "blood pressure",
     main = "Histogram of blood pressure",
     breaks = sqrt(nrow(DataSet))
)

hist(triceps,
     xlab = "skin thickness",
     main = "Histogram of skin thickness",
     breaks = sqrt(nrow(DataSet))
)

hist(insulin,
     xlab = "insulin",
     main = "Histogram of insulin",
     breaks = sqrt(nrow(DataSet))
)

hist(bmi,
     xlab = "bmi",
     main = "Histogram of body mass index",
     breaks = sqrt(nrow(DataSet))
)

hist(diabetes,
     xlab = "diabetes pedigree function",
     main = "Histogram of diabetes pedigree function",
     breaks = sqrt(nrow(DataSet))
)

hist(age,
     xlab = "age",
     main = "Histogram of age",
     breaks = sqrt(nrow(DataSet))
)

#Boxplots
boxplot(pregnant, ylab = "pregnancy")

boxplot(glucose, ylab = "glucose")

boxplot(diastolic, ylab = "blood pressure")

boxplot(triceps, ylab = "skin thickness")

boxplot(insulin, ylab = "insulin")

boxplot(bmi, ylab = "body mass index")

boxplot(diabetes, ylab = "diabetes pedigree function")

boxplot(age, ylab = "age")

#Outliers Detection
#Values that are outside the interval I = [1st Qu - 1.5*(3rd Qu - 1st Qu), 3rd Qu + 1.5*(3rd Qu - 1st Qu)]
pregnant_out <- boxplot.stats(pregnant)$out
pregnant_out
pregnant_out_ind <- which(pregnant %in% c(pregnant_out))
pregnant_out_ind

glucose_out <- boxplot.stats(glucose)$out
glucose_out
glucose_out_ind <- which(glucose %in% c(glucose_out))
glucose_out_ind

diastolic_out <- boxplot.stats(diastolic)$out
diastolic_out
diastolic_out_ind <- which(diastolic %in% c(diastolic_out))
diastolic_out_ind

triceps_out <- boxplot.stats(triceps)$out
triceps_out
triceps_out_ind <- which(triceps %in% c(triceps_out))
triceps_out_ind

insulin_out <- boxplot.stats(insulin)$out
insulin_out
insulin_out_ind <- which(insulin%in% c(insulin_out))
insulin_out_ind

bmi_out <- boxplot.stats(bmi)$out
bmi_out
bmi_out_ind <- which(bmi %in% c(bmi_out))
bmi_out_ind

diabetes_out <- boxplot.stats(diabetes)$out
diabetes_out
diabetes_out_ind <- which(diabetes %in% c(diabetes_out))
diabetes_out_ind

age_out <- boxplot.stats(age)$out
age_out
age_out_ind <-which(age %in% c(age_out))
age_out_ind


#Fit a regression model (logistic regression)
lr_model <- glm( test ~ pregnant + glucose + diastolic + triceps + insulin + bmi + diabetes + age, data = DataSet,family = binomial(link='logit'))
summary(lr_model)

model1=glm(d$test~.+pregnant*insulin,data = DataSet, family = "binomial")
model2=glm(d$test~.+ age*insulin,data = DataSet, family = "binomial")
summary(model)
summary(model1)
summary(model2)

##Check collinearity
install.packages("car")
library(car)
vif(model)
vif(model2)
sqrt(vif(lr_model))>2
sqrt(vif(model2))>2

d=DataSet
res=cor(d)
res

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE, Colv = NA, Rowv = NA)

## Pregnancie as a categorical variable
Pregnancie_Cat=as.factor(ifelse(d$pregnant>0,1,0))
Pregnancie_Cat
levels(Pregnancie_Cat)=c("not given birth", "given birth")
Pregnancie_Cat


newdata=data.frame(Pregnancie_Cat,d$glucose,d$diastolic,d$triceps,d$insulin,d$bmi,d$diabetes,d$age, d$test)
head(newdata)
model3=glm(newdata$d.test~., family="binomial", data=newdata)
summary(model3)
Pregnancie_Cat
d$test
table(Pregnancie_Cat)
tab_preg_cat=xtabs(~Pregnancie_Cat+newdata$d.test, data=newdata)
tab_preg_cat
tab_preg_cat[1,2]

par(mfrow=c(1,1))
plot(tab_preg_cat, col=c("green","blue"))
plot(t(tab_preg_cat),col=c("green","blue"))
RR_preg_cat=tab_preg_cat[2,2]/tab_preg_cat[1,2]
RR_preg_cat  ##6.052632
OR=(tab_preg_cat[1,1]*tab_preg_cat[2,2])/(tab_preg_cat[1,2]*tab_preg_cat[2,1])
OR  ##1.034759


##the probability of resulting positive to the test given that you were pregnant
#is 6.05 the one that you could have had without the pregnancie
tab=xtabs(~d$pregnant+d$test,data=d)

DataSet=d
#Calculate leverage for each observation in the model
hats <- as.data.frame(hatvalues(lr_model))
hats

#Sort observations by leverage, descending
hats[order(-hats['hatvalues(lr_model)']), ]
#Higher leverage point is 0.078, so, since it isn't greater than 2, we know that none of the observations in the dataSet has a high leverge

#Plot leverage values for each observation
plot(hatvalues(lr_model), type = 'h')

##REGRESSION PART
model=glm(d$test~ ., family="binomial", data=DataSet)
summary(model)


#subset selection
library(leaps)
regfit.full=regsubsets(test~.,Diabetes)
summary(regfit.full)
reg.summary = summary(regfit.full)
names(reg.summary)
#some plots to determine which models we prefer based on 
#adjr2, cp, bic
par(mfrow=c(2,2))
#adjr2
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(5,reg.summary$adjr2[5], col="red", cex= 2, pch=20)

#cp
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(4, reg.summary$cp[4], col = "red", cex = 2, pch = 20)

#BIC
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.summary$bic) #BIC
points(4,reg.summary$bic[4],col="red",cex=2,pch=20)

par(mfrow=c(2,2)) 
plot(regfit.full,scale="r2") #black boxes
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

#forward selection
#forward selection
regfit.fwd=regsubsets(test~.,Diabetes, method = "forward")
summary(regfit.fwd)

coef(regfit.full, 4)
coef(regfit.fwd, 4)

final.fit = glm(test ~ pregnant+ glucose+bmi+Diabetes$diabetes ,data = Diabetes, family = "binomial")
summary(final.fit) 

#k.fold cross validation
set.seed(125)
library(tidyverse)
library(dplyr)
library(caret)
library(purrr)
train_control <- trainControl(method = "cv",
                              number = 10)
model <- train(test ~., data = Diabetes,
               method = "glm",
               trControl = train_control)
print(model)

?plot
attach(Diabetes)
model1 <- train(test ~ pregnant + glucose + bmi + diabetes, data = Diabetes,
               method = "glm",
               trControl = train_control)
print(model1)
attach(Diabetes)
model2 <- train(test ~ pregnant + glucose + bmi + diabetes + age, data = Diabetes,
                method = "glm",
                trControl = train_control)
print(model2)

d=DataSet
##selected model
model4=glm(d$test~+d$pregnant+d$bmi+d$glucose+d$diabetes, family="binomial", data=d)
summary(model4)


require(caTools)
set.seed(3)
sample = sample.split(d$test, SplitRatio=0.75)
train_sample = subset(d, sample==TRUE)
train_sample
test_sample = subset(d, sample==FALSE)
nrow(train_sample) ##576
nrow(test_sample)  ##192
##Baseline model
table(d$test)
##Baseline accuracy
baseline_accu <- round(500/nrow(d),2)
baseline_accu  #0.65

##Predict test on training set data

Allvar <- glm(train_sample$test ~ ., data = train_sample, family = binomial)
Predicttrain_sample <- predict(Allvar, type = "response")
summary(Predicttrain_sample)
##Average prediction on each of the two test
tapply(Predicttrain_sample , train_sample$test, mean)

##Build the confusion matrix with threshold value of 0.5
thres=table(train_sample$test,Predicttrain_sample>0.5)
thres
# Accuracy
accur <- round(sum(diag(thres))/sum(thres),2) ##0.76
# Mis-classification error rate
MC <- 1-accur  #0.24
sensi <- round(107/(94+107),2) #Sensitivity at 0.5 threshold: 0.54
specifi <- round(329/(329+46),2) #Specificity at 0.5 threshold: 0.89
sprintf("Sensitivity at 0.5 threshold: %s", sensi)
sprintf("Specificity at 0.5 threshold: %s", specifi)


##ROC Curve
install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(Predicttrain_sample, train_sample$test)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
abline(a=0, b=1)
auc_train <- round(as.numeric(performance(ROCRpred, "auc")@y.values),2)
legend(.8, .2, auc_train, title = "AUC", cex=1)

##MAKE PREDICTION ON TEST SET 
PredictTest <- predict(Allvar, type = "response", newdata = test_sample)
test_tab <- table(test_sample$test, PredictTest > 0.5)
test_tab
accuracy_test <- round(sum(diag(test_tab))/sum(test_tab),2)
#Accuracy on test set is 0.81"


########
##ROC CURVE with a model with less varriables
Selected_model <- glm(train_sample$test ~ train_sample$pregnant+train_sample$glucose+train_sample$bmi+train_sample$diabetes, data = train_sample, family = binomial)
Predict_sel_Train_sample <- predict(Selected_model, type = "response")
summary(Predict_sel_Train_sample)

##Average prediction on each of the two test
tapply(Predict_sel_Train_sample , train_sample$test, mean)

##Build the confusion matrix with threshold value of 0.5
thres_sel=table(train_sample$test,Predict_sel_Train_sample>0.5)
thres_sel

# Accuracy
accur_sel<- round(sum(diag(thres_sel))/sum(thres_sel),2)
sprintf("Accuracy is %s",accur)

# Mis-classification error rate
MC_sel <- 1-accur_sel
MC_sel

nstall.packages("ROCR")
library(ROCR)

###controllare le variabili

ROCRpred_sel = prediction(Predict_sel_Train_sample, train_sample$test)
ROCRperdf_sel = performance(ROCRpred_sel, "tpr", "fpr")
plot(ROCRperf_sel)
abline(a=0, b=1)
auc_train <- round(as.numeric(performance(ROCRpred_sel, "auc")@y.values),2)
legend(.8, .2, auc_train, title = "AUC", cex=1)
