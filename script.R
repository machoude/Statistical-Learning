data.set=read.table("data.txt",sep = ',')
names.of.data=c("n.pregnant","glucose","b.p","s.t","inslunin",'b.m.index',"diabetes","age","class")
colnames(data.set)=names.of.data
data.A=data.set
data.B=data.set
#data set B without the missing values
data.B["glucose"][data.B["glucose"]==0]=NA
data.B["s.t"][data.B["s.t"]==0]=NA
data.B["b.p"][data.B["b.p"]==0]=NA
data.B["inslunin"][data.B["inslunin"]==0]=NA
data.B["b.m.index"][data.B["b.m.index"]==0]=NA
summary(data.B)
df=data.B[!(is.na(data.B$glucose) | is.na(data.B$b.p) | is.na(data.B$s.t) | is.na(data.B$inslunin) | 
              is.na(data.B$b.m.index)),]
data.B=df
#install.packages("mice")
library(mice)
#convert the zeros to NA
data.A["glucose"][data.A["glucose"]==0]=NA
data.A["s.t"][data.A["s.t"]==0]=NA
data.A["b.p"][data.A["b.p"]==0]=NA
data.A["inslunin"][data.A["inslunin"]==0]=NA
data.A["b.m.index"][data.A["b.m.index"]==0]=NA
data.A["diabetes"][data.A["diabetes"]==0]=NA
summary(data.A)
md.pattern(data.A)
#Imputation for data set A
#Predictive mean matching
data.imp.1=mice(data.A,method = "pmm",maxit = 20)
f.data.A.1=complete(data.imp.1,3)
#Lasso linear regression
data.imp.2=mice(data.A,method = "lasso.norm",maxit = 20)
f.data.A.2=complete(data.imp.2,3)
#Linear regression using bootstrap
data.imp.3=mice(data.A,method = "norm.boot",maxit = 20)
f.data.A.3=complete(data.imp.3,3)
summary(f.data.A.1)
summary(f.data.A.2)
summary(f.data.A.3)
summary(data.A)
imp.data.A=f.data.A.2
#modeling with glm
model=glm(class ~ .,data = imp.data.A,family = binomial)
summary(model)
f.model=glm(class ~ n.pregnant + glucose + b.m.index + diabetes,data = imp.data.A,family=binomial)
summary(f.model)
coef(f.model)
#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(dplyr)
#data visualization
imp.data.A%>%ggplot(aes(n.pregnant))+geom_density()
imp.data.A%>%ggplot(aes(glucose))+geom_density()
imp.data.A%>%ggplot(aes(b.m.index))+geom_density()
imp.data.A%>%ggplot(aes(diabetes))+geom_density()
#The variable of the number of the times pregnant has 3 outliers
imp.data.A%>%ggplot(aes(y=n.pregnant))+geom_boxplot(outlier.colour = "red")
#the variable of the glucose has no outliers
imp.data.A%>%ggplot(aes(y=glucose))+geom_boxplot(outlier.colour = "red")
#the body mass index has 7 outliers
imp.data.A%>%ggplot(aes(y=b.m.index))+geom_boxplot(outlier.colour = "red")
#the diabetes has many outliers
imp.data.A%>%ggplot(aes(y=diabetes))+geom_boxplot(outlier.colour = "red")
#Outliers' removing with interquartile method
#diabetes
Q1 <- quantile(imp.data.A$diabetes, .25)
Q3 <- quantile(imp.data.A$diabetes, .75)
IQR <- IQR(imp.data.A$diabetes)
nod <- subset(imp.data.A, imp.data.A$diabetes> (Q1 - 1.5*IQR) & imp.data.A$diabetes< (Q3 + 1.5*IQR))
Q1 <- quantile(imp.data.A$b.m.index, .25)
Q3 <- quantile(imp.data.A$b.m.index, .75)
IQR <- IQR(imp.data.A$b.m.index)
nod <- subset(imp.data.A, imp.data.A$b.m.index> (Q1 - 1.5*IQR) & imp.data.A$b.m.index< (Q3 + 1.5*IQR))
dim(nod)
dim(imp.data.A)
#Normallity test
data.a=nod
shapiro.test(data.a$n.pregnant)
shapiro.test(data.a$glucose)
shapiro.test(data.a$b.m.index)
shapiro.test(data.a$diabetes)
#log transformation
shapiro.test(log(data.a$n.pregnant+1))
shapiro.test(log(data.a$glucose))
shapiro.test(log(data.a$b.m.index))
shapiro.test(log(data.a$diabetes))
#install.packages("MASS")
#install.packages("caret")
library(MASS)
#library(caret)
set.seed(2121)
# Create dummy for splitting (80 - 20)
split_dummy <- sample(c(rep(0, 0.8 * nrow(data.a)),  
                        rep(1, 0.2 * nrow(data.a))))
data_train.a <- data.a[split_dummy == 0, ]
data_test.a <- data.a[split_dummy == 1, ]
#Linear Discrimination Analysis
lda.a=lda(class ~ n.pregnant + glucose + b.m.index + diabetes ,data = data_train.a)
lda.a
plot(lda.a)
#prediction with the test data set
lda.a.pred = predict(lda.a,data_test.a,type="response")
names(lda.a.pred)
lda.a.class=lda.a.pred$class
table(lda.a.class,data_test.a$class)
#misclassification error
mle.a=(29+7)/148
mle.a
#sensitivity
sensitivity.lda.a = (81+31)/148
sensitivity.lda.a
#FNR
fnr.lda.a=30/(30+30)
fnr.lda.a
# Quadratic Discriminant Analysis
qda.a = qda(class ~ n.pregnant + glucose + b.m.index + diabetes ,data = data_train.a)
qda.a
qda.a.pred = predict(qda.a,data_test.a)
qda.a.class=qda.a.pred$class
table(qda.a.class,data_test.a$class)
#misclassification error
mqe.a=(26+11)/148
mqe.a
#sensitivity
sensitivity.qda.a = (77+34)/148
sensitivity.qda.a
#FNR
fnr.qda.a=26/(26+34)
fnr.qda.a
# K-Nearest Neighbors
library(class)
train.x.a=cbind(data_train.a$n.pregnant,data_train.a$glucose,data_train.a$b.m.index,data_train.a$diabetes)
test.x.a=cbind(data_test.a$n.pregnant,data_test.a$glucose,data_test.a$b.m.index,data_test.a$diabetes)
knn.a.pred=knn(train.x.a,test.x.a,data_train.a$class,k=3)
table(knn.a.pred,data_test.a$class)
#misclassification error
mke.a=(31+14)/148
mke.a
#sensitivity
sensitivity.knn.a=(74+29)/148
sensitivity.knn.a
#FNR
fnr.knn.a=30/(30+30)
fnr.knn.a
#data set b
#modeling
f.model.b=glm(class ~ glucose  + b.m.index + diabetes + age,data = data.B,family = binomial)
summary(f.model.b)
library(ggplot2)
library(dplyr)
#data visualization
data.B%>%ggplot(aes(glucose))+geom_density()
data.B%>%ggplot(aes(b.m.index))+geom_density()
data.B%>%ggplot(aes(diabetes))+geom_density()
data.B%>%ggplot(aes(age))+geom_density()
#The variable of the age has 8 outliers
data.B%>%ggplot(aes(y=age))+geom_boxplot(outlier.colour = "red")
#the variable of the glucose has no outliers
data.B%>%ggplot(aes(y=glucose))+geom_boxplot(outlier.colour = "red")
#the body mass index has 6 outliers
data.B%>%ggplot(aes(y=b.m.index))+geom_boxplot(outlier.colour = "red")
#the diabetes has 10 outliers
data.B%>%ggplot(aes(y=diabetes))+geom_boxplot(outlier.colour = "red")
#Outliers' removing with interquartile method
#diabetes
Q1 <- quantile(data.B$diabetes, .25)
Q3 <- quantile(data.B$diabetes, .75)
IQR <- IQR(data.B$diabetes)
nb <- subset(data.B, data.B$diabetes> (Q1 - 1.5*IQR) & data.B$diabetes< (Q3 + 1.5*IQR))
dim(nb)
dim(data.B)
data.b=nb
hapiro.test(data.b$age)
shapiro.test(data.b$glucose)
shapiro.test(data.b$b.m.index)
shapiro.test(data.b$diabetes)
#none of the variables are normally distributed
#log transformation (not working)
shapiro.test(log(data.b$age))
shapiro.test(log(data.b$glucose))
shapiro.test(log(data.b$b.m.index))
shapiro.test(log(data.b$diabetes))
library(MASS)
set.seed(2021)
# Create dummy for splitting (80-20)
split_dummy <- sample(c(rep(0, 0.8 * nrow(data.b)),  
                        rep(1, 0.2 * nrow(data.b))))
data_train.b <- data.b[split_dummy == 0, ]
data_test.b <- data.b[split_dummy == 1, ]
#Linear Discrimination Analysis
lda.b=lda(class ~  glucose  + b.m.index + diabetes + age,data = data_train.b)
lda.b
plot(lda.b)
#prediction with the test data
lda.b.pred = predict(lda.b,data_test.b)
lda.b.class=lda.b.pred$class
table(lda.b.class,data_test.b$class)
#sensitivity
sensitivity.lda.b=(39+17)/76
sensitivity.lda.b
#misclassification
mle.b=20/76
mle.b
#FNR 
fnr.lda.b=12/(12+17)
fnr.lda.b
# Quadratic Discriminant Analysis
qda.b = qda(class ~  glucose  + b.m.index + diabetes + age,data = data_train.b)
qda.b
qda.b.pred = predict(qda.b,data_test.b)
qda.b.class=qda.b.pred$class
table(qda.b.class,data_test.b$class)
#sensitivity
sensitivity.qda.b=(38+16)/76
sensitivity.qda.b
#misclassification
mqe.b=(9+13)/76
mqe.b
#FNR
fnr.qda.b=13/(13+16)
fnr.qda.b
# K-Nearest Neighbors
library(class)
train.x.b=cbind(data_train.b$n.pregnant,data_train.b$glucose,data_train.b$b.m.index,data_train.b$diabetes)
test.x.b=cbind(data_test.b$n.pregnant,data_test.b$glucose,data_test.b$b.m.index,data_test.b$diabetes)
knn.b.pred=knn(train.x.b,test.x.b,data_train.b$class,k=3)
table(knn.b.pred,data_test.b$class)
#sensitivity
sensitivity.knn.b=(43+14)/76
sensitivity.knn.b
#misclassification
mke.b=(4+15)/76
mke.b
#FNR
fnr.knn.b=15/(14+15)
fnr.knn.b
#final results
nam=c("lda-a","qda-a","knn-a","lda-b","qda-b","knn-b")
sen=c(sensitivity.lda.a,sensitivity.qda.a,sensitivity.knn.a,sensitivity.lda.b,sensitivity.qda.b,sensitivity.knn.b)
ms=c(mle.a,mqe.a,mke.a,mle.b,mqe.b,mke.b)
fnr=c(fnr.lda.a,fnr.qda.a,fnr.knn.a,fnr.lda.b,fnr.qda.b,fnr.knn.b)
d=data.frame(nam,sen,ms,fnr)
knitr::kable(d,"pipe",col.names = c("Method-DataSet","Sensitivity","Misclassification Rate","False Negative Rate"))


