library(tidyverse)
library(naniar)
library(VIM)
library(mice)

rm(list=ls())

df=read.csv('diabetes.csv')

df[df$Glucose==0,'Glucose']=NA
df[df$BloodPressure==0,'BloodPressure']=NA
df[df$SkinThickness==0,'SkinThickness']=NA
df[df$Insulin==0,'Insulin']=NA
df[df$BMI==0,'BMI']=NA

df$Outcome=as.factor(df$Outcome)


#train, test 7:3 으로 분리
set.seed(2018110493)

n=nrow(df)
idx = 1:n

train_idx = sample(idx, n * 0.7)
test_idx = setdiff(idx, train_idx)

train = df[train_idx,]
test = df[test_idx,]

#정규화
for(i in 1:ncol(train)){
  if (class(train[,i])!='factor'){
    m=mean(train[,i], na.rm=T)
    s=sd(train[,i], na.rm=T)
    
    train[,i]=(train[,i]-m)/s
    test[,i]=(test[,i]-m)/s
  }
}

y_train=train$Outcome
y_test=test$Outcome


train=train[,-9]
test=test[,-9]
view(train)

#mice
train_df=mice(train, method = 'norm.predict', m=5)
test_df=mice(test, method = 'norm.predict', m=5)

train1=complete(train_df,1)
train2=complete(train_df,2)
train3=complete(train_df,3)
train4=complete(train_df,4)
train5=complete(train_df,5)

test1=complete(test_df,1)
test2=complete(test_df,2)
test3=complete(test_df,3)
test4=complete(test_df,4)
test5=complete(test_df,5)


train=data.frame(matrix(nrow=nrow(train1),ncol=ncol(train1)))
test=data.frame(matrix(nrow=nrow(test1),ncol=ncol(test1)))

colnames(train)=colnames(train1)
colnames(test)=colnames(test1)

for (i in 1:ncol(train1)){
  if (class(train1[,i])!='factor'){
    train[,i]=(train1[,i]+train2[,i]+train3[,i]+train4[,i]+train5[,i])/5
  }else { train[,i]=train1[,i]
}}

for (i in 1:ncol(test1)){
  if (class(test1[,i])!='factor'){
    test[,i]=(test1[,i]+test2[,i]+test3[,i]+test4[,i]+test5[,i])/5
  }else { test[,i]=test1[,i]
  }}

train$Outcome=y_train
test$Outcome=y_test

X_train=train %>% 
  filter(-Outcome)

X_test=test %>% 
  filter(-Outcome)

y_train=train[,'Outcome']

y_test=test[,'Outcome']

