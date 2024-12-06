library(tidyverse)
library(naniar)
library(VIM)
library(lawstat)

rm(list=ls())

df=read.csv('diabetes.csv')
summary(df)
view(df)

df %>% 
  filter(Insulin > 400) %>% 
  view()

summary(df)
colSums(is.na(df))
corrplot::corrplot(cor(df))


df[df$Glucose==0,'Glucose']=NA
df[df$BloodPressure==0,'BloodPressure']=NA
df[df$SkinThickness==0,'SkinThickness']=NA
df[df$Insulin==0,'Insulin']=NA
df[df$BMI==0,'BMI']=NA

table(apply(is.na(df),1,sum))

miss_var_summary(df)

df %>% 
  arrange(Insulin, SkinThickness, BloodPressure, BMI) %>% 
  vis_miss()

#등분산 및 정규성 검정
df_omit=na.omit(df)

#Pregnancies
levene.test(df_omit$Pregnancies, df_omit$Outcome)

#등분산 x
wilcox.test(df_omit$Pregnancies, df_omit$Outcome)

#두 그룹 차이 있다고 할 수 있음

#Glucose
levene.test(df_omit$Glucose, df_omit$Outcome)

#등분산 x

wilcox.test(df_omit$Glucose, df_omit$Outcome)

#두 그룹 차이 있다고 할 수 있음


#BloodPressure
levene.test(df_omit$BloodPressure, df_omit$Outcome)

#등분산 o

shapiro.test(df_omit[df_omit$Outcome==0,'BloodPressure'])
shapiro.test(df_omit[df_omit$Outcome==1,'BloodPressure'])

#정규성 따름

#두 그룹 차이 있다고 할 수 있음
t.test(df_omit[df_omit$Outcome==0,'BloodPressure'],
       df_omit[df_omit$Outcome==1,'BloodPressure'],var.equal = T)

#두 그룹 차이 있다고 할 수 있음


#SkinThickness
levene.test(df_omit$SkinThickness, df_omit$Outcome)

#등분산 o

shapiro.test(df_omit[df_omit$Outcome==0,'SkinThickness'])
shapiro.test(df_omit[df_omit$Outcome==1,'SkinThickness'])

#정규성 x

wilcox.test(df_omit$SkinThickness, df_omit$Outcome)

#두 그룹 차이 있다고 할 수 있음


#Insulin
levene.test(df_omit$Insulin, df_omit$Outcome)

#등분산 x

wilcox.test(df_omit$Insulin, df_omit$Outcome)

#두 그룹 차이 있다고 할 수 있음

#BMI
levene.test(df_omit$Insulin, df_omit$Outcome)

#등분산 o

shapiro.test(df_omit[df_omit$Outcome==0,'Insulin'])
shapiro.test(df_omit[df_omit$Outcome==1,'Insulin'])

#정규성 x

wilcox.test(df_omit$Insulin, df_omit$Outcome)

#두 그룹 차이 있다고 할 수 있음


#DiabetesPedigreeFunction
levene.test(df_omit$DiabetesPedigreeFunction, df_omit$Outcome)

#등분산 o

shapiro.test(df_omit[df_omit$Outcome==0,'DiabetesPedigreeFunction'])
shapiro.test(df_omit[df_omit$Outcome==1,'DiabetesPedigreeFunction'])

#정규성 x

wilcox.test(df_omit$DiabetesPedigreeFunction, df_omit$Outcome)

#두 그룹 차이 있다고 할 수 있음

#Age
levene.test(df_omit$Age, df_omit$Outcome)

#등분산 x

wilcox.test(df_omit$Age, df_omit$Outcome)

#두 그룹 차이 있다고 할 수 있음










