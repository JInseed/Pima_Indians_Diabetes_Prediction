#평균 대체
rm(list=ls())
df=read.csv('diabetes.csv')

df$Outcome=as.factor(df$Outcome)


#train, test 7:3 으로 분리
set.seed(2018110493)

n=nrow(df)
idx = 1:n

train_idx = sample(idx, n * 0.7)
test_idx = setdiff(idx, train_idx)

train_m = df[train_idx,]
test_m = df[test_idx,]


train_m[train_m$Glucose==0,'Glucose']=mean(train_m$Glucose,na.rm = T)
train_m[train_m$BloodPressure==0,'BloodPressure']=mean(train_m$BloodPressure,na.rm = T)
train_m[train_m$SkinThickness==0,'SkinThickness']=mean(train_m$SkinThickness,na.rm = T)
train_m[train_m$Insulin==0,'Insulin']=mean(train_m$Insulin,na.rm = T)
train_m[train_m$BMI==0,'BMI']=mean(train_m$BMI,na.rm = T)

test_m[test_m$Glucose==0,'Glucose']=mean(train_m$Glucose,na.rm = T)
test_m[test_m$BloodPressure==0,'BloodPressure']=mean(train_m$BloodPressure,na.rm = T)
test_m[test_m$SkinThickness==0,'SkinThickness']=mean(train_m$SkinThickness,na.rm = T)
test_m[test_m$Insulin==0,'Insulin']=mean(train_m$Insulin,na.rm = T)
test_m[test_m$BMI==0,'BMI']=mean(train_m$BMI,na.rm = T)


#정규화
for(i in 1:ncol(train_m)){
  if (class(train_m[,i])!='factor'){
    m=mean(train_m[,i], na.rm=T)
    s=sd(train_m[,i], na.rm=T)
    
    train_m[,i]=(train_m[,i]-m)/s
    test_m[,i]=(test_m[,i]-m)/s
  }
}















