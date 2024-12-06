library(moonBook)
library(car)
library(caret)
library(ROCR)
library(pROC)
library(Epi)
library(klaR)
library(rpart)
library(rpart.plot)
library(Metrics)
library(mlr)
library(gplots)

prediction=ROCR::prediction
performance=ROCR::performance

############################################################################
#Naive Bayse
#mice
fit_naive=NaiveBayes(Outcome ~ ., data=train)

pred_naive = predict(fit_naive, newdata=test)

cpred_naive = ifelse(pred_naive$posterior[,2] >= 0.5, 1, 0)

# confusion matrix 
confusionMatrix(as.factor(cpred_naive), test$Outcome, positive= '1') 

#시각화
pl_naive=prediction(pred_naive$posterior[,2],test$Outcome)

acc.perf = performance(pl_naive,'acc','cutoff')
recall.perf = performance(pl_naive,'rec','cutoff')
precision.perf = performance(pl_naive,'prec','cutoff')
roc.perf = performance(pl_naive,'tpr','fpr')


#Accuracy, cutoff 시각화
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc_m = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc_m, cutoff = cutoff))

ggplot()+
  geom_line(aes(slot(acc.perf, 'x.values')[[1]], slot(acc.perf, 'y.values')[[1]]))+
  xlab(slot(acc.perf, 'x.name'))+
  ylab(slot(acc.perf, 'y.name'))+
  geom_vline(xintercept=cutoff, linetype = 'dotted', color='red', size = 2)+
  scale_x_continuous(breaks=c(seq(0,1,0.2),cutoff),
                     labels = c(seq(0,1,0.2),round(cutoff,5)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1),acc_m),
                     labels = c(seq(0,1,0.1),round(acc_m,5)))


#recall-precision 시각화
ggplot()+
  geom_line(aes(slot(recall.perf, 'x.values')[[1]], slot(recall.perf, 'y.values')[[1]],
                colour='recall'),
            size=1,
            alpha=.3)+
  geom_line(aes(slot(precision.perf, 'x.values')[[1]], slot(precision.perf, 'y.values')[[1]],
                colour='precision'),
            size=1,
            alpha=.3)+
  xlab(slot(recall.perf, 'x.name'))+
  ylab('recall-precision')+
  scale_x_continuous(breaks=c(seq(0,1,0.2)),
                     labels = c(seq(0,1,0.2)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)),
                     labels = c(seq(0,1,0.1)))+
  scale_colour_manual(breaks = c('recall','precision'),
                      values = c("blue", "red"))


#ROC 시각화
ggplot()+
  geom_line(aes(slot(roc.perf, 'x.values')[[1]], slot(roc.perf, 'y.values')[[1]]),
            size=1,
            col='blue',
            alpha=.4)+
  geom_abline(slope = 1, lty=2)+
  xlab(slot(roc.perf, 'x.name'))+
  ylab(slot(roc.perf, 'y.name'))+
  scale_x_continuous(breaks=c(seq(0,1,0.2)),
                     labels = c(seq(0,1,0.2)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)),
                     labels = c(seq(0,1,0.1)))


############################################################################
#Logistc
#mice
fit_glm=glm(Outcome ~ ., family=binomial, data=train)
?glm
vif(fit_glm)
summary(fit_glm)
ORplot(fit_glm)

pred_glm = predict(fit_glm, newdata=test, type="response")  

cpred_glm = ifelse(pred_glm >= 0.5, 1, 0)

# confusion matrix 
confusionMatrix(as.factor(cpred_glm), test$Outcome, positive= '1')               


#시각화
pl_glm=prediction(pred_glm,test$Outcome)

acc.perf = performance(pl_glm,'acc','cutoff')
recall.perf = performance(pl_glm,'rec','cutoff')
precision.perf = performance(pl_glm,'prec','cutoff')
roc.perf = performance(pl_glm,'tpr','fpr')


#Accuracy, cutoff 시각화
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc_m = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc_m, cutoff = cutoff))

ggplot()+
  geom_line(aes(slot(acc.perf, 'x.values')[[1]], slot(acc.perf, 'y.values')[[1]]))+
  xlab(slot(acc.perf, 'x.name'))+
  ylab(slot(acc.perf, 'y.name'))+
  geom_vline(xintercept=cutoff, linetype = 'dotted', color='red', size = 2)+
  scale_x_continuous(breaks = c(0.1,0.2,0.3,0.6,0.8,1,cutoff),
                     labels = c(0.1,0.2,0.3,0.6,0.8,1,round(cutoff,5)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1),acc_m),
                     labels = c(seq(0,1,0.1),round(acc_m,5)))


#recall-precision 시각화
ggplot()+
  geom_line(aes(slot(recall.perf, 'x.values')[[1]], slot(recall.perf, 'y.values')[[1]],
                colour='recall'),
            size=1,
            alpha=.3)+
  geom_line(aes(slot(precision.perf, 'x.values')[[1]], slot(precision.perf, 'y.values')[[1]],
                colour='precision'),
            size=1,
            alpha=.3)+
  xlab(slot(recall.perf, 'x.name'))+
  ylab('recall-precision')+
  scale_x_continuous(breaks=c(seq(0,1,0.2)),
                     labels = c(seq(0,1,0.2)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)),
                     labels = c(seq(0,1,0.1)))+
  scale_colour_manual(breaks = c('recall','precision'),
                      values = c("blue", "red"))


#ROC 시각화
ggplot()+
  geom_line(aes(slot(roc.perf, 'x.values')[[1]], slot(roc.perf, 'y.values')[[1]]),
            size=1,
            col='blue',
            alpha=.4)+
  geom_abline(slope = 1, lty=2)+
  xlab(slot(roc.perf, 'x.name'))+
  ylab(slot(roc.perf, 'y.name'))+
  scale_x_continuous(breaks=c(seq(0,1,0.2)),
                     labels = c(seq(0,1,0.2)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)),
                     labels = c(seq(0,1,0.1)))



#Decision Tree

#분류기준 설정
# cp : 복잡성
# nsplist : 가지의 분기 수 (nsplit+1 의 리프 노드가 생성)
# rel error : 오류율
# xerror : 교차 검증 오류
# xstd : 교차 검증 오류의 표준오차
# xval : k-fold(과적합 방지 위한 교차검정 k 수)
# minsplit : 분할 위한 최소 기준 관측치

#mice
#grid search
d.tree.mlr <- makeClassifTask(
  data=train, 
  target='Outcome'
)

d.tree.mlr.test <- makeClassifTask(
  data=test, 
  target='Outcome'
)

param_grid_multi <- makeParamSet( 
  makeDiscreteParam('maxdepth', values=1:10),
  makeNumericParam('cp', lower = 0.02, upper = 0.03),
  makeDiscreteParam('minsplit', values=1:10)
)

# Define Grid
control_grid = makeTuneControlGrid()
# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)
# Define Measure
measure = mmce

dt_tuneparam_multi <- tuneParams(learner='classif.rpart', 
                                 task=d.tree.mlr, 
                                 resampling = resample,
                                # measures = xerror,
                                 par.set=param_grid_multi, 
                                 control=control_grid, 
                                 show.info = TRUE)

#Result:  maxdepth=9; cp=0.0289; minsplit=9 : mmce.test.mean=0.2253259

result_hyperparam <- generateHyperParsEffectData(dt_tuneparam_multi, partial.dep = TRUE)

ggplot(
  data = result_hyperparam$data,
  aes(x = cp, y=mmce.test.mean)
) + geom_line(color = 'darkblue')


#Model 생성
my_control = rpart.control(maxdepth=9, cp=0.0289, minsplit=9)

tree_model = rpart(Outcome ~., method = 'class', control = my_control,
                   data = train)

#시각화
rpart.plot(tree_model)

printcp(tree_model)

pred_dt = predict(tree_model, newdata=test, type="prob")  

cpred_dt = ifelse(pred_dt[,2] >= 0.5, 1, 0)

# confusion matrix 
confusionMatrix(as.factor(cpred_dt), test$Outcome, positive= '1') 


pl_dt=prediction(pred_dt[,2],test$Outcome)

acc.perf = performance(pl_dt,'acc','cutoff')
recall.perf = performance(pl_dt,'rec','cutoff')
precision.perf = performance(pl_dt,'prec','cutoff')
roc.perf = performance(pl_dt,'tpr','fpr')

#Accuracy, cutoff 시각화
ind = which.max( slot(acc.perf, "y.values")[[1]] )
acc_m = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc_m, cutoff = cutoff))

ggplot()+
  geom_line(aes(slot(acc.perf, 'x.values')[[1]], slot(acc.perf, 'y.values')[[1]]))+
  xlab(slot(acc.perf, 'x.name'))+
  ylab(slot(acc.perf, 'y.name'))+
  geom_vline(xintercept=cutoff, linetype = 'dotted', color='red', size = 2)+
  scale_x_continuous(breaks = c(0.1,0.2,0.4,0.8,1,cutoff),
                     labels = c(0.1,0.2,0.4,0.8,1,round(cutoff,5)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1),acc_m),
                     labels = c(seq(0,1,0.1),round(acc_m,5)))


#recall-precision 시각화
ggplot()+
  geom_line(aes(slot(recall.perf, 'x.values')[[1]], slot(recall.perf, 'y.values')[[1]],
                colour='recall'),
            size=1,
            alpha=.3)+
  geom_line(aes(slot(precision.perf, 'x.values')[[1]], slot(precision.perf, 'y.values')[[1]],
                colour='precision'),
            size=1,
            alpha=.3)+
  xlab(slot(recall.perf, 'x.name'))+
  ylab('recall-precision')+
  scale_x_continuous(breaks=c(seq(0,1,0.2)),
                     labels = c(seq(0,1,0.2)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)),
                     labels = c(seq(0,1,0.1)))+
  scale_colour_manual(breaks = c('recall','precision'),
                      values = c("blue", "red"))


#ROC 시각화
ggplot()+
  geom_line(aes(slot(roc.perf, 'x.values')[[1]], slot(roc.perf, 'y.values')[[1]]),
            size=1,
            col='blue',
            alpha=.4)+
  geom_abline(slope = 1, lty=2)+
  xlab(slot(roc.perf, 'x.name'))+
  ylab(slot(roc.perf, 'y.name'))+
  scale_x_continuous(breaks=c(seq(0,1,0.2)),
                     labels = c(seq(0,1,0.2)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)),
                     labels = c(seq(0,1,0.1)))







