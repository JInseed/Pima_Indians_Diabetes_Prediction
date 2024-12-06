library(randomForest)
library(car)
library(xgboost)

#RandomForest
#mice
#grid search
d.rf.mlr <- makeClassifTask(
  data=train, 
  target='Outcome'
)

d.rf.mlr.test <- makeClassifTask(
  data=test, 
  target='Outcome'
)
param_grid_multi <-makeParamSet(
  makeIntegerParam("mtry", lower = 2, upper = 6),
  makeDiscreteParam('ntree', values=c(100,120,140,160,180)),
  makeDiscreteParam("nodesize", values=c(2:10))
)


# Define Grid
control_grid = makeTuneControlGrid()
# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)
# Define Measure
measure = mmce

dt_tuneparam_multi <- tuneParams(learner='classif.randomForest', 
                                 task=d.rf.mlr, 
                                 resampling = resample,
                                 measures = measure,
                                 par.set=param_grid_multi, 
                                 control=control_grid, 
                                 show.info = TRUE)

#Result: mtry=2; ntree=140; nodesize=7 : mmce.test.mean=0.2253259

result_hyperparam <- generateHyperParsEffectData(dt_tuneparam_multi, partial.dep = TRUE)

ggplot(
  data = result_hyperparam$data,
  aes(x = k, y=acc.test.mean)
) + geom_line(color = 'darkblue')



#Model 생성
#minsplit = nodesize
#mtry = 변수 몇개 써서 각 트리 만들지
rf_model = randomForest(Outcome~ ., ntree = 140 , mtry = 2, nodesize = 7,  
                        importance = T, data=train)

pred_rf=predict(rf_model, newdata=test, type='prob')

cpred_rf = ifelse(pred_rf[,2] >= 0.5, 1, 0)

pl_rf=prediction(pred_rf[,2],test$Outcome)

acc.perf = performance(pl_rf,'acc','cutoff')
recall.perf = performance(pl_rf,'rec','cutoff')
precision.perf = performance(pl_rf,'prec','cutoff')
roc.perf = performance(pl_rf,'tpr','fpr')

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
  scale_x_continuous(breaks = c(0.1,0.2,0.3,0.8,1,cutoff),
                     labels = c(0.1,0.2,0.3,0.8,1,round(cutoff,5)))+
  scale_y_continuous(breaks=c(seq(0,0.5,0.1),acc_m),
                     labels = c(seq(0,0.5,0.1),round(acc_m,5)))


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




# confusion matrix 
confusionMatrix(as.factor(cpred_rf), test$Outcome, positive= '1') 

varImpPlot(rf_model, type=1)
varImpPlot(rf_model, type=2)

#########################################################################
#xgb
#mice
#grid search
d.xgb.mlr <- makeClassifTask(
  data=train, 
  target='Outcome'
)

d.xgb.mlr.test <- makeClassifTask(
  data=test, 
  target='Outcome'
)
param_grid_multi <-makeParamSet(
  # The number of trees in the model (each one built sequentially)
  makeIntegerParam("nrounds", lower = 100, upper = 200),
  # number of splits in each tree
#  makeIntegerParam("max_depth", lower = 1, upper = 10),
  # "shrinkage" - prevents overfitting
#  makeNumericParam("eta", lower = .1, upper = .5),
  # L2 regularization - prevents overfitting
  makeNumericParam("lambda", lower = -1, upper = 0, trafo = function(x) 10^x),
  makeNumericParam('subsample', lower=.2, upper=.7)
)


# Define Grid
control_grid = makeTuneControlGrid()
# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)
# Define Measure
measure = mmce

xgb_learner <- makeLearner(
  "classif.xgboost",
#  predict.type = "response",
  par.vals = list(
    objective = "binary:logistic"
 #   eval_metric = "error"
  )
)

dt_tuneparam_multi <- tuneParams(learner=xgb_learner, 
                                 task=d.xgb.mlr, 
                                 resampling = resample,
                                 measures = measure,
                                 par.set=param_grid_multi, 
                                 control=control_grid, 
                                 show.info = TRUE)

#Result: m nrounds=189; lambda=0.167; subsample=0.311 : mmce.test.mean=0.2364991

result_hyperparam <- generateHyperParsEffectData(dt_tuneparam_multi, partial.dep = TRUE)

ggplot(
  data = result_hyperparam$data,
  aes(x = k, y=acc.test.mean)
) + geom_line(color = 'darkblue')



#Model 생성
predictors = as.matrix(train[,-9])
train_label = as.numeric(train[,9])-1

dtrain = xgb.DMatrix(data = predictors, label= train_label)

xgb_model = xgboost(data = predictors, label = train_label,  nround = 189, params = list(lambda = 0.167,  subsample = 0.311), objective = "binary:logistic")



pred_xgb=predict(xgb_model, newdata=as.matrix(test[,-9]), type = 'response')

cpred_xgb = ifelse(pred_xgb >= 0.5, 1, 0)

# confusion matrix 
confusionMatrix(as.factor(cpred_xgb), test$Outcome, positive= '1') 


#시각화
importance_matrix <- xgb.importance(names(predictors), model = xgb_model)

# and plot it!
xgb.plot.importance(importance_matrix)

pl_xgb=prediction(pred_xgb, test$Outcome)

acc.perf = performance(pl_xgb,'acc','cutoff')
recall.perf = performance(pl_xgb,'rec','cutoff')
precision.perf = performance(pl_xgb,'prec','cutoff')
roc.perf = performance(pl_xgb,'tpr','fpr')

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
  scale_x_continuous(breaks = c(0.1,0.2,0.4,0.5,1,cutoff),
                     labels = c(0.1,0.2,0.4,0.5,1,round(cutoff,5)))+
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




################################################################
#ensemble

#soft voting
soft=(pred_naive$posterior[,2] + pred_glm + pred_dt[,2]  + pred_rf[,2] + pred_xgb)/5


soft=(pred_naive$posterior[,2] + pred_glm + pred_rf[,2] + pred_xgb)/4

pl_soft=prediction(soft,test$Outcome)

acc.perf = performance(pl_soft,'acc','cutoff')
recall.perf = performance(pl_soft,'rec','cutoff')
precision.perf = performance(pl_soft,'prec','cutoff')
roc.perf = performance(pl_soft,'tpr','fpr')

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
  scale_x_continuous(breaks = c(0.1,0.2,0.4,0.5,0.7,1,cutoff),
                     labels = c(0.1,0.2,0.4,0.5,0.7,1,round(cutoff,5)))+
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


#hard voting
cpred_naive = ifelse(pred_naive$posterior[,2] >= 0.66775, 1, 0)
cpred_glm = ifelse(pred_glm >= 0.43927, 1, 0)
cpred_dt = ifelse(pred_dt[,2] >= 0.60811, 1, 0)
cpred_rf = ifelse(pred_rf[,2] >= 0.44286, 1, 0)
cpred_xgb = ifelse(pred_xgb >= 0.78905, 1, 0)
cpred_svm = as.numeric(pred_svm) - 1

hard = cpred_naive + cpred_glm + cpred_dt  + 
  cpred_rf + cpred_xgb + cpred_svm

hard = cpred_naive + cpred_glm   + 
  cpred_rf + cpred_xgb + cpred_svm


cpred_hard = ifelse(hard >= 3, 1, 0)

confusionMatrix(as.factor(cpred_hard), test$Outcome, positive= '1')


#ROC 그래프 총정리
roc.naive = performance(pl_naive,'tpr','fpr')
roc.glm = performance(pl_glm,'tpr','fpr')
roc.dt = performance(pl_dt,'tpr','fpr')
roc.knn = performance(pl_knn,'tpr','fpr')
roc.rf = performance(pl_rf,'tpr','fpr')
roc.xgb = performance(pl_xgb,'tpr','fpr')
roc.soft = performance(pl_soft,'tpr','fpr')


ggplot()+
  geom_line(aes(slot(roc.naive, 'x.values')[[1]], slot(roc.naive, 'y.values')[[1]],
                colour='Naive'),
            size=1,
            alpha=.4)+
  geom_line(aes(slot(roc.glm, 'x.values')[[1]], slot(roc.glm, 'y.values')[[1]],
                colour='Logistic'),
            size=1,
            alpha=.4)+
  geom_line(aes(slot(roc.dt, 'x.values')[[1]], slot(roc.dt, 'y.values')[[1]],
                colour='Decision Tree'),
            size=1,
            alpha=.4)+
  geom_line(aes(slot(roc.knn, 'x.values')[[1]], slot(roc.knn, 'y.values')[[1]],
                colour='KNN'),
            size=1,
            alpha=.4)+
  geom_line(aes(slot(roc.rf, 'x.values')[[1]], slot(roc.rf, 'y.values')[[1]],
                colour='Random Forest'),
            size=1,
            alpha=.4)+
  geom_line(aes(slot(roc.xgb, 'x.values')[[1]], slot(roc.xgb, 'y.values')[[1]],
                colour='XGBoost'),
            size=1,
            alpha=.4)+
  geom_line(aes(slot(roc.soft, 'x.values')[[1]], slot(roc.soft, 'y.values')[[1]],
                colour='Ensemble'),
            size=1,
            alpha=.4)+
  geom_abline(slope = 1, lty=2)+
  xlab(slot(roc.naive, 'x.name'))+
  ylab(slot(roc.naive, 'y.name'))+
  scale_x_continuous(breaks=c(seq(0,1,0.2)),
                     labels = c(seq(0,1,0.2)))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)),
                     labels = c(seq(0,1,0.1)))+
  scale_colour_manual(breaks = c('Naive','Logistic','Decision Tree','KNN','Random Forest','XGBoost','Ensemble'),
                      values = c(1:7))



roc.naive = performance(pl_naive,'tpr','fpr')
roc.glm = performance(pl_glm,'tpr','fpr')
roc.dt = performance(pl_dt,'tpr','fpr')
roc.knn = performance(pl_knn,'tpr','fpr')
roc.rf = performance(pl_rf,'tpr','fpr')
roc.xgb = performance(pl_xgb,'tpr','fpr')
roc.soft


auc.naive = performance(pl_naive,'auc')
auc.glm = performance(pl_glm,'auc')
auc.dt = performance(pl_dt,'auc')
auc.knn = performance(pl_knn,'auc')
auc.rf = performance(pl_rf,'auc')
auc.xgb = performance(pl_xgb,'auc')
auc.soft = performance(pl_soft,'auc')

slot(auc.naive, 'y.values')
slot(auc.glm, 'y.values')
slot(auc.dt, 'y.values')
slot(auc.knn, 'y.values')
slot(auc.rf, 'y.values')
slot(auc.xgb, 'y.values')
slot(auc.soft, 'y.values')























