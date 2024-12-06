library(FNN)
library(gmodels)
library(kernlab)

#KNN
#mice
#grid search
d.knn.mlr <- makeClassifTask(
  data=train, 
  target='Outcome'
)

d.knn.mlr.test <- makeClassifTask(
  data=test, 
  target='Outcome'
)

param_grid_multi <- makeParamSet( 
  makeDiscreteParam('k', values=1:30)
)

# Define Grid
control_grid = makeTuneControlGrid()
# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)
# Define Measure
measure = mmce

dt_tuneparam_multi <- tuneParams(learner='classif.knn', 
                                 task=d.knn.mlr, 
                                 resampling = resample,
                                 measures = measure,
                                 par.set=param_grid_multi, 
                                 control=control_grid, 
                                 show.info = TRUE)

#Result: k=15 : acc.test.mean=

result_hyperparam <- generateHyperParsEffectData(dt_tuneparam_multi, partial.dep = TRUE)

ggplot(
  data = result_hyperparam$data,
  aes(x = k, y=mmce.test.mean)
) + geom_line(color = 'darkblue')

train_set=train
test_set=test
train_label=train[,'Outcome']

#Model 생성
knn_model = knn(train=train[-9], test=test[-9],cl = train_label, k=15, prob=T)

pred_knn=attr(knn_model, 'prob')

pl_knn=prediction(pred_knn,test$Outcome)

acc.perf = performance(pl_knn,'acc','cutoff')
recall.perf = performance(pl_knn,'rec','cutoff')
precision.perf = performance(pl_knn,'prec','cutoff')
roc.perf = performance(pl_knn,'tpr','fpr')

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
confusionMatrix(as.factor(knn_model), test$Outcome, positive= '1') 


############################################################################
#svm

#SVM 알고리즘에서 bias와 variance는 C의 값과 sigma^2에 의해서 조절이 가능합니다.

#C의 값이 크면 Lower bias, high variance가 되며, 값이 작으면 higher bias, low variance가 됩니다. C는 logistic regression에서의 lambda의 역수이며 비슷한 작용을 하게 됩니다.

#sigma^2은 f features의 경사도를 의미하며 이 값이 커지면 higher bias, lower variance가 되고 경사가 보다 부드럽게 퍼지는 형태가 됩니다. 또 이 값이 작으면 lower bias, higher variance가 되고 경사가 급하게 떨어지는 형태

#mice
#grid search
d.svm.mlr <- makeClassifTask(
  data=train, 
  target='Outcome'
)

d.svm.mlr.test <- makeClassifTask(
  data=test, 
  target='Outcome'
)

param_grid_multi <- makeParamSet(
  makeNumericParam("C", lower = -10, upper = 10, trafo = function(x) 10^x),
  makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 10^x)
)

# Define Grid
control_grid = makeTuneControlGrid()
# Define Cross Validation
resample = makeResampleDesc("CV", iters = 3L)
# Define Measure
measure = mmce

dt_tuneparam_multi <- tuneParams(learner='classif.ksvm', 
                                 task=d.svm.mlr, 
                                 resampling = resample,
                                 measures = measure,
                                 par.set=param_grid_multi, 
                                 control=control_grid, 
                                 show.info = TRUE)

#Result: C=12.9,  : sigma=0.000464 : mmce.test.mean=0.2234637

result_hyperparam <- generateHyperParsEffectData(dt_tuneparam_multi, partial.dep = TRUE)

ggplot(
  data = result_hyperparam$data,
  aes(x = C, y=acc.test.mean)
) + geom_line(color = 'darkblue')


#Model 생성


svm_model=ksvm(Outcome ~ .,kernel = 'rbfdot', kpar = list(sigma = 0.000464), C = 12.9,
     data = train)

pred_svm=predict(svm_model, newdata=test)

# confusion matrix 
confusionMatrix(pred_svm, test$Outcome, positive= '1')


























