# Gradient Boosting (Ensemble of weak decisions trees) of the package xgboost
library(xgboost)

# Set parameters, seed, best logloss and index
best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0

mydataMatrix = data.matrix(train_data)
mydataMatrix[,15] = mydataMatrix[,15] - 1

dtrain = xgb.DMatrix(data = mydataMatrix[,1:14], label = mydataMatrix[,15],missing = NA)

# randomized parameter search
for (iter in 1:100) {
  param <- list(objective = "multi:softprob",
                eval_metric = "mlogloss",
                num_class = 3,
                max_depth = sample(4:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .5, .8),
                colsample_bytree = runif(1, .5, .9), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = 1
  )
  cv.nround = 1000
  cv.nfold = 4
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=dtrain, params = param, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early.stopping.rounds=8, maximize=FALSE)
  
  min_logloss = min(mdcv$evaluation[, test_mlogloss_mean])
  min_logloss_index = which.min(mdcv$evaluation[, test_mlogloss_mean])
  
  # save best parameter and iteration as index
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
  print(iter)
  print(best_logloss)
}

# Creates plot of cross validation results of model training
plot(mdcv$evaluation_log$iter,mdcv$evaluation_log$train_mlogloss_mean,type="l",col="red"
     ,xlab="Epochs",ylab = "logloss mean",
     main = "Cross-validation results of constructing model\n with 1000 iterations")
lines(mdcv$evaluation_log$iter,mdcv$evaluation_log$test_mlogloss_mean,col="green")
legend(850,1.0, c("Test","Train"),lty=c(1,1),lwd=c(2.5,2.5),col=c("green","red"))

train_data$interest_level = factor(train_data$interest_level)
train_data$neighborhoods = as.numeric(train_data$neighborhoods)
trainTask <- makeClassifTask(data = train_data, target = "interest_level")

#Get cross-validation values of finalised model, because not possible with only gxboost package
xg_set <- makeLearner("classif.xgboost", predict.type = "prob")
bbb <- list(
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  nrounds = best_logloss_index,
  max_depth = best_param$max_depth,
  eta = best_param$eta,
  gamma = best_param$gamma,
  subsample = best_param$subsample,
  colsample_bytree = best_param$colsample_bytree,
  min_child_weight = best_param$min_child_weight,
  max_delta_step = best_param$max_delta_step
)
xg_new <- setHyperPars(learner = xg_set, par.vals = bbb)

rdesc = makeResampleDesc("CV", iters = 10L)
# Do cross validation with specific parameters to get all folds
r = resample(learner = xg_new, resampling = rdesc, measures = logloss, task = trainTask)


# Train the model with the best parameters
nround = best_logloss_index
set.seed(best_seednumber)
md <- xgb.train(data=dtrain, params=best_param, nrounds=nround, nthread=6)

# Prediction of the test_data
xgboost_pred = matrix(predict(md,data.matrix(test_data)),ncol=3,byrow = T)
colnames(xgboost_pred) = c("high","medium","low")
