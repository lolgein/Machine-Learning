library(mlr)

train_data$interest_level = factor(train_data$interest_level)
trainTask = makeClassifTask(data = train_data, target = "interest_level")

# Start with Random Forest model
# Gets all parameter settings of random Forest
getParamSet("classif.randomForest")

# Makes the learner with specific parameters
rf = makeLearner("classif.randomForest", predict.type = "prob", par.vals = list(ntree = 2000, mtry = 3,nodesize=10))
rf$par.vals = list(
  importance = TRUE
)

# Set tunable parameters
# Randomize parameter search
rf_param = makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 1000),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

# Total of iteration, set at 100
rancontrol = makeTuneControlRandom(maxit = 100L)

# Cross-validation with 10 folds
set_cv = makeResampleDesc("CV",iters = 10L)

#Tuning parameters, set measure at logloss
rf_tune = tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol, measures = logloss)

# Creates learner with parameters from parameter search
rf.tree = setHyperPars(rf,par.vals = rf_tune$x)

rdesc = makeResampleDesc("CV", iters = 10L)
# Do cross validation with specific parameters to get all folds
r = resample(learner = rf.tree, resampling = rdesc, measures = logloss, task = trainTask)

#Build the Random Forest model 
rforest = train(rf.tree, trainTask)

#Make the predictions for the test data
rfprediction = predict(rforest, newdata = test_data)
rf_predict = rfprediction$data
rf_predict = rf_predict[,-4]
colnames(rf_predict) = c("high","medium","low")
