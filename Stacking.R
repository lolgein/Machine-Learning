# Trained models after best parameter selection
xg_new
rf.tree

base.learners = list(xg_new,rf.tree)

# Stack base learners(gradient boosting and random forest) with method average and hill climb algorithm
stacked = makeStackedLearner(base.learners = base.learners, predict.type = "prob",
                             method = "average")

stackedHC = makeStackedLearner(base.learners = base.learners, predict.type = "prob",
                                method = "hill.climb")


rdesc = makeResampleDesc("CV", iters = 10L)
# Do cross validation with specific parameters to get all folds for ensemble
CrossStacked = resample(learner = stacked, resampling = rdesc, measures = logloss, task = trainTask)
CrossStackedHC = resample(learner = stackedHC, resampling = rdesc, measures = logloss, task = trainTask)

# train ensemble models with train data
AverageModel = train(CrossStacked, trainTask)
HCmodel = train(stackedHC, trainTask)
