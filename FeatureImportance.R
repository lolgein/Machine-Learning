library(mlr)
library(FSelector)

# Look at information gain and chi squared for all features with respect to the target
im_feat = generateFilterValuesData(trainTask,method = c("information.gain","chi.squared"))
plotFilterValues(im_feat)

