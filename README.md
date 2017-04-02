# OVERVIEW

The R scripts in this branch were used to analyze, prepare and model the data from the Kaggle Two Sigma Connect: Rental Listing Inquiries competition.
Applied in the following chronical order:

-	GamesHowellPostHoc
-	DataObservation
-	TrainDataPreparation
-	TestDataPreparation
-	FeatureImportance
-	xgboost
-	RandomForest
-	Stacking

# USAGE

The scripts should be executed in order of the overview to reproduce all of the steps described in the paper. Data observation includes the loading of the data.
The next packages should be installed:
*jsonlite, dplyr, purrr, gridExtra, ggplot2, ggmap, rjson, class, mlr, FSelector, xgboost, randomForest*

# SUMMARY OF SCRIPTS

**GamesHowellPostHoc**

Function to perform Games Howell Post Hoc test. Needed for data observation.

**DataObservation**

This script is about loading the train and test set into R, transform it into usable format and analyze for shape, outliers and correlation.

**TrainDataPreparation**

Prepare the raw train data with information gain of previous script into ready to use data for model input.
Removing outliers, attribute selection, attribute preparation and creation of features.

**TestDataPreparation**

Same procedure as TrainDataPreparation. Only with the raw test data and excluding the target feature.

**FeatureImportance**

Script which computes information gain and chi-square test of features with respect to the target feature.

**xgboost**

Script for optimizing parameters of gradient boosting including cross-validation. Training model with optimal parameters and predicting test set data.

**RandomForest**

Script for optimizing parameters of Random Forest including cross-validation. Training model with optimal parameters and predicting test set data.

**Stacking**

Ensemble of xgboost and Random Forest through averaging and stacking with hill climb algorithm.
Note: Hill climb is not used in the paper, because of time and max word count.
