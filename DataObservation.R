# Load data and extract to data frame
packages <- c("jsonlite", "dplyr", "purrr")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

json_fileTest <- fromJSON("test.json")
json_fileTrain <- fromJSON("train.json")

vars <- setdiff(names(json_fileTrain), c("photos", "features"))
trainData <- map_at(json_fileTrain, vars, unlist) %>% tibble::as_tibble(.)
vars <- setdiff(names(json_fileTest), c("photos", "features"))
testData <- map_at(json_fileTest, vars, unlist) %>% tibble::as_tibble(.)

# Observe training data by plots
# Numerical: bathroom, bedrooms, latitude, longitude, price
# Categorical: building_id, listing_id, manager_id, display_address
# List and texts: description, features, photos
# Date: created

plot(as.factor(trainData$interest_level))
# Distribution of target variable is not uniform distributed

hist(trainData$bathrooms)
boxplot(trainData$bathrooms)
summary(trainData$bathrooms)
# mean is 1.2 bathrooms. Some outliers (max 10 bathrooms)

hist(trainData$bedrooms)
boxplot(trainData$bedrooms)
summary(trainData$bedrooms)
# mean is 1.5. Also some outliers but fewer than previous case (max 8 bedrooms)

hist(trainData$latitude)
boxplot(trainData$latitude)
summary(trainData$latitude)
# Almost all points in 40 range. Some outliers (one with 0!!) is this in New York?

hist(trainData$longitude)
boxplot(trainData$longitude)
summary(trainData$longitude)
# Also outliers including zero

trainData = trainData[trainData$latitude>40,]
trainData = trainData[trainData$latitude<42,]
trainData = trainData[trainData$longitude>-80,]
#Removed mostly points outside new york

hist(trainData$price)
# Ofcourse very right skewed. Log transformation can help with readability
hist(log(trainData$price))
plot(as.factor(trainData$interest_level),log(trainData$price))

library(ggplot2)

# Looks like correlation between high price-low interest vs low price-high interest. (do not know if it is significant)
par(mfrow=c(1,2))
ggplot(trainData, aes(x=as.factor(trainData$interest_level), y=log(trainData$price)))+ geom_boxplot() + theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank()) + labs(title="Boxplots of log(price) per interest level") + ylab("Log(price)")

vv = trainData[trainData$price<30000,]
hist(vv$price, main = "Histogram of\n rental price", xlab = "Price (dollar)")

boxplot(log(trainData$price)~trainData$interest_level, main = "Boxplots of log(price)\n per interest", ylab="Log(price)")

# Creates ANOVA test of price per interest level
plant = lm(log(trainData$price)~trainData$interest_level)
anova(plant)

# Post hoc test Games-Howell test to look which groups differ significantly from each other.
# Every group differ significant from each other. Thus, excellent variable to implement in model
posthoc.tgh(y=log(trainData$price),x=trainData$interest_level, method = "games-howell")

library(gridExtra)

bbb = factor(trainData$interest_level, levels= c("high","medium","low"))
# Interesting correlation. Low prices for same bedrooms quantity gives higher interest
p1 = ggplot(trainData, aes(x=as.factor(trainData$bedrooms), y = log(trainData$price), fill=factor(trainData$interest_level, levels= c("high","medium","low")))) + geom_boxplot() + theme(legend.title = element_blank(), legend.justification = c(1,0),legend.position = c(1,0), plot.title = element_text(hjust = 0.5)) + ggtitle("Log(Price) per bedroom and interest") + labs(x="Bedrooms",y="Log(Price)") + scale_fill_grey(start = 0.4,end = 1)

# Same behaviour. Also interesting. bedrooms and bathrooms quantity above certain value only gives low interest.
p2 = ggplot(trainData, aes(x=as.factor(trainData$bathsRound), y = log(trainData$price), fill=factor(trainData$interest_level, levels= c("high","medium","low")))) + geom_boxplot() + theme(legend.title = element_blank(), legend.justification = c(1,0),legend.position = c(1,0), plot.title = element_text(hjust = 0.5)) + ggtitle("Log(price) per bathroom and interest") + labs(x="Bathrooms",y="Log(Price)") + scale_fill_grey(start = 0.4,end = 1)

grid.arrange(p1,p2,ncol=2)

grid.arrange(arrangeGrob(p1,p2,ncol = 2))

# Boxplots of log ratio price/median per region per bedrooms quantity. Implement anova and Games-Howell Test
ggplot(dataSet2, aes(x=as.factor(dataSet2$interest_level), y = log(dataSet2$dd_ratio_bedRegion), fill=as.factor(dataSet2$interest_level))) + geom_boxplot() + theme_bw()

ggplot(dataSet2, aes(x=as.factor(dataSet2$interest_level), y = log(dataSet2$bathRegion), fill=as.factor(dataSet2$interest_level))) + geom_boxplot() + theme_bw()

# Accept alternative hypothesis. At least two groups differ significantly
plant = lm(dataSet2$dd_ratio_bedRegion~as.factor(dataSet2$interest_level))
anova(plant)

# Post hoc test Games-Howell test to look which groups differ significantly from each other.
# Every group differ significant from each other. Thus, excellent variable to implement in model
posthoc.tgh(y=log(dataSet2$bathRegion),x=dataSet2$interest_level)
