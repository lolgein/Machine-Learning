# create new columns bathrooms and toilet(binary)
baths = testData$bathrooms
bathsRound = floor(baths)

toilet = baths - bathsRound
toiletRound = ceiling(toilet)
testData = cbind(testData,bathsRound)

# add numeric values
test_data = cbind(testData$bedrooms,bathsRound,toiletRound,log(testData$price),testData$latitude,testData$longitude,test_data)

test_data = as.data.frame(test_data)

colnames(test_data) = c("bedrooms","bathrooms","toilet","logPrice","latitude","longitude")

# Make some numeric values of non-numeric values
# Everything is in year 2016, thus exclude year
dayNumerics = as.numeric(substr(testData$created,9,10))
hourNumerics = as.numeric(substr(testData$created,12,13))
descriptionSumWords = sapply(strsplit(testData$description, "\\s+"), length)
sumFeatures = as.vector(sapply(testData$features,length))
sumPhotos = as.vector(sapply(testData$photos,length))

test_data = cbind(dayNumerics,hourNumerics,descriptionSumWords,sumFeatures,sumPhotos,test_data)

# Here we get the centre coordinates of regions in New York. 
# The corresponding coordinates which lie closest to certain centre regions are listed in that region
# This is a way to allocate regions to coordinates with nearest neighbors

df_coords <- data.frame(lat=testData$latitude, 
                        long=testData$longitude, 
                        ID=c(1:length(testData[[1]])))

# Perform 1-NN
neighborhoods <- knn(X[, c("lat", "lon")], df_coords[, c(1,2)], X$n, k = 1)

neighborhoods = as.factor(neighborhoods)
test_data = cbind(neighborhoods,test_data)


# Using regions to look for cheap appartments inside region
# used median because of skewed data
# negative means cheaper appartment in comparison with median region price
# Using price divided by median price per bedroom
rr = cbind(seq(1,length(testData[,1]),1), testData$price, as.factor(testData$bedrooms))
colnames(rr) = c("seq", "price", "bedroom")
g = aggregate(rr[,2] ~ rr[,3], data = rr, median)
dd = merge(rr,g, by.x = "bedroom", by.y = "rr[, 3]")
dd = dd[order(dd$seq),]
dd_ratio_bed = cbind(dd$seq, dd$price/dd$`rr[, 2]`)
colnames(dd_ratio_bed) = c("seq","ratio_bed")
dd_ratio_bed = as.data.frame(dd_ratio_bed)
ratio_bed = dd_ratio_bed$ratio_bed

rr = cbind(seq(1,length(testData[,1]),1), testData$price, as.factor(testData$bathsRound))
colnames(rr) = c("seq", "price", "bathrooms")
g = aggregate(rr[,2] ~ rr[,3], data = rr, median)
dd = merge(rr,g, by.x = "bathrooms", by.y = "rr[, 3]")
dd = dd[order(dd$seq),]
dd_ratio_bath = cbind(dd$seq, dd$price/dd$`rr[, 2]`)
colnames(dd_ratio_bath) = c("seq","ratio_bath")
dd_ratio_bath = as.data.frame(dd_ratio_bath)
ratio_bath = dd_ratio_bath$ratio_bath

test_data = cbind(ratio_bath,ratio_bed,test_data)

write.csv(test_data,file="test_data.csv", row.names = F)
