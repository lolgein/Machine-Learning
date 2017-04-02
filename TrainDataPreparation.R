#Removed mostly points outside new york and zero latitudes and longitudes
trainData = trainData[trainData$latitude>40,]
trainData = trainData[trainData$latitude<42,]
trainData = trainData[trainData$longitude>-80,]

# Transform interest levels to numeric factors, 0=high,1=medium,2=low
final_data = factor(trainData$interest_level,levels= c("high","medium","low"))
final_data = as.numeric(final_data)-1
final_data = as.data.frame(final_data)

# create new columns bathrooms and toilet(binary)
baths = trainData$bathrooms
bathsRound = floor(baths)

toilet = baths - bathsRound
toiletRound = ceiling(toilet)
trainData = cbind(trainData,bathsRound)

# add numeric values
final_data = cbind(trainData$bedrooms,bathsRound,toiletRound,log(trainData$price),trainData$latitude,trainData$longitude,final_data)

colnames(final_data) = c("bedrooms","bathrooms","toilet","logPrice","latitude","longitude","interest_level")

# Make some numeric values of non-numeric values
# Everything is in year 2016, thus exclude year
dayNumerics = as.numeric(substr(trainData$created,9,10))
hourNumerics = as.numeric(substr(trainData$created,12,13))
descriptionSumWords = sapply(strsplit(trainData$description, "\\s+"), length)
sumFeatures = as.vector(sapply(trainData$features,length))
sumPhotos = as.vector(sapply(trainData$photos,length))

final_data = cbind(dayNumerics,hourNumerics,descriptionSumWords,sumFeatures,sumPhotos,final_data)

# Here we get the centre coordinates of regions in New York. 
# The corresponding coordinates which lie closest to certain centre regions are listed in that region
# This is a way to allocate regions to coordinates with nearest neighbors
library(ggplot2)
library(ggmap)
library(rjson)
library(class)

df_coords = data.frame(lat=trainData$latitude, 
                        long=trainData$longitude, 
                        ID=c(1:length(trainData[[1]])))

# Here I listed regions from Renthop search option

m_neighborhoods = c("Downtown Manhattan", "Midtown Manhattan", "Upper Manhattan")

b_neighborhoods = c("Northern Brooklyn", "Northwestern Brooklyn", "Eastern Brooklyn",
                     "South Brooklyn", "Southwestern Brooklyn", "Southeastern Brooklyn")

q_neighborhoods = c("Northwestern Queens", "Northeastern Queens", "Southeastern Queens", "Southwestern Queens", "Rockaway Peninsula")

s_neighborhoods = c("West Bronx", "East Bronx")


bx_neighborhoods = c("Jersey City")

nj_neighborhoods = c("Staten island")

# Get coordinates from google with geocode
getCoords = function(neighborhoods){  
  num_n = length(neighborhoods)
  if (neighborhoods[1]=="Newark"){
    neighborhoods = paste0(neighborhoods, ", NJ")
  } else {
    neighborhoods = paste0(neighborhoods, ", NY")
  }
  lat = rep(0, num_n)
  lon = rep(0, num_n)
  
  for(i in 1:num_n){
    n = neighborhoods[i]
    reply = suppressMessages(geocode(n)) 
    lat[i] = reply$lat
    lon[i] = reply$lon
  }
  
  return(data.frame(n=neighborhoods, lat=lat, lon=lon))
}

X = do.call("rbind", list(getCoords(m_neighborhoods), getCoords(b_neighborhoods), 
                           getCoords(q_neighborhoods), getCoords(s_neighborhoods),
                           getCoords(bx_neighborhoods), getCoords(nj_neighborhoods)))

# Perform 1-NN
neighborhoods = knn(X[, c("lat", "lon")], df_coords[, c(1,2)], X$n, k = 1)


neighborhoods = as.factor(neighborhoods)
final_data = cbind(neighborhoods,final_data)


# Using regions to look for cheap appartments inside region
# used median because of skewed data
# negative means cheaper appartment in comparison with median region price
# Using price divided by median price per bedroom
rr = cbind(seq(1,length(trainData[,1]),1), trainData$price, as.factor(trainData$bedrooms))
colnames(rr) = c("seq", "price", "bedroom")
g = aggregate(rr[,2] ~ rr[,3], data = rr, median)
dd = merge(rr,g, by.x = "bedroom", by.y = "rr[, 3]")
dd = dd[order(dd$seq),]
dd_ratio_bed = cbind(dd$seq, dd$price/dd$`rr[, 2]`)
colnames(dd_ratio_bed) = c("seq","ratio_bed")
dd_ratio_bed = as.data.frame(dd_ratio_bed)
ratio_bed = dd_ratio_bed$ratio_bed

rr = cbind(seq(1,length(trainData[,1]),1), trainData$price, as.factor(trainData$bathsRound))
colnames(rr) = c("seq", "price", "bathrooms")
g = aggregate(rr[,2] ~ rr[,3], data = rr, median)
dd = merge(rr,g, by.x = "bathrooms", by.y = "rr[, 3]")
dd = dd[order(dd$seq),]
dd_ratio_bath = cbind(dd$seq, dd$price/dd$`rr[, 2]`)
colnames(dd_ratio_bath) = c("seq","ratio_bath")
dd_ratio_bath = as.data.frame(dd_ratio_bath)
ratio_bath = dd_ratio_bath$ratio_bath

train_data = cbind(ratio_bath,ratio_bed,final_data)

write.csv(train_data,file="train_data.csv", row.names = F)
