#install curl package
install.packages("curl")
library(curl)

#install jsonlite package
install.packages("jsonlite")
library(jsonlite)

#Step 1: Load the data
#Read in the following JSON dataset
accidentURL <- 'http://opendata.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD'
apiResult <- curl(accidentURL)
results <- fromJSON(apiResult)
length(results)

#Step 2: Clean the data
#separate into metadata and actual data
acc_data <- results[[2]]

#remove the first 8 unwanted columns
acc_data <- acc_data[, -(1:8)]

#convert the list of data into a dataframe
accidentData <- data.frame(acc_data)

#inspect
head(accidentData, 15)

#create column header
namesOfColumns <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLI SION_WITH_1","COLLISION_WITH_2")

#assign names to accident dataset and inspect
names(accidentData) <- namesOfColumns
head(accidentData, 15)

#Step 3: Understand the data using SQL (via SQLDF)
#install sqldf package
install.packages("sqldf")
library(sqldf)

#install another library plyr to use the count function
install.packages("plyr")
library(plyr)

#remove space and turn into a factor
accidentData$DAY_OF_WEEK <- gsub('\\s+','',accidentData$DAY_OF_WEEK)
accidentData$DAY_OF_WEEK<-as.factor(accidentData$DAY_OF_WEEK)

#How many accidents happen on SUNDAY
sqldf("SELECT COUNT(*) FROM accidentData WHERE DAY_OF_WEEK == 'SUNDAY'")

#How many accidents had injuries (might need to remove NAs from the data)
sqldf("SELECT COUNT(*) FROM accidentData WHERE INJURY == 'YES'")

#List the injuries by day
sqldf("SELECT DAY_OF_WEEK, COUNT(*) FROM accidentData WHERE INJURY == 'YES' GROUP BY DAY_OF_WEEK")

#Step 4: Understand the data using tapply
#Answer the following questions (same as before) – compare results:

#How many accidents happen on Sunday
tapply(accidentData$DAY_OF_WEEK, accidentData$DAY_OF_WEEK == "SUNDAY", count)

#How many accidents had injuries (might need to remove NAs from the data)
tapply(accidentData$INJURY, accidentData$INJURY == "YES", count)

#List the injuries by day
tapply(accidentData$DAY_OF_WEEK, accidentData$INJURY == "YES", count)














