#STEP 1
#install moments package for the "skewness" function
install.packages("moments")
library(moments)

#Write a summarizing function to understand the distribution of a vector
printVecInfo <- function(vec.x) {
  cat('Mean:', mean(vec.x),  '\n')
  cat('Median:', median(vec.x), '\n')
  cat('Min: ', min(vec.x))
  cat(' Max:', max(vec.x), '\n')
  cat('sd:', sd(vec.x), '\n')
  cat('quantile:', quantile(vec.x, probs = c(0.05, 0.95)), '\n')
  cat('skewness:', skewness(vec.x), '\n')
}

#Create a test vector
test.v <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 50)

#test function
printVecInfo(test.v)

#STEP 2
#Creating Samples in a Jar
marbles <- c("red", "blue")

#Create a variable ‘jar’ that has 50 red and 50 blue marbles
jar <- rep(marbles, 50)

#Confirm there are 50 reds by summing the samples that are red
length(which(jar == "red"))

#Sample 10 ‘marbles’ (really strings) from the jar. How many are red? What was the percentage of red marbles?
m.samples <- sample(jar, 10, replace=TRUE)
percent.red <-length(which(m.samples =="red")) / 10

#step 7
m.s.twenty <- replicate(20, length(which((sample(jar, 10, replace = TRUE)) == 'red'))/ 10)

# Use your printVecInfo to see information of the samples
printVecInfo(m.s.twenty)

#Also generate a histogram of the samples
hist(m.s.twenty)

#Repeat #7, but this time, sample the jar 100 times
m.s.hundred <- replicate(20, length(which((sample(jar, 100, replace = TRUE)) == 'red'))/ 100)

# Use your printVecInfo to see information of the samples
printVecInfo(m.s.hundred)

#Also generate a histogram of the samples
hist(m.s.hundred)

#Repeat #8, but this time, replicate the sampling 100 times
m.s.hundredtimes <- replicate(100, length(which((sample(jar, 100, replace = TRUE)) == 'red'))/ 100)

# Use your printVecInfo to see information of the samples
printVecInfo(m.s.hundredtimes)

#Also generate a histogram of the samples
hist(m.s.hundredtimes)

#STEP 3: Explore the airquality dataset
qualityair <- airquality
summary(qualityair)
sum(is.na(qualityair))

#remove NAs
qualityair <- na.omit(qualityair)

#Explore Ozone, Wind and Temp by doing a ‘printVecInfo’ on each as well as generating a histogram for each
#Ozone
printVecInfo(qualityair$Ozone)

hist(qualityair$Ozone)

#Wind
printVecInfo(qualityair$Wind)

hist(qualityair$Wind)

#Temp
printVecInfo(qualityair$Temp)

hist(qualityair$Temp)
