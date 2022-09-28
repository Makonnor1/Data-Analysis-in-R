#install packages
install.packages("kernlab")
library(kernlab)

library(ggplot2)

install.packages('e1071')
library('e1071')

install.packages('gridExtra')
library(gridExtra)

#Step 1: Load the data
quality_air <-  airquality

#remove the NAs
quality_air <- na.omit(quality_air)

#Step 2: Create train and test data sets
#first create a randomized index
randIndex <- sample(1:dim(quality_air)[1])

#training data
cut_point_2_3 <- floor(2 * dim(quality_air)[1]/3)
train_data <- quality_air[randIndex[1:cut_point_2_3], ]

#test data
test_data <- quality_air[randIndex[(cut_point_2_3+1):dim(quality_air)[1]],]

#Step 3: Build a Model using KSVM & visualize the results

rmse <- function(error)
{
  sqrt(mean(error^2))
}

#1) Build a model (using the ‘ksvm’ function, trying to predict onzone). You can use all the possible attributes, or select the attributes that you think would be the most helpful.
svm_output <- ksvm(Ozone ~., data=train_data, kernel = "rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)

#2) Test the model on the testing dataset, and compute the Root Mean Squared Error
svm_pred <- predict(svm_output, test_data, type = "votes")

#error
ozone_error <- test_data$Ozone - svm_pred
rmse(ozone_error)

#3) Plot the results. Use a scatter plot. Have the x-axis represent temperature, the y-axis represent wind, the point size and color represent the error, as defined by the actual ozone level minus the predicted ozone level)
plot_1 <- ggplot(test_data, aes(x = Temp, y = Wind)) + geom_point(aes(size = ozone_error, color = ozone_error))

#4) Compute models and plot the results for ‘svm’ (in the e1071 package) and ‘lm’. Generate similar charts for each model
svm_2 <- svm(Ozone ~., data=train_data)

#prediction 
pred_svm_2 <- predict(svm_2, test_data)
pred_svm_2

#error
svm_2_error <- test_data$Ozone - pred_svm_2
rmse(svm_2_error)

#plot
plot_2 <- ggplot(test_data, aes(x = Temp, y = Wind)) + geom_point(aes(size = svm_2_error, color = svm_2_error))

#Linear Model
ozone_lm <- lm(Ozone ~ Solar.R + Wind + Temp, data = train_data)
pred_lm <- predict(ozone_lm, test_data, type = "response")
lm_error <- test_data$Ozone - pred_lm

#error
rmse(lm_error)

#plot linear model
plot_3 <- ggplot(test_data, aes(x = Temp, y = Wind)) + geom_point(aes(size = lm_error, color = lm_error))

#5) Show all three results (charts) in one window, using the grid.arrange function
grid.arrange(plot_1, plot_2, plot_3)

#Step 4: Create a ‘goodOzone’ variable
#This variable should be either 0 or 1. It should be 0 if the ozone is below the average for all the data observations, and 1 if it is equal to or above the average ozone observed.
avg_ozone <- mean(quality_air$Ozone)

#Add new column
goodOzone <- c()

for (i in 1:length(quality_air$Ozone)){
  if(quality_air$Ozone[i] < avg_ozone )
  {
    goodOzone <- append(goodOzone, 0)
  }
  else
  {
    goodOzone <- append(goodOzone, 1)
  }
}

#Create a new dataframe appending goodOzone
new_quality_air <- data.frame(quality_air, goodOzone)

#Step 5: See if we can do a better job predicting ‘good’ and ‘bad’ days
#Create random indexes for training and test data
randIndex_2 <- sample(1:dim(new_quality_air)[1])
goodOZ_cutpoint <- floor(2 * dim(new_quality_air)[1]/3)
train_data_2 <- new_quality_air[randIndex_2[1:goodOZ_cutpoint], ]
test_data_2 <- new_quality_air[randIndex_2[(goodOZ_cutpoint+1):dim(new_quality_air)[1]],]

#Build a model (using the ‘ksvm’ function, trying to predict ‘goodOzone’)
ksvm_output_goodOz <- ksvm(goodOzone ~., data=train_data_2, kernel = "rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)

#Test the ksvm model
ksvm_pred_goodOz <- predict(ksvm_output_goodOz, test_data_2)
ksvm_pred_goodOz <- round(ksvm_pred_goodOz)

#error
goodOz_error <- test_data_2$goodOzone - ksvm_pred_goodOz
rmse(goodOz_error)

#goodOZ plot
plot_4 <- ggplot(test_data_2, aes(x = Temp, y = Wind)) + geom_point(aes(size = goodOz_error, color = goodOzone, shape = as.factor(ksvm_pred_goodOz)))

#Compute the percent of ‘goodOzone’ that was correctly predicted.
res_table <- table(ksvm_pred_goodOz, test_data_2$goodOzone)
accuracy <- (res_table[1,1]+res_table[2,2])/(res_table[1,1]+res_table[1,2]+res_table[2,1]+res_table[2,2])*100

#Compute models and plot the results for ‘svm’ (in the e1071 package) and ‘nb’ (Naive Bayes, also in the e1071 package).
svm_3 <- svm(goodOzone ~., data=train_data_2)

#predict
pred_svm_3 <- predict(svm_3, test_data_2)
pred_svm_3 <- round(pred_svm_3)

#error
goodOz_error_2 <- test_data_2$goodOzone - pred_svm_3
rmse(goodOz_error_2)

#plot
plot_5 <- ggplot(test_data_2, aes(x = Temp, y = Wind)) + geom_point(aes(size = goodOz_error_2, color = goodOzone, shape = as.factor(pred_svm_3)))

#accuracy
res_table_2 <- table(pred_svm_3, test_data_2$goodOzone)
accuracy <- (res_table_2[1,1]+res_table_2[2,2])/(res_table_2[1,1]+res_table_2[1,2]+res_table_2[2,1]+res_table_2[2,2])*100

#Naive Bayes Model
goodOz_nb <- naiveBayes(as.factor(goodOzone) ~ ., train_data_2)
pred_NB <- predict(goodOz_nb, test_data_2)

#error
goodOz_error_3 <- test_data_2$goodOzone - as.numeric(pred_NB)
rmse(goodOz_error_3)

#NB plot
plot_6 = ggplot(data = test_data_2, aes(x = Temp, y = Wind)) + geom_point(aes(size = goodOz_error_3, col = goodOzone, shape = pred_NB))

#All 3 in a grid
grid.arrange(plot_4, plot_5, plot_6)

#Step 6: Which are the best Models for this data?
The SVM was the best model for this based on the RSME scores. SVM was the lowest at 0.1
