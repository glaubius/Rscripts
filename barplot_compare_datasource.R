## create barplot to compare data used in analysis

## ONLY BEFORE FIRST ADDITION OF DATA
## create empty dataframe to hold mean and info
testData <- data.frame("dataType"=character(), "years"=integer(), "meanRMSE"=numeric(), "sdRMSE"=numeric(), "n"=integer(), stringsAsFactors=FALSE)

## After running calc_rmse, run this to calculate mean and sd of RMSEs
meanRMSE <- mean(rmse_values)
sdRMSE <- sd(rmse_values)

## Add values to dataframe
## need to change dataType and years for each addition
testData <- rbind(testData, data.frame("dataType"="DoD", "years"=1000, "meanRMSE"=meanRMSE, "sdRMSE"=sdRMSE, "n"=num_calcs))

## Once all values are added to dataframe, save to file for future use
write.table(testData, "fileName.txt", sep="\t")

## Calculate standard error (se)
testData$seRMSE <- testData$sdRMSE / sqrt(testData$n)

## construct barplot using ggplot2
## adapted from: https://www.r-bloggers.com/building-barplots-with-error-bars/
library(ggplot2)

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = testData$meanRMSE + testData$seRMSE, ymin = testData$meanRMSE - testData$seRMSE)

p <- ggplot(data = testData, aes(x = factor(years), y = meanRMSE, fill = factor(dataType)))

p + geom_bar(stat = "identity", position = position_dodge(0.9)) + 
  geom_errorbar(limits, position = position_dodge(0.9), width =0.25) + 
  labs(x = "Length of Simulation", y = "meanRMSE") + 
  ggtitle("mean RMSE by Simulation Length for Final DEM and DEM of Difference") +
  scale_fill_discrete(name = "Data Type")