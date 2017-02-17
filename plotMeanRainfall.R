## Plot mean rainfall by simulation conditions
## Storm Generator Test

## ONLY BEFORE FIRST ADDITION OF DATA
## create empty dataframe to hold mean and info
plotData <- data.frame("test"=character(), "meanRain"=numeric(), "sdRain"=numeric(), "n"=integer(), stringsAsFactors=FALSE)

## read in rainfall data from all simulations in test
rain_files <- list.files(path = "./yes-at", pattern = "Rainfall-*")
num_files <- length(rain_files)
rainData <- list()

for(i in 1:num_files) {
  read_file <- paste0("./yes-at/", rain_files[i])
  rain <- read.table(read_file, header=FALSE)
  rainData <- rbind(rainData, rain)
}

## calculate mean and sd of rainfall, number of rain observations, then add to dataframe
meanRain <- mean(rainData[[1]])
sdRain <- sd(rainData[[1]])
num_obs <- dim(rainData)

plotData <- rbind(plotData, data.frame("test"="yes-at", "meanRain"=meanRain, "sdRain"=sdRain, "n"=num_obs[1]))

## Calculate standard error (se) once all tests are in plotData
plotData$seRain <- plotData$sdRain / sqrt(plotData$n)

## construct barplot using ggplot2
## adapted from: https://www.r-bloggers.com/building-barplots-with-error-bars/
library(ggplot2)

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = plotData$meanRain + plotData$seRain, ymin = plotData$meanRain - plotData$seRain)

p <- ggplot(data=plotData, aes(x = test, y = meanRain, fill = test))

p + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank())



