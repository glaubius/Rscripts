# Read in generated storm and interstorm durations, starts with storm duration
datDuration <- read.csv("StormEventDuration-thr-3hr-3779.txt", header=FALSE)
datDepth <- read.csv("StormEventRainRate-thr-3hr-3779.txt", header=FALSE)

dat <- data.frame(cbind(datDuration, datDepth))
colnames(dat) <- c("Duration", "Depth")

# Generate Storm and Interstorm dataframes
numEvents <- length(dat$Duration)
stormEvents <- data.frame("Duration"=numeric(), "Depth"=numeric(), stringsAsFactors = FALSE) 
interstormEvents <- data.frame("Duration"=numeric(), "Depth"=numeric(), 
                               stringsAsFactors = FALSE) 

for (i in 1:numEvents){
  if(dat$Depth[i] > 0){
    stormEvents <- rbind(stormEvents, data.frame("Duration"=dat$Duration[i], 
                                                 "Depth"=dat$Depth[i]))
  }else{
    interstormEvents <- rbind(interstormEvents, data.frame("Duration"=dat$Duration[i], 
                                                           "Depth"=dat$Depth[i]))
  }
}

# Calculate mean Durations and Depth
meanStormDepth <- mean(stormEvents$Depth)
meanStormDuration <- mean(stormEvents$Duration)
meanInterstormDuration <- mean(interstormEvents$Duration)
