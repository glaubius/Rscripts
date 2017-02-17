## Calculate storm and interstorm durations

## CREATE LIST OF FILES
rain_files <- list.files(path=".", pattern="TRMM*")
num_files <- length(rain_files)

## CREATE DATASET
dat <- read.csv(rain_files[1], header=TRUE)

## append csv files as needed
for(t in 2:num_files){
  dat <- rbind(dat, read.csv(rain_files[t], header=TRUE))
}

## CONVERT DATE String to numerical
dat$Date <- as.Date(dat$system.time_start, format = "%B %d, %Y")
colnames(dat) <- c("oldSystemDate", "meanPrecip", "DateTime")

## AGGREGATE DATA BY DAY
datDay <- aggregate(x = dat$meanPrecip, by = list(dat$DateTime), FUN = sum)

## ONCE ALL CSV FILES ARE ADDED IF NOT AGGREGATING BY DAY
## create datetime column
#dat$datetime <- paste(dat$system.time_start, c("0-3", "3-6", "6-9", "9-12", "12-15", "15-18",
#                                               "18-21", "21-24"))
num_records <- length(dat$DateTime)

## CREATE STORM and INTERSTORM DATAFRAMEs
stormEvents <- data.frame("event"=character(), "numPeriods"=integer(), stringsAsFactors = FALSE)
interstormEvents <- data.frame("event"=character(), "numPeriods"=integer(), stringsAsFactors = FALSE)

## create counters
storm = 0
interstorm = 0

## POPULATE durations dataframe by looping
for (i in 1:num_records){
  if(dat$meanPrecip[i] == 0){
    if(interstorm == 0){
      stormEvents <- rbind(stormEvents, data.frame("event"="storm", "numPeriods"=storm))
      storm = 0
      interstorm <- interstorm + 1
    }else{
      interstorm <- interstorm + 1
    }
  }else{
    if(storm == 0){
      interstormEvents <- rbind(interstormEvents, data.frame("event"="interstorm", "numPeriods"=interstorm))
      interstorm = 0
      storm <- storm + 1
    }else{
      storm <- storm + 1
    }
  }
}

if(dat$meanPrecip[num_records] == 0){
  interstormEvents <- rbind(interstormEvents, data.frame("event"="interstorm", "numPeriods"=interstorm))
}else{
  stormEvents <- rbind(stormEvents, data.frame("event"="storm", "numPeriods"=storm))
}

## delete first record which is spurious from record of type of event that occurs second
stormEvents <- stormEvents[-1,]

## Calculate Time from numPeriods
interstormEvents$timeDuration <- interstormEvents$numPeriods * 3
stormEvents$timeDuration <- stormEvents$numPeriods * 3

## Calculate mean duration for storm and interstorm periods
meanStormDuration <- mean(stormEvents$timeDuration)
meanInterstormDuration <- mean(interstormEvents$timeDuration)

## CALCULATE MEAN_STORM_DEPTH
rainTimes <- dat[ which(dat$meanPrecip!=0.0), ]
meanStormDepth <- mean(rainTimes$meanPrecip * 3)

## PLOT DISTRIBUTION OF STORM/INTERSTORM Periods
stormCounts <- table(stormEvents$numPeriods)
stormBarPlot <- barplot(stormCounts, main="Storm Period Distribution", xlab="Storm Duration")

interstormCounts <- table(interstormEvents$numPeriods)
interstormBarPlot <- barplot(interstormCounts, main="Interstorm Period Distribution", xlab="Interstorm Duration")
