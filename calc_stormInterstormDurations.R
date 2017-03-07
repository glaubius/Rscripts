## Calculate storm and interstorm durations

## AGGREGATE DATA BY DAY
datDay <- aggregate(x = dat$meanPrecip, by = list(dat$DateTime), FUN = sum)

## ONCE ALL CSV FILES ARE ADDED IF NOT AGGREGATING BY DAY
## create datetime column
#dat$datetime <- paste(dat$system.time_start, c("0-3", "3-6", "6-9", "9-12", "12-15", "15-18",
#                                               "18-21", "21-24"))
num_records <- length(dat$DateTime)

## CREATE STORM and INTERSTORM DATAFRAMEs
stormEvents <- data.frame("event"=character(), "numPeriods"=integer(), 
                          "stormDepth"=numeric(), stringsAsFactors = FALSE)
interstormEvents <- data.frame("event"=character(), "numPeriods"=integer(), 
                               stringsAsFactors = FALSE)

## create counters
storm = 0
stormDepth = 0.0
interstorm = 0
precipThreshold = 0.25

## POPULATE durations dataframe by looping, interstorm duration minimum of 3 hours
for (i in 1:num_records){
  if(dat$meanPrecip[i] < precipThreshold){
    if(interstorm == 0){
        stormEvents <- rbind(stormEvents, data.frame("event"="storm", 
                                                     "numPeriods"=storm, 
                                                     "stormDepth"=stormDepth))
        storm = 0
        stormDepth = 0.0
        interstorm <- interstorm + 1
    }else{
      interstorm <- interstorm + 1
    }
  }else{
    if(storm == 0){
      interstormEvents <- rbind(interstormEvents, data.frame("event"="interstorm", 
                                                             "numPeriods"=interstorm))
      interstorm = 0
      storm <- storm + 1
      stormDepth <- stormDepth + (dat$meanPrecip[i] * 3)
    }else{
      storm <- storm + 1
      stormDepth <- stormDepth + (dat$meanPrecip[i] * 3)
    }
  }
}

## POPULATE durations dataframe by looping, interstorm duration minimum of 6 hours
for (i in 1:num_records){
  if(dat$meanPrecip[i] < precipThreshold){
    if(interstorm == 0){
      if(dat$meanPrecip[i + 1] < precipThreshold){
        stormEvents <- rbind(stormEvents, data.frame("event"="storm", 
                                                     "numPeriods"=storm, 
                                                     "stormDepth"=stormDepth))
        storm = 0
        stormDepth = 0.0
        interstorm <- interstorm + 1
      }else{
        storm <- storm + 1
        stormDepth <- stormDepth + (dat$meanPrecip[i] * 3)
      }
      
    }else{
      interstorm <- interstorm + 1
    }
  }else{
    if(storm == 0){
      interstormEvents <- rbind(interstormEvents, data.frame("event"="interstorm", 
                                                             "numPeriods"=interstorm))
      interstorm = 0
      storm <- storm + 1
      stormDepth <- stormDepth + (dat$meanPrecip[i] * 3)
    }else{
      storm <- storm + 1
      stormDepth <- stormDepth + (dat$meanPrecip[i] * 3)
    }
  }
}

# Add last event to proper dataframe
if(dat$meanPrecip[num_records] < precipThreshold){
  interstormEvents <- rbind(interstormEvents, data.frame("event"="interstorm", 
                                                         "numPeriods"=interstorm))
}else{
  stormEvents <- rbind(stormEvents, data.frame("event"="storm", "numPeriods"=storm, 
                                               "stormDepth"=stormDepth))
}

## delete first record which is spurious from record of type of event that occurs second
stormEvents <- stormEvents[-1,]

## Calculate Time from numPeriods
interstormEvents$timeDuration <- interstormEvents$numPeriods * 3
stormEvents$timeDuration <- stormEvents$numPeriods * 3

## Calculate mean duration for storm and interstorm periods
meanStormDuration <- mean(stormEvents$timeDuration)
meanStormDepth <- mean(stormEvents$stormDepth)
meanInterstormDuration <- mean(interstormEvents$timeDuration)

## Calculate mean duration for storm and interstorm periods in years
meanStormDurationYr <- meanStormDuration / 8765.82
meanInterstormDurationYr <- meanInterstormDuration / 8765.82

## find minimum time between sucessive rainfall events for them to be independent
## Hawk 1992, p. 31, eq. 3-3
CV <- function(interstormEvents){
  sd(interstormEvents)/pexp(1, rate=1/interstormEvents)
}
cv <- CV(meanInterstormDuration)

## CALCULATE MEAN_STORM_DEPTH
rainTimes <- dat[ which(dat$meanPrecip!=0.0), ]
meanStormDepth <- mean(rainTimes$meanPrecip * 3)

## PLOT DISTRIBUTION OF STORM/INTERSTORM Periods
stormCounts <- table(stormEvents$numPeriods)
stormBarPlot <- barplot(stormCounts, main="Storm Period Distribution", xlab="Storm Duration")

interstormCounts <- table(interstormEvents$numPeriods)
interstormBarPlot <- barplot(interstormCounts, main="Interstorm Period Distribution", 
                             xlab="Interstorm Duration")
