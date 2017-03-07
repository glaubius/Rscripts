## Analyze TRMM precipitation data as a time series

## create list of files
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

## AGGREGATE DATA BY MONTH
datDay$mo <- strftime(datDay$Group.1, "%m")
datDay$yr <- strftime(datDay$Group.1, "%Y")

datMon <- aggregate(x ~ mo + yr, datDay, FUN = sum)

## read into time series vector with frequency 8 per day * 365.25 days in a year = 2922
#trmmTS <- ts(dat, start=c(2000, 1, 1), end=c(2015, 12, 31), frequency=2922)

## read into time series vector with frequency 365.25 (days per year)
#trmmDayTS <- ts(datDay, start=c(2000, 1, 1), end=c(2015, 12, 31), frequency=365)

## read into time series vector with frequency 12 (months)
trmmMonTS <- ts(datMon$x, start=c(2000, 1), end=c(2015, 12), frequency=12)

## plot time series to check that all is OK
plot(trmmMonTS)

## seasonal decomposition of data using stl (loess)
trmmtimeseriescomponents <- stl(trmmMonTS, s.window="period")
plot(trmmtimeseriescomponents)
seasonal <- trmmtimeseriescomponents$time.series[,1]
print(seasonal[1])
seasonalDat <- c(seasonal[1:12])
barplot(seasonalDat)


## seasonal decomposition of data using decopose()
trmm.timeseries <- decompose(trmmMonTS)
seasonalTrend <- (trmm.timeseries$seasonal)
print(seasonalTrend[1])
seasonTrendDat <- c(seasonalTrend[1:12])
barplot(seasonTrendDat)

