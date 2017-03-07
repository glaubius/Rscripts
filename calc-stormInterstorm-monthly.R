## Calculate mean storm and interstorm durations and mean depths on
## monthly intervals

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

## Convert month and year to columns
datDay$mo <- strftime(datDay$Group.1, "%m")
datDay$yr <- strftime(datDay$Group.1, "%Y")
colnames(datDay)<- c("fullDate", "meanPrecip", "month", "year")

