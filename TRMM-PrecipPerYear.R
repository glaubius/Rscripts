## Produce a plot with total precipitation per year from
## TRMM data downloaded from GEE

## create list of files
rain_files <- list.files(path=".", pattern="TRMM*")
num_files <- length(rain_files)

## create dataframe with total annual rainfall
totalAnnualPrecip <- rep(NA, num_files)

for(i in 1:num_files){
  totalPrecip <- 0
  dat <- read.csv(rain_files[i], header=TRUE)
  totalPrecip <- sum(dat$undefined, na.rm=TRUE)
  print(totalPrecip)
  totalAnnualPrecip[i] <- totalPrecip * 3
}

meanAnnualPrecip <- mean(totalAnnualPrecip)
sdAnnualPrecip <- sd(totalAnnualPrecip)
medianAnnualPrecip <- median(totalAnnualPrecip)

## PLOT totalAnnualPrecip by year
## include horizontal lines for mean/yr from climate data = 902 mm
## and from this data, which is 292.9939375
barplot(totalAnnualPrecip, main="Total Annual Rainfall from TRMM",
        xlab="Year")
abline(h=meanAnnualPrecip, col="blue")
abline(h=902, col="red")