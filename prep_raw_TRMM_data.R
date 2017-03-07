# Create list of TRMM files to be imported
rain_files <- list.files(path=".", pattern="TRMM*")
num_files <- length(rain_files)

# Create dataframe using first file in the list
dat <- read.csv(rain_files[1], header=TRUE, na.strings = "NA")

# Append remaining TRMM files to dataframe
for(t in 2:num_files){
  dat <- rbind(dat, read.csv(rain_files[t], header=TRUE, na.strings = "NA"))
}

# Convert date String to Numerical
dat$Date <- as.Date(dat$system.time_start, format = "%B %d, %Y")
colnames(dat) <- c("oldSystemDate", "meanPrecip", "DateTime")

# Check for missing data, replace NA with 0
isNA <- dat[ which(is.na(dat$meanPrecip) == TRUE), ]
dat$meanPrecip[is.na(dat$meanPrecip)] <- 0

# Check that all NA values are out of dat
isNA <- dat[ which(is.na(dat$meanPrecip) == TRUE), ]

# ADD Save clean data file


