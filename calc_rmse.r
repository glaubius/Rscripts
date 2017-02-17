## calculate rmse for pairs of ctrl and test DEMofDiffvalues from AgrTerrModel
## first create list of ctrl and test file names, number of calculations
ctrl_files <- list.files(path = "./no-at", pattern = "DoD-1000-*")
test_files <- list.files(path = "./yes-at", pattern = "DoD-1000-*")
num_calcs <- length(ctrl_files)
## finally, set up a list for rmse values
rmse_values <- rep(NA, num_calcs)

## calc_rmse function with inputs: ctrl_files, test_files, num_calcs, and rmse_values
calc_rmse <- function(ctrl_files, test_files, num_calcs, rmse_values){
	for (i in 1:num_calcs){
		read_ctrl <- paste0("./no-at/", ctrl_files[i])
	  ctrl <- read.table(read_ctrl, header=FALSE)
	  #ctrl <- read.table(ctrl_files[i], header=FALSE)
	  read_test <- paste0("./yes-at/", test_files[i])
		test <- read.table(read_test, header=FALSE)
		#test <- read.table(test_files[i], header=FALSE)
		error <- ctrl - test
		rmse <- sqrt(mean(error^2))
		rmse_values[i] <- rmse
	} 
	return(rmse_values)
}

rmse_values <- calc_rmse(ctrl_files, test_files, num_calcs, rmse_values)