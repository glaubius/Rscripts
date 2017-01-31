y## Plot sample means with confidence intervals
## adapted from https://www.youtube.com/watch?v=x4ekQ1nanQ4

## Inputs: list of rmse_values, list to save sample_means, matrix of cis
num_means <- length(rmse_values) - 1
sample_mean <- matrix(nrow=num_means, ncol=2)
cis <- matrix(nrow=num_means, ncol=2)


for (i in 1:num_means){
  rows <- i + 1
  observations <- rmse_values[1:rows]
  #store number of means and sample mean
  sample_mean[i, 1] <- rows
  sample_mean[i, 2] <- mean(observations)
  #construct ci
  stdev <- sd(observations)
  n <- length(observations)
  se_mean <- stdev / sqrt(n)
  #store cis
  cis[i, 1] <- sample_mean[i, 2] - 1.96 * se_mean
  cis[i, 2] <- sample_mean[i, 2] + 1.96 * se_mean
}

#Plot all data, see subset below
plot(sample_mean[,1], sample_mean[,2], xlab = "Number of Simulations", ylab = "Mean of RMSE values", cex = 0.5, col="blue")
# segments(x0=sample_mean[, 1], x1=sample_mean[, 1], y0=cis[, 1], y1=cis[, 2], col="red", lwd=20)
arrows(sample_mean[,1], cis[,1], sample_mean[,1], cis[,2], code=3, angle=90, length=0.05, col='red')

#Subset data for greater legibility in plots, then plot
Sub_sample_num <- sample_mean[,1][seq(1, nrow(sample_mean), 10)]
Sub_sample_mean <- sample_mean[,2][seq(1, nrow(sample_mean), 10)]
Sub_cis_low <- cis[,1][seq(1, nrow(cis), 10)]
Sub_cis_high <- cis[,2][seq(1, nrow(cis), 10)]

plot(Sub_sample_num, Sub_sample_mean, xlab = "Number of Simulations", ylab = "Mean of RMSE values", cex = 0.5, col="blue")
# segments(x0=sample_mean[, 1], x1=sample_mean[, 1], y0=cis[, 1], y1=cis[, 2], col="red", lwd=20)
arrows(Sub_sample_num, Sub_cis_low, Sub_sample_num, Sub_cis_high, code=3, angle=90, length=0.05, col='red')
