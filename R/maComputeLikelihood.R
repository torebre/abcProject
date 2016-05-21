
ComputeMALikelihoodInternal <- function(theta1, theta2, time.series.length, time.series) {
  theta1 <- -theta1
  theta2 <- -theta2
  B <- -rbind(c(theta1, theta2), c(0, theta1))
  L <- diag(1, nrow = time.series.length)
  diag(L[2:time.series.length, 1:(time.series.length - 1)]) <- -theta1
  diag(L[3:time.series.length, 1:(time.series.length - 2)]) <- -theta2
  F.mat <- rbind(B, matrix(0, nrow = time.series.length - 2, ncol = 2))

  sigma.squared <- 1

  # TODO This is a bit off because of the start of the range
  a.est <- rep(0, time.series.length)
  for(i in 3:time.series.length) {
    a.est[i] <- time.series[i] + theta1 * a.est[i - 1] + theta2 * a.est[i - 2]
  }

  D <- diag(2) + t(F.mat) %*% solve(t(L)) %*% solve(L) %*% F.mat
  likelihood <- sigma.squared^(-time.series.length/2) * det(D)^(-1/2) * exp(-(1/(2 * sigma.squared)) * sum(a.est^2))

  if(is.nan(likelihood)) {
    return(0)
  }
  likelihood
}

ComputeMALikelihood <- function(time.series.length, time.series, theta1.range = seq(-2, 2, 0.2), theta2.range = seq(-1, 1, 0.2)) {
  likelihood.map <- matrix(NA, nrow = length(theta1.range), ncol = length(theta2.range))

  for(i in 1:length(theta1.range)) {
    for(j in 1:length(theta2.range)) {

      # -2 < theta1 < 2, theta1 + theta2 > -1, theta1 - theta2 < 1
      # if(theta1.range[i] + theta2.range[j] > 1 || theta2.range[j] - theta1.range[i] > 1) {
      if(theta1.range[i] + theta2.range[j] > -1 && theta1.range[i] - theta2.range[j] < 1) {
        likelihood.map[i, j] <- ComputeMALikelihoodInternal(theta1.range[i], theta2.range[j], time.series.length, time.series) #, error = function(x) {likelihood.map[i, j] <- -1})
        # next
      }

      # tryCatch(likelihood.map[i, j] <- ComputeMALikelihoodInternal(theta1.range[i], theta2.range[j], time.series.length, time.series), error = function(x) {likelihood.map[i, j] <- 0})
    }
  }

  likelihood.map
}


