GetFits <- function(zeta,epsilon,B,j,k) {
  fit <- vector(length = length(participantID))
  for (i in 1:length(participantID)){
    output <- KalmanPMU(choices[,,i],points[,,i],zeta,epsilon,B,j,k)
    fit[i] <- output$negativeLL
  }
  return(fit)
}