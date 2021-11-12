#' pow1020
#'
#' Computes the proportion of the power spectrum captured between the bandwidths of 10 to 20 Hz
#' Applies a Fast Fourier Transform to a time-series signal and computes the modulus of the series to estimate the signal power spectrum
#'
#' @param signal A time series signal
#' @param samp_freq Sampling frequency of the raw accelerometer data. Defaults to 80 Hz
#'
#' Library dependencies: N/A


pow1020 <- function(signal, samp_freq = 80)
{
  mods <- Mod(fft(signal))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- samp_freq*(1:n)/(2*n)
  mods <- mods[1:n]
  inds <- (1:n)[(freq>10)&(freq<20)] # Refer to 2016 Straczkiewicz Physiol Meas paper on Driving Detection DADA algorithm
  pow1020 <- sum(mods[inds])/sum(mods)
  mods[is.na(mods)] <- 0
  if (sd(signal)==0)
    pow1020 <- 0
  return(pow1020)
}
