# pow.625
#
# Computes the proportion of the power spectrum captured between the bandwidths of 0.6 to 2.5 Hz
# Applies a Fast Fourier Transform to a time-series signal and computes the modulus of the series to estimate the signal power spectrum
# Used in Staudenmayer et al 2015 supplementary code, written to be applied using tapply
#
# signal = A time series signal
#
# Library dependencies: N/A


pow.625 <- function(signal)
{
  mods <- Mod(fft(signal))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- 80*(1:n)/(2*n)
  mods <- mods[1:n]
  inds <- (1:n)[(freq>0.6)&(freq<2.5)]
  pow625 <- sum(mods[inds])/sum(mods)
  mods[is.na(mods)] <- 0
  if (sd(signal)==0)
    pow625 <- 0
  return(pow625)
}
