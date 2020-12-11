# domfreq
#
# Applies a Fast Fourier Transform to a time-series signal and identifies frequency with thie largest modulus
# Used in Staudenmayer et al 2015 supplementary code, written to be applied using tapply
#
# signal = A time series signal
#
# Library dependencies: N/A

dom.freq <- function(signal)
{
  if(length(signal)==1)
    return(NA)
  mods <- Mod(fft(signal))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- 80*(1:n)/(2*n)
  mods <- mods[1:n]
  dom.ind <- which.max(mods)
  d.f <- as.vector(freq[which.max(mods)])
  return(d.f)
}
