# frac_pow_domfreq
#
# Applies a Fast Fourier Transform to a time-series signal and identifies proportion of the power spectrum represented by the dominant frequency (i.e. frequency with the largest modulus)
# Used in Staudenmayer et al 2015 supplementary code, written to be applied using tapply
#
# signal = A time series signal
#
# Library dependencies: N/A

frac.pow.dom.freq <- function(signal)
{
  mods <- Mod(fft(signal))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- 80*(1:n)/(2*n)
  mods <- mods[1:n]
  rat <- max(mods)/sum(mods)
  mods[is.na(mods)] <- 0
  if (sd(signal)==0)
    rat <- 0
  return(rat)

}
