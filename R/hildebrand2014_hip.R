#' ENMO adult hip cutpoints for intensity categories from Hildebrand et al 2014 (sedentary) and 2017 (activity intensity)
#'
#' @param   enmo_data Calibrated raw-acceleration data with column names Timestamp and ENMO
#' @param   samp_freq Sampling frequency of the raw accelerometer data. Default is 80 hz
#' @param   epoch Non-overlapping window size in seconds. Default is 15-seconds
#' @param   expand_1sec Binary indicator of whether only estimates should be returned as a second-by-second vector
#'
#' @return  Aggregated data in 15-second epochs with accelerometer values and SedSphere estimate
#'
#' @example hildebrand2014(enmo_data)


hildebrand2014_hip = function(enmo_data, sed_cp = 47.4, mpa_cp = 69.1, vpa_cp = 258.7, samp_freq = 80, epoch = 60, expand_1sec = F){

  n <- dim(enmo_data)[1]

  secs <- ceiling(n/samp_freq)
  mins <- ceiling(n/(samp_freq*epoch))

  # Collapse into 1-second windows by taking the mean of ENMO
  enmo_data$secs <- rep(1:secs,each=samp_freq)[1:n]
  enmo_data.secs = enmo_data %>% group_by(secs) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                     enmo_1sec = mean(ENMO, na.rm = T))

  # Collapse into 1-minute windows by taking the mean of 1-second ENMO
  enmo_data.secs$mins <- rep(1:mins,each=epoch)[1:nrow(enmo_data.secs)]
  enmo_data.min = enmo_data.secs %>% group_by(mins) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                         enmo_1min = mean(enmo_1sec, na.rm = T))

  # Apply Hildebrand cutpoints
  enmo_data.min$Intensity = cut(enmo_data.min$enmo_1min, breaks = c(-Inf, sed_cp, mpa_cp, vpa_cp, Inf), labels = c('Sedentary','LPA','MPA','VPA'), right = F)
  enmo_data.min$Intensity = factor(enmo_data.min$Intensity, levels = c('Sedentary','LPA','MPA','VPA'), labels = c('Sedentary','LPA','MPA','VPA'))

  if(expand_1sec == T){
    Hildebrand2014_Intensity = data.frame(Timestamp = enmo_data.secs$Timestamp[1:floor(n/samp_freq)],
                                          Hildebrand2014_Intensity = factor(rep(enmo_data.min$Intensity, each = epoch), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:floor(n/samp_freq)])
    return(Hildebrand2014_Intensity)

  } else {

    return(enmo_data)


  }

}
