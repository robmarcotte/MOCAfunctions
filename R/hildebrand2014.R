#' ENMO cutpoints for intensity categoriers from Hildebrand et al 2014 (sedentary) and 2017 (activity intensity)
#'
#' @param   enmo_data Calibrated raw-acceleration data with column names Timestamp and ENMO
#' @param   samp_freq Sampling frequency of the raw accelerometer data. Default is 80 hz
#' @param   epoch Non-overlapping window size in seconds. Default is 15-seconds
#' @param   expand_1sec Binary indicator of whether only estimates should be returned as a second-by-second vector
#'
#' @return  Aggregated data in 15-second epochs with accelerometer values and SedSphere estimate
#'
#' @example hildebrand2014(enmo_data)


hildebrand2014 = function(enmo_data, samp_freq = 80, epoch = 60, expand_1sec = F){

  n <- dim(enmo_data)[1]

  secs <- ceiling(n/samp_freq)
  mins <- ceiling(n/(samp_freq*epoch))

  # Collapse into 1-second windows by taking the mean of ENMO
  enmo_data$secs <- rep(1:secs,each=samp_freq)[1:n]
  enmo_data = enmo_data %>% group_by(secs) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                enmo_1sec = mean(ENMO, na.rm = T))

  # Collapse into 1-minute windows by taking the mean of 1-second ENMO
  enmo_data$mins <- rep(1:mins,each=epoch)[1:nrow(enmo_data)]
  enmo_data = enmo_data %>% group_by(mins) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                enmo_1min = mean(enmo_1sec, na.rm = T))

  # Apply Hildebrand cutpoints
  enmo_data$Intensity = cut(enmo_data$enmo_1min, breaks = c(-Inf, 44.8, 100.6, 428.8, Inf), labels = c('Sedentary','LPA','MPA','VPA'), include.lowest = F)

  if(expand_1sec == T){
    Hildebrand2014_Intensity = data.frame(Timestamp = acc_data_raw$Timestamp[seq(1, n, by = samp_freq)],
                                          Hildebrand2014_Intensity = factor(rep(enmo_data$Intensity, each = epoch), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:floor(n/samp_freq)])
    return(Hildebrand2014_Intensity)

  } else {

    return(enmo_data)


  }

}