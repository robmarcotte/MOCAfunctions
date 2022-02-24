#' Function to apply the intensity cutpoints from Freedson et al 1998
#'
#' @param   acc_data_counts Wrist accelerometer count data in 1-second epochs with column names Timestamp, AxisX, AxisY, AxisZ
#' @param   sed_cp Sedentary behavior acceleration threshold. Default MVPA threshold is <= 100 cpm
#' @param   mpa_cp Moderate intensity activity acceleration threshold. Default MPA threshold is 1952-5997 cpm
#' @param   vpa_cp Vigorous intensity activity acceleration threshold. Default MVPA threshold is >= 5998 cpm
#' @param   epoch Non-overlapping window size in seconds. Default is 60-seconds
#' @param   expand_1sec Binary indicator of whether only PA estimates should be returned as a second-by-second vector
#'
#' @return  Aggregated data in 60-second epochs with accelerometer count values and method estimates
#'
#' @example sedsphere(acc_data_raw)

freedson2005_youth = function(acc_data_counts, age = NA, sed_cp = 150, mpa_cp = 1952, vpa_cp = 5725, epoch = 60, expand_1sec = F){
  if(epoch != 60){
    stop("Freedson Youth Cutpoint was developed using 60-second epochs. As of now, cutpoint scaling is not supported.")
  }


  acc_data_new = ag_epochr(acc_data_counts, epoch = epoch)

  acc_data_new$METs = 2.757 + (0.0015*acc_data_new$Axis1) - 0.08957*age - 0.000038*acc_data_new$Axis1*age

  acc_data_new$Freedson2005_Youth = factor(ifelse(acc_data_new$Axis1 < sed_cp, 'Sedentary',
                                                  as.character(cut(acc_data_new$METs, breaks = c(-Inf, 3, 6, Inf), labels = c('LPA','MPA','VPA'), right = F))),
                                                  levels = c('Sedentary','LPA','MPA','VPA'), labels = c('Sedentary','LPA','MPA','VPA'))


  if(expand_1sec == T){
    Freedson2005_Youth = data.frame(Timestamp = acc_data_counts$Timestamp,
                              Axis1_60sec = rep(acc_data_new$Axis1, each = epoch)[1:nrow(acc_data_counts)],
                              Freedson2005_Youth = factor(rep(acc_data_new$Freedson2005_Youth, each = epoch), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:nrow(acc_data_counts)])
    return(Freedson2005_Youth)

  } else {

    return(acc_data_new)

  }

}
