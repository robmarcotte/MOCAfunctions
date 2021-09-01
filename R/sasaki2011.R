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

sasaki2011 = function(acc_data_counts, sed_cp = 101, mpa_cp = 2690, vpa_cp = 6167, epoch = 60, expand_1sec = F){
  if(epoch != 60){
    stop("Sasaki2011 Cutpoint was developed using 60-second epochs. As of now, cutpoint scaling is not supported.")
  }

  acc_data_new = ag_epochr(acc_data_counts, epoch = epoch)

  acc_data_new$Sasaki2011 = factor(cut(acc_data_new$VM, breaks = c(-Inf, sed_cp, mpa_cp, vpa_cp, Inf), labels = c('Sedentary','LPA','MPA','VPA'), right = F), levels = c('Sedentary','LPA','MPA','VPA'), labels = c('Sedentary','LPA','MPA','VPA'))

  if(expand_1sec == T){
    Sasaki2011 = data.frame(Timestamp = acc_data_counts$Timestamp,
                            VM_60sec = rep(acc_data_new$VM, each= epoch)[1:nrow(acc_data_counts)],,
                            Sasaki2011 = factor(rep(acc_data_new$Sasaki2011, each = epoch), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:nrow(acc_data_counts)])
    return(Sasaki2011)

  } else {

    return(acc_data_new)

  }

}
