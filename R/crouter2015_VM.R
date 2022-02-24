#' Function to apply the intensity cutpoints from Crouter et al 2015 VM in children
#'
#' @param   acc_data_counts Wrist accelerometer count data in 1-second epochs with column names Timestamp, AxisX, AxisY, AxisZ
#' @param   sed_cp Sedentary behavior acceleration threshold.
#' @param   mpa_cp Moderate intensity activity acceleration threshold.
#' @param   vpa_cp Vigorous intensity activity acceleration threshold.
#' @param   epoch Non-overlapping window size in seconds. Default is 5-seconds
#' @param   expand_1sec Binary indicator of whether only PA estimates should be returned as a second-by-second vector
#'
#' @return  Aggregated data in 60-second epochs with accelerometer count values and method estimates
#'
#' @example sedsphere(acc_data_raw)

crouter2015_VM = function(acc_data_counts, sed_cp = 101, mpa_cp = 610, vpa_cp = 1810, epoch = 5, expand_1sec = F){
  if(epoch != 5){
    stop("crouter2015_VM Cutpoint was developed using 5-second epochs. As of now, cutpoint scaling is not supported.")
  }

  acc_data_new = ag_epochr(acc_data_counts, epoch = epoch)

  acc_data_new$Crouter2015_VM = factor(cut(acc_data_new$Axis1, breaks = c(-Inf, sed_cp, mpa_cp, vpa_cp, Inf), labels = c('Sedentary','LPA','MPA','VPA'), right = F), levels = c('Sedentary','LPA','MPA','VPA'), labels = c('Sedentary','LPA','MPA','VPA'))

  if(expand_1sec == T){
    Crouter2015_VM = data.frame(Timestamp = acc_data_counts$Timestamp,
                              Axis1_60sec = rep(acc_data_new$Axis1, each = epoch)[1:nrow(acc_data_counts)],
                              Crouter2015_VM = factor(rep(acc_data_new$Crouter2015_VM, each = epoch), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:nrow(acc_data_counts)])
    return(Crouter2015_VM)

  } else {

    return(acc_data_new)

  }

}
