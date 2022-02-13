#' Function to apply the intensity cutpoints from Montoye et al 2020 Journal of Sports Sciences
#'
#' @param   acc_data_counts Wrist accelerometer count data in 1-second epochs with column names Timestamp, AxisX, AxisY, AxisZ
#' @param   sed_cp Moderate-to-vigorous intensity activity acceleration threshold. Default MVPA threshold is >489 g
#' @param   mvpa_cp Moderate-to-vigorous intensity activity acceleration threshold. Default MVPA threshold is >489 g
#' @param   epoch Non-overlapping window size in seconds. Default is 60-seconds
#' @param   expand_1sec Binary indicator of whether only SedSphere estimates should be returned as a second-by-second vector
#'
#' @return  Aggregated data in 15-second epochs with accelerometer values and SedSphere estimate
#'
#' @example sedsphere(acc_data_raw)

montoye2020 = function(acc_data_counts, sed_cp = 2860, mpa_cp = 3941, vpa_cp = 5613, epoch = 60, expand_1sec = F){
  if(epoch != 60){
    stop("Montoye2020 Cutpoint was developed using 60-second epochs. As of now, cutpoint scaling is not supported.")
  }


  acc_data_new = ag_epochr(acc_data_counts, epoch = epoch)

  acc_data_new$Montoye2020 = factor(cut(acc_data_new$VM, breaks = c(-Inf, sed_cp, mpa_cp, vpa_cp, Inf), labels = c('Sedentary','LPA','MPA','VPA'), right = T), levels = c('Sedentary','LPA','MPA','VPA'), labels = c('Sedentary','LPA','MPA','VPA'))

  if(expand_1sec == T){
    Montoye2020 = data.frame(Timestamp = acc_data_counts$Timestamp,
                             VM_60sec = rep(acc_data_new$VM, each = epoch)[1:nrow(acc_data_counts)],
                             Montoye2020 = factor(rep(acc_data_new$Montoye2020, each = epoch), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:nrow(acc_data_counts)])
    return(Montoye2020)

  } else {

    return(acc_data_new)

  }

}
