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

romanzini2014_VA = function(acc_data_counts, sed_cp = 47, mpa_cp = 607, vpa_cp = 818, epoch = 15, expand_1sec = F){
  if(epoch != 15){
    stop("Romanzini2014 Cutpoint was developed using 15-second epochs. As of now, cutpoint scaling is not supported.")
  }

  acc_data_new = ag_epochr(acc_data_counts, epoch = epoch)

  acc_data_new$Romanzini2014_VA = factor(cut(acc_data_new$Axis1, breaks = c(-Inf, sed_cp, mpa_cp, vpa_cp, Inf), labels = c('Sedentary','LPA','MPA','VPA'), right = F), levels = c('Sedentary','LPA','MPA','VPA'), labels = c('Sedentary','LPA','MPA','VPA'))

  if(expand_1sec == T){
    Romanzini2014_VA = data.frame(Timestamp = acc_data_counts$Timestamp,
                              Axis1_15sec = rep(acc_data_new$Axis1, each = epoch)[1:nrow(acc_data_counts)],
                              Romanzini2014_VA = factor(rep(acc_data_new$Romanzini2014_VA, each = epoch), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:nrow(acc_data_counts)])
    return(Romanzini2014_VA)

  } else {

    return(acc_data_new)

  }

}
