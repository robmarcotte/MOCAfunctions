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

crouter2010 = function(acc_data_counts, epoch = 10, expand_1sec = F){
  if(epoch != 10){
    stop("Crouter 2010 two-regression was developed using 10-second epochs. As of now, cutpoint scaling is not supported.")
  }

  acc_data_new = ag_epochr(acc_data_counts, epoch = epoch)

  acc_data_new$METs = NA
  acc_data_new$cv1 = slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 0, .after = 5, .complete = T)
  acc_data_new$cv2 = slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 1, .after = 4, .complete = T)
  acc_data_new$cv3 = slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 2, .after = 3, .complete = T)
  acc_data_new$cv4 = slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 3, .after = 2, .complete = T)
  acc_data_new$cv5 = slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 4, .after = 1, .complete = T)
  acc_data_new$cv6 = slide_dbl(acc_data_new$Axis1, function(x){ifelse(mean(x) == 0, 0, sd(x)/mean(x))}, .before = 5, .after = 0, .complete = T)

  acc_data_new = acc_data_new %>% dplyr::rowwise() %>% dplyr::mutate(CV = min(c(cv1, cv2, cv3, cv4, cv5, cv6), na.rm = T))

  for(i in 1:nrow(acc_data_new)){

    if(acc_data_new$Axis1[i] <= 8){
      acc_data_new$METs[i] = 1.0
    } else {
      acc_data_new$METs[i] = ifelse(acc_data_new$CV[i] <= 10,
                                 2.294275*(exp(0.00084679*acc_data_new$Axis1[i])),
                                 0.749395+(0.716431*ln(acc_data_new$Axis1[i]))-
                                   (0.179874*ln(acc_data_new$Axis1[i]))+
                                   (0.033173*(ln(acc_data_new$Axis1[i])^3)))
    }
  }

  # Average MET value of 6 consecutive 10-second epochs within each minute is calculated to obtain average MET value for that minute
  acc_data_new$index = rep(seq(1, (nrow(acc_data_new)/(60/epoch))), each = (60/epoch))

  acc_data_minute = acc_data_new %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                          METs = mean(METs, na.rm = T))

  acc_data_minute$Crouter2010 = factor(cut(acc_data_minute$METs, breaks = c(-Inf, 1.51, 3, 6, Inf), labels = c('Sedentary','LPA','MPA','VPA'), right = F), levels = c('Sedentary','LPA','MPA','VPA'), labels = c('Sedentary','LPA','MPA','VPA'))

  if(expand_1sec == T){
    Crouter2010 = data.frame(Timestamp = acc_data_counts$Timestamp,
                             METs = rep(acc_data_minute$METs, each = 60)[1:nrow(acc_data_counts)],,
                             Crouter2010 = factor(rep(acc_data_minute$Crouter2010, each = 60), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:nrow(acc_data_counts)])


    return(Crouter2010)

  } else {

    return(acc_data_minute)

  }

}
