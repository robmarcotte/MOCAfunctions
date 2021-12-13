#' Function to apply the Sedentary Sphere from Rowlands et al 2014
#'
#' @param   acc_data_raw Raw tri-axial wrist accelerometer data with column names Timestamp, AxisX, AxisY, AxisZ
#' @param   VMcorrG_mod_15s Moderate-to-vigorous intensity activity acceleration threshold. Default MVPA threshold is >489 g
#' @param   samp_freq Sampling frequency of the raw accelerometer data. Default is 80 hz
#' @param   epoch Non-overlapping window size in seconds. Default is 15-seconds
#' @param   expand_1sec Binary indicator of whether only SedSphere estimates should be returned as a second-by-second vector
#' @param   long_axis Axis that is parallel to the long axis of the forearm when worn on the wrist. x = 2, y = 3, z = 4 assuming that X, Y, and Z axes are at column positions 2:4. Default is y (3)
#' @param   interpolae Binary indicator of whether raw acceleration signal should be interpolated to 100 Hz. Default is T
#'
#' @return  Aggregated data in 15-second epochs with accelerometer values and SedSphere estimate
#'
#' @example sedsphere(acc_data_raw)

sedsphere = function(acc_data_raw, VMcorrG_mod_15s = 489, samp_freq = 80, epoch = 15, expand_1sec = F, long_axis = 3, interpolate = F){
  # Check if data.table package is loaded. If so, unload it since it causes issues with column number referencing
  # if("data.table" %in% (.packages())){
  #   detach(package:data.table, unload = TRUE) # causes issues with referencing column indices with a non-numeric character element
  # }

  # Original method was developed using the sum of VMcorrG values from 100 Hz data. If samp_freq is not 100 Hz, need to interpolate/upsample to proper frequency
  if(samp_freq != 100 & interpolate == T){ # Section is currently bugged with the interpolation -RM 7/19/2021
    warning('Sampling frequency (Sf) is lower than 100 Hz. Interpolating to the proper Sf (this may take a while for larger files)...')
    acc_data_raw = acc_data_raw %>% dplyr::rename(HEADER_TIME_STAMP =Timestamp,
                                           X = AxisX,
                                           Y = AxisY,
                                           Z = AxisZ) %>% select(HEADER_TIME_STAMP, X,Y,Z)


    acc_data_raw = MIMSunit::interpolate_signal(acc_data_raw)
    samp_freq = 100

    # Rename columns to default MOCAfunctions naming schema
    acc_data_raw = acc_data_raw %>% dplyr::rename(Timestamp = HEADER_TIME_STAMP,
                                                  AxisX = X,
                                                  AxisY = Y,
                                                  AxisZ = Z)
  }

  if(("VMcorrG" %in% colnames(acc_data_raw)) == F)
    acc_data_raw$VMcorrG = abs(sqrt(acc_data_raw$AxisX^2 + acc_data_raw$AxisY^2 + acc_data_raw$AxisZ^2)-1)

  n <- dim(acc_data_raw)[1]

  mins <- ceiling(n/(samp_freq*epoch))

  acc_data_raw$min <- rep(1:mins,each=epoch*samp_freq)[1:n]

  acc_data_raw.sum <- data.frame(Timestamp = acc_data_raw$Timestamp[seq(1, n, by = epoch*samp_freq)],
                                 mean.x=tapply(acc_data_raw$AxisX,acc_data_raw$min,mean,na.rm=T),
                                 mean.y=tapply(acc_data_raw$AxisY,acc_data_raw$min,mean,na.rm=T),
                                 mean.z=tapply(acc_data_raw$AxisZ,acc_data_raw$min,mean,na.rm=T),
                                 sum.VMcorrG = tapply(acc_data_raw$VMcorrG,acc_data_raw$min,sum,na.rm=T))

  # Assume that the columns are Timestamp, X, Y, Z
  # long_axis_index = switch(long_axis,
  #                          'x' = 2,
  #                          'y' = 3,
  #                          'z' = 4)
  long_axis_index = long_axis


  acc_data_raw.sum$v.ang <- ifelse(acc_data_raw.sum[,long_axis_index] > 1, asin(1)*180/pi,
                                        ifelse(acc_data_raw.sum[,long_axis_index] < -1, asin(-1)*180/pi,
                                               asin(pmin(pmax(acc_data_raw.sum[,long_axis_index],-1.0),1.0))*180/pi))

  # 0 = Sedentary, 1 = Upright, 2 = MVPA Activity
  acc_data_raw.sum$SedSphere = ifelse(acc_data_raw.sum$sum.VMcorrG > VMcorrG_mod_15s,2,
                                      # ifelse(acc_data_raw.sum$v.ang < -15,1,0))
                                      ifelse(acc_data_raw.sum$v.ang > 15,1,0)) # modified from <-15 to >15 because angles below horizontal are 0 to +90deg based on above calculation and device orientation (+1g for arms straight down @ side)
  acc_data_raw.sum$SedSphere = factor(acc_data_raw.sum$SedSphere, levels = c(0,1,2), labels =c('Sedentary','Upright','MVPA_Activity'))


  if(expand_1sec == T){
    SedSphere = data.frame(Timestamp = acc_data_raw$Timestamp[seq(1, n, by = samp_freq)],
                           SedSphere = factor(rep(acc_data_raw.sum$SedSphere, each = epoch), levels =c('Sedentary','Upright','MVPA_Activity'), labels =c('Sedentary','Upright','MVPA_Activity'))[1:floor(n/samp_freq)])
    return(SedSphere)

  } else {
    return(acc_data_raw.sum)

  }

}
