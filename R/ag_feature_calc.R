#' Custom function to compute features of raw accelerometer data. Can be applied to fixed- or variable-windows (epochs vs sojourns, respectively).
#'
#' @param ag_data_raw_wrist Raw accelerometer data containing AxisX, AxisY, AxisZ, and VM data columns
#' @param participant Character string for participant ID
#' @param samp_freq Sampling frequency of the raw accelerometer data. Defaults to 80 Hz
#' @param window Epoch window (in seconds). Can be either a fixed number or 'sojourn' character string. If 'sojourn', a column of sojourn indices is expected to be present in the data.
#' @param long_axis Axis that is parallel to the longitudinal axis of the forearm (wrist) or vertical axis (hip)
#' @param angle_comp Trigonometric function to compute the device angle. Rowlands (SedSphere) and Staudenmayer 2015 use slightly different equations resulting in slightly different values.
#' @param soj_colname Character string for column name containing the sojourn indices
#' @param seconds_colname Character string for column name containing the duration of a given sojourn
#
#' Library dependencies: tidyverse, lubridate, data.table


ag_feature_calc = function(ag_data_raw_wrist, participant, samp_freq = 80, window = 15, long_axis = 'y', angle_comp ='Default_Staudenmayer',
                           soj_colname = NA, seconds_colname = NA, inactive_threshold = .00375){

  if("data.table" %in% (.packages())){
    detach(package:data.table, unload = TRUE, force = T) # causes issues with referencing column indices with a non-numeric character element
  }

  n <- dim(ag_data_raw_wrist)[1]


  # Assumes that the Timestamp column is first
  long_axis_index = switch(long_axis,
                           'x' = str_which(colnames(ag_data_raw_wrist), 'AxisX'),
                           'y' = str_which(colnames(ag_data_raw_wrist), 'AxisY'),
                           'z' = str_which(colnames(ag_data_raw_wrist), 'AxisZ'))


  switch(angle_comp,
         'Rowlands' = {
           ag_data_raw_wrist$v.ang <- (90*asin(ag_data_raw_wrist[,long_axis_index]/ag_data_raw_wrist$VM))/(pi/2)
           ag_data_raw_wrist$SedSphere_v.ang <- ifelse(ag_data_raw_wrist[,long_axis_index] > 1, asin(1)*180/pi,
                                                       ifelse(ag_data_raw_wrist[,long_axis_index] < -1, asin(-1)*180/pi,
                                                              asin(pmin(pmax(ag_data_raw_wrist[,long_axis_index],-1.0),1.0))*180/pi))
         },
         'Default_Staudenmayer' = {
           ag_data_raw_wrist$v.ang <- (90*asin(ag_data_raw_wrist[,long_axis_index]/ag_data_raw_wrist$VM))/(pi/2)
         })


  if(window == 'sojourns'){
    soj_colindex = which(colnames(ag_data_raw_wrist) == soj_colname)
    seconds_colindex = which(colnames(ag_data_raw_wrist) == seconds_colname)

    # Compute features within sojourns using dplyr
    ag_data_raw_wrist.sum = ag_data_raw_wrist %>% dplyr::group_by_at(soj_colindex) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                                                        sojourn = dplyr::first(.[[soj_colindex]]),
                                                                                                        seconds = dplyr::first(.[[seconds_colindex]]),
                                                                                                        perc.soj.inactive = mean(VM_sd_1sec<=inactive_threshold),
                                                                                                        mean.vm = mean(VM, na.rm = T),
                                                                                                        sd.vm = sd(VM, na.rm = T),
                                                                                                        mean.ang = mean(v.ang, na.rm = T),
                                                                                                        sd.ang = sd(v.ang, na.rm = T),
                                                                                                        p625 = pow.625(VM),
                                                                                                        dfreq = dom.freq(VM),
                                                                                                        ratio.df = frac.pow.dom.freq(VM))


    # Compute features within sojourns using tapply
    # ag_data_raw_wrist.sum <- data.frame(sojourn = tapply(ag_data_raw_wrist[,..soj_colindex], ag_data_raw_wrist[,..soj_colindex], data.table::first),
    #                                     seconds = tapply(ag_data_raw_wrist[,..seconds_colindex], ag_data_raw_wrist[,..soj_colindex], data.table::first),
    #                                     mean.vm=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,..soj_colindex],mean,na.rm=T),
    #                                     sd.vm=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,..soj_colindex],sd,na.rm=T),
    #                                     mean.ang=tapply(ag_data_raw_wrist$v.ang,ag_data_raw_wrist[,..soj_colindex],mean,na.rm=T),
    #                                     sd.ang=tapply(ag_data_raw_wrist$v.ang,ag_data_raw_wrist[,..soj_colindex],sd,na.rm=T),
    #                                     p625=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,..soj_colindex],pow.625),
    #                                     dfreq=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,..soj_colindex],dom.freq),
    #                                     ratio.df=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,..soj_colindex],frac.pow.dom.freq),
    #                                     stringsAsFactors = F)

  } else {

    epoch <- ceiling(n/(samp_freq*window))
    ag_data_raw_wrist$epoch <- rep(1:epoch,each=window*samp_freq)[1:n]

    # Compute features within epochs
    ag_data_raw_wrist.sum <- ag_data_raw_wrist %>% dplyr::group_by(epoch) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                                               mean.vm = mean(VM, na.rm = T),
                                                                                               sd.vm = sd(VM, na.rm = T),
                                                                                               mean.ang = mean(v.ang, na.rm = T),
                                                                                               sd.ang = sd(v.ang, na.rm = T),
                                                                                               p625 = pow.625(VM),
                                                                                               p1020 = pow1020(VM),
                                                                                               dfreq = dom.freq(VM),
                                                                                               ratio.df = frac.pow.dom.freq(VM))

    if(angle_comp == 'Rowlands'){
      temp <- ag_data_raw_wrist %>% dplyr::group_by(epoch) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                                SedSphere_mean.ang = mean(SedSphere_v.ang, na.rm = T),
                                                                                SedSphere_sd.ang = sd(SedSphere_v.ang, na.rm = T))

      ag_data_raw_wrist.sum = dplyr::left_join(ag_data_raw_wrist.sum, temp)
    }

  }

  return(ag_data_raw_wrist.sum)

}
