#' Tree models from Staudenmayer et al 2015
#'
#' @param   acc_data_raw Raw tri-axial wrist accelerometer data with column names Timestamp, AxisX, AxisY, AxisZ
#' @param   mods_filepath Filepath that contains rf.loc.model in an RData object. Assuming by default that model is already loaded in the environment.
#' @param   samp_freq Sampling frequency of the raw accelerometer data. Default is 80 hz
#' @param   epoch Non-overlapping window size in seconds. Default is 15-seconds
#' @param   expand_1sec Binary indicator of whether only estimates should be returned as a second-by-second vector
#'
#' @return  Aggregated data in 15-second epochs with accelerometer values and staudenmayer2015_rf_loco estimate
#'
#' @example staudenmayer2015_rf_loco(acc_data_raw)

staudenmayer2015_rf_intensity = function(acc_data_raw, mods_filepath = NA, samp_freq = 80, epoch = 15, expand_1sec = F){

  acc_data_raw.sum = ag_feature_calc(acc_data_raw, window = epoch)

  # acc_data_raw$VMcorrG = abs(sqrt(acc_data_raw$AxisX^2 + acc_data_raw$AxisY^2 + acc_data_raw$AxisZ^2)-1)
  # acc_data_raw$v.ang <- 90*(asin(acc_data_raw$AxisX/acc_data_raw$VM)/(pi/2))
  #
  # n <- dim(acc_data_raw)[1]
  #
  # mins <- ceiling(n/(samp_freq*epoch))
  #
  # acc_data_raw$min <- rep(1:mins,each=epoch*samp_freq)[1:n]
  #
  #
  # acc_data_raw.sum <- data.frame(Timestamp = acc_data_raw$Timestamp[seq(1, n, by = epoch*samp_freq)],
  #                                mean.vm=tapply(acc_data_raw$VM,acc_data_raw$min,mean,na.rm=T),
  #                                sd.vm=tapply(acc_data_raw$VM,acc_data_raw$min,sd,na.rm=T),
  #                                mean.ang=tapply(acc_data_raw$v.ang,acc_data_raw$min,mean,na.rm=T),
  #                                sd.ang=tapply(acc_data_raw$v.ang,acc_data_raw$min,sd,na.rm=T),
  #                                p625=tapply(acc_data_raw$VM,acc_data_raw$min,pow.625),
  #                                dfreq=tapply(acc_data_raw$VM,acc_data_raw$min,dom.freq),
  #                                ratio.df=tapply(acc_data_raw$VM,acc_data_raw$min,frac.pow.dom.freq))

  if(!is.na(mods_filepath)){
    load(mods_filepath)
  }

  acc_data_raw.sum$intensity.rf = factor(predict(rf.combo.model, newdata=acc_data_raw.sum, type="class"), levels =c('S','L','M','V'), labels = c('Sedentary','LPA','MPA','VPA'))

  if(expand_1sec == T){
    Staudenmayer2015_Intensity = data.frame(Timestamp = acc_data_raw$Timestamp[seq(1, n, by = samp_freq)],
                                             Staudenmayer2015_Intensity = factor(rep(acc_data_raw.sum$intensity.rf, each = epoch), levels =c('Sedentary','LPA','MPA','VPA'), labels =c('Sedentary','LPA','MPA','VPA'))[1:floor(n/samp_freq)])
    return(Staudenmayer2015_Intensity)

  } else {

    return(acc_data_raw.sum)


  }

}
