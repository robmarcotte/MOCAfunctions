# ag_feature_calc
#
# Custom function to compute features of raw accelerometer data. Can be applied to fixed- or variable-windows (epochs vs sojourns, respectively).
#
# ag_data_raw_wrist = Raw accelerometer data containing AxisX, AxisY, AxisZ, and VM data columns
# participant = Character string for participant ID
# samp_freq = Sampling frequency of the raw accelerometer data. Defaults to 80 Hz
# window = Epoch window (in seconds). Can be either a fixed number or 'sojourn' character string. If 'sojourn', a column of sojourn indices is expected to be present in the data.
# soj_colname = Column name containing the sojourn indices
# seconds_colname = Column name containing the duration of a given sojourn
#
# Library dependencies: N/A


ag_feature_calc = function(ag_data_raw_wrist, participant, samp_freq = 80,window = 15, soj_colname = NA, seconds_colname = NA){

  n <- dim(ag_data_raw_wrist)[1]

  ag_data_raw_wrist$v.ang <- ifelse(ag_data_raw_wrist$AxisY > 1, asin(1)*180/pi,
                                    ifelse(ag_data_raw_wrist$AxisY < -1, asin(-1)*180/pi,
                                           asin(pmin(pmax(ag_data_raw_wrist$AxisY,-1.0),1.0))*180/pi))

  if(window == 'sojourns'){
    soj_colindex = which(colnames(ag_data_raw_wrist) == soj_colname)
    seconds_colindex = which(colnames(ag_data_raw_wrist) == seconds_colname)
    # Compute features within sojourns
    ag_data_raw_wrist.sum <- data.frame(sojourn = tapply(ag_data_raw_wrist[,soj_colindex], ag_data_raw_wrist[,soj_colindex], data.table::first),
                                        seconds = tapply(ag_data_raw_wrist[,seconds_colindex], ag_data_raw_wrist[,soj_colindex], data.table::first),
                                        mean.vm=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,soj_colindex],mean,na.rm=T),
                                        sd.vm=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,soj_colindex],sd,na.rm=T),
                                        mean.ang=tapply(ag_data_raw_wrist$v.ang,ag_data_raw_wrist[,soj_colindex],mean,na.rm=T),
                                        sd.ang=tapply(ag_data_raw_wrist$v.ang,ag_data_raw_wrist[,soj_colindex],sd,na.rm=T),
                                        p625=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,soj_colindex],pow.625),
                                        dfreq=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,soj_colindex],dom.freq),
                                        ratio.df=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist[,soj_colindex],frac.pow.dom.freq),
                                        stringsAsFactors = F)
  } else {

    epoch <- ceiling(n/(samp_freq*window))
    ag_data_raw_wrist$epoch <- rep(1:epoch,each=window*samp_freq)[1:n]

    # Compute features within epochs
    ag_data_raw_wrist.sum <- data.frame(Participant = tapply(ag_data_raw_wrist$Participant, ag_data_raw_wrist$epoch, data.table::first),
                                        Date = tapply(ag_data_raw_wrist$Date, ag_data_raw_wrist$epoch, data.table::first),
                                        Time = tapply(ag_data_raw_wrist$Time, ag_data_raw_wrist$epoch, data.table::first),
                                        Behavior = tapply(ag_data_raw_wrist$Behavior, ag_data_raw_wrist$epoch, majority_string, piece = 'coded_strings'),
                                        Modifier_1 = tapply(ag_data_raw_wrist$Modifier_1, ag_data_raw_wrist$epoch, majority_string, piece = 'coded_strings'),
                                        Modifier_2 = tapply(ag_data_raw_wrist$Modifier_2, ag_data_raw_wrist$epoch, majority_string, piece = 'coded_strings'),
                                        Stationary = tapply(ag_data_raw_wrist$Stationary, ag_data_raw_wrist$epoch, majority_string, piece = 'coded_strings'),
                                        Omit_me = tapply(ag_data_raw_wrist$Omit_me, ag_data_raw_wrist$epoch, data.table::first),
                                        mean.vm=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist$epoch,mean,na.rm=T),
                                        sd.vm=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist$epoch,sd,na.rm=T),
                                        mean.ang=tapply(ag_data_raw_wrist$v.ang,ag_data_raw_wrist$epoch,mean,na.rm=T),
                                        sd.ang=tapply(ag_data_raw_wrist$v.ang,ag_data_raw_wrist$epoch,sd,na.rm=T),
                                        p625=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist$epoch,pow.625),
                                        dfreq=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist$epoch,dom.freq),
                                        ratio.df=tapply(ag_data_raw_wrist$VM,ag_data_raw_wrist$epoch,frac.pow.dom.freq),
                                        stringsAsFactors = F)

  }

  return(ag_data_raw_wrist.sum)

}
