#' Function to automate the process of merging ActiGraph and Noldus direct observation data
#' Assumes that screening of observations has already occurred and either a default do_fix data or a custom filepath with how to approach novel observations is provided. See DO_screen for further details.
#'
#' @param ag_filepath Filepath containing ActiGraph data
#' @param do_filepath Filepath containing Noldus Observer XT data coded using the MOCA coding template
#' @param timestart Time object (ymd_hms format) identifying the start of the direct observation
#' @param samp_freq Sampling frequency of raw accelerometer data
#' @param participant_id Character string of the filename or participant ID
#' @param do_fix_reference Character string to identify which lookup table should be used for correct direct observation annotations
#' @param do_fix_custom_filepath Filepath containing a custom lookup table for correcting direct observation annotations
#' @param output_filepath Filepath where merged actigraph and direct observation data should be stored
#'
#' Library dependencies: foreach, doParallel, stringr, lubridate, dplyr, tidyr, data.table, readxl

AG_DO_merge = function(ag_filepath, do_filepath, timestart, samp_freq = 80, participant_id,
                       do_fix_reference = c('18to20','15to17.9','13to14.9','10to12.9','6to9.9','3to5.9','1.5to2.9','custom'), do_fix_custom_filepath,
                       output_filepath, export = F, device_serial_calibrate_df = NA){

  do_fix = switch(do_fix_reference,
                      '18to20' = do_fix_18to20,
                      '15to17.9' = readRDS('filepath to 15to17.9 DO errors data'),
                      '13to14.9' = readRDS('filepath to 13to14.9 DO errors data'),
                      '10to12.9' = readRDS('filepath to 10to12.9 DO errors data'),
                      '6to9.9' = readRDS('filepath to 6to9.9 DO errors data'),
                      '3to5.9' = readRDS('filepath to 3to5.9 DO errors data'),
                      '1.5to2.9' = readRDS('filepath to 1.5to2.9 DO errors data'),
                      'custom' = readRDS(do_fix_custom_filepath))

  do_fix$METs = as.character(do_fix$METs)

  session_start_time = timestart
  session_date = ymd(str_split(session_start_time, ' ', simplify = T)[,1])

  noldus_data = MOCAfunctions::DO_descriptives(do_filepath)
  noldus_data = MOCAfunctions::DO_msec_transform(noldus_data) # expand DO data to hundredths of seconds
  noldus_data = MOCAfunctions::DO_timestamp(noldus_data, session_start_time)

  noldus_data$index = rep(seq(1, ceiling(nrow(noldus_data)/100)), each = 100)[1:nrow(noldus_data)]

  noldus_data = noldus_data %>% group_by(index) %>% summarize(Participant = participant_id,
                                                              Timestamp = dplyr::first(Time),
                                                              Date = session_date,
                                                              Behavior = names(which.max(table(Behavior, useNA = 'ifany'))),
                                                              METs = names(which.max(table(METs, useNA = 'ifany'))),
                                                              Modifier_2 = names(which.max(table(Modifier_2, useNA = 'ifany'))),
                                                              Modifier_3 = names(which.max(table(Modifier_3, useNA = 'ifany'))),
                                                              MET.level = names(which.max(table(MET.level, useNA = 'ifany'))))

  noldus_data$METs = as.character(noldus_data$METs)

  noldus_data =  MOCAfunctions::DO_cleaning_18to20(noldus_data, do_fix)

  noldus_data$Time =  MOCAfunctions::strip_time_from_fulldate(noldus_data$Timestamp)

  noldus_start = ymd_hms(str_c(noldus_data$Date[1], noldus_data$Time[1]))
  noldus_end_raw = ymd_hms(str_c(noldus_data$Date[1], noldus_data$Time[nrow(noldus_data)])) + seconds(1)
  noldus_end_count = ymd_hms(str_c(noldus_data$Date[1], noldus_data$Time[nrow(noldus_data)]))

  noldus_data = noldus_data %>% select(Date, Time, Behavior, METs, Modifier_2, Omit_me) %>% dplyr::rename(Modifier_1 = METs) %>%
    mutate(Date = as.character(Date), Time =as.character(Time))

  do_name_append = str_split(basename(do_filepath), ' - ', simplify = T)[,2]

  for(aaa in 1:length(ag_filepath)){
    ag_data = read_ag(ag_filepath[aaa], ENMO_calibrate = T, device_serial_calibrate = T, calibration_file = device_serial_calibrate_df, parse_timestamp = F)

    ag_data = ag_data %>% dplyr::mutate(Full_date = ymd_hms(str_c(Date, Time, sep = ' '))) %>%
      filter(inrange(Full_date, noldus_start, noldus_end_raw))%>% select(-Full_date)

    if(nrow(ag_data)%%samp_freq!=0){
      excess_remainder = nrow(ag_data)%%samp_freq
      ag_data = ag_data[-((nrow(ag_data)-excess_remainder):nrow(ag_data)),]
    }


    ag_data$Date = as.character(ag_data$Date)
    ag_data = left_join(ag_data, noldus_data)
    ag_data = cbind(Participant = participant_id, ag_data)

    ag_name_append = str_replace(str_split(basename(ag_filepath[aaa]), participant_id, simplify = T)[,2], '.csv', '')

    if(export == T){
      saveRDS(ag_data, paste(output_filepath, '/', do_name_append, ag_name_append, '.rds', sep = ''))
    } else {
      if(aaa == 1){
        ag_export = list(ag_data)
      } else {
        ag_export[[aaa]] = ag_data
      }
    }
  }

  return(ag_export)

}
