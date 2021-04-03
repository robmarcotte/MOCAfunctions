IMU_DO_merge = function(imu_filepath, do_filepath, timestart, samp_freq = 100, participant_id,
                       do_fix_reference = c('18to20','15to17.9','13to14.9','10to12.9','6to9.9','3to5.9','1.5to2.9','custom'), do_fix_custom_filepath,
                       output_filepath, imu_meta_skip = 10){

  do_fix = switch(do_fix_reference,
                  '18to20' = do_fix_18to20,
                  '15to17.9' = readRDS('filepath to 15to17.9 DO errors data'),
                  '13to14.9' = readRDS('filepath to 13to14.9 DO errors data'),
                  '10to12.9' = readRDS('filepath to 10to12.9 DO errors data'),
                  '6to9.9' = readRDS('filepath to 6to9.9 DO errors data'),
                  '3to5.9' = readRDS('filepath to 3to5.9 DO errors data'),
                  '1.5to2.9' = readRDS('filepath to 1.5to2.9 DO errors data'),
                  'custom' = readRDS(do_fix_custom_filepath))

  session_start_time = timestart
  session_date = ymd(str_split(session_start_time, ' ', simplify = T)[,1])

  noldus_data = MOCAfunctions::DO_descriptives(do_filepath)
  noldus_data = MOCAfunctions::DO_msec_transform(noldus_data) # expand DO data to hundredths of seconds
  noldus_data = MOCAfunctions::DO_timestamp(noldus_data, session_start_time)

  noldus_data$index = rep(seq(1, ceiling(nrow(noldus_data)/100)), each = 100)[1:nrow(noldus_data)]

  noldus_data = noldus_data %>% group_by(index) %>% summarize(Participant = dplyr::first(Participant),
                                                              Date = dplyr::first(Date),
                                                              Time = dplyr::first(Time),
                                                              Behavior = names(which.max(table(Behavior, useNA = 'ifany'))),
                                                              METs = names(which.max(table(METs, useNA = 'ifany'))),
                                                              Modifier_2 = names(which.max(table(Modifier_2, useNA = 'ifany'))),
                                                              Modifier_3 = names(which.max(table(Modifier_3, useNA = 'ifany'))),
                                                              MET.level = names(which.max(table(MET.level, useNA = 'ifany'))))


  noldus_data =  MOCAfunctions::DO_cleaning_18to20(noldus_data, do_fix)

  noldus_data$Time =  MOCAfunctions::strip_time_from_fulldate(noldus_data$Time)

  noldus_start = ymd_hms(str_c(noldus_data$Date[1], noldus_data$Time[1]))
  noldus_end_raw = ymd_hms(str_c(noldus_data$Date[1], noldus_data$Time[nrow(noldus_data)])) + seconds(1)
  noldus_end_count = ymd_hms(str_c(noldus_data$Date[1], noldus_data$Time[nrow(noldus_data)]))

  noldus_data = noldus_data %>% select(Date, Time, Behavior, METs, Modifier_2, Omit_me) %>% dplyr::rename(Modifier_1 = METs) %>%
    mutate(Date = as.character(Date), Time =as.character(Time))

  do_name_append = str_split(basename(do_filepath), ' - ', simplify = T)[,2]

  for(aaa in 1:length(imu_index)){
    imu_data = fread(imu_filepaths[imu_index[aaa]], skip = imu_meta_skip)
    imu_data$Timestamp = ymd_hms(imu_data$Timestamp)

    imu_data = imu_data %>%
      filter(inrange(Timestamp, noldus_start, noldus_end_raw))%>% mutate(Date = str_split(Timestamp, ' ', simplify = T)[,1], Time = str_split(Timestamp, ' ', simplify = T)[,2])

    if(nrow(imu_data)%%samp_freq!=0){
      excess_remainder = nrow(imu_data)%%samp_freq
      imu_data = imu_data[-((nrow(imu_data)-excess_remainder):nrow(imu_data)),]
    }


    imu_data = left_join(imu_data, noldus_data)
    imu_data = cbind(Participant = do_time_indicator[iii], imu_data)

    imu_name_append = str_split(basename(imu_filepath), participant_id, simplify = T)[,2]

    saveRDS(imu_data, paste(output_filepath, '/', do_name_append, imu_name_append, '.rds', sep = ''))

  }

}

