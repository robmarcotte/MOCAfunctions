# AG_DO_merge
#
# Function to automate the process of merging ActiGraph and Noldus direct observation data
# Assumes that screening of observations has already occurred and either a default do_fix data or a custom filepath with how to approach novel observations is provided. See DO_screen for further details.
#
# Library dependencies: foreach, doParallel, stringr, lubridate, dplyr, tidyr, data.table, readxl

AG_DO_merge = function(ag_filepaths, do_filepaths, timestamps, do_time_indicator, ag_do_indicator, samp_freq = 80,
                       do_fix_reference = c('18to20','15to17.9','13to14.9','10to12.9','6to9.9','3to5.9','1.5to2.9','custom'), do_fix_custom_filepath,
                       output_filepath,
                       runparallel = FALSE, cores = NA){

  if(length(do_time_indicator) != length(do_filepaths)){
    stop('Number of do_filepaths supplied do not equal the do_time_indicator. This will result in errors, please check them to make sure every filepath has a timestamp indicator present.')
    }

  do_fix = switch(do_fix_reference,
                      '18to20' = do_fix_18to20,
                      '15to17.9' = readRDS('filepath to 15to17.9 DO errors data'),
                      '13to14.9' = readRDS('filepath to 13to14.9 DO errors data'),
                      '10to12.9' = readRDS('filepath to 10to12.9 DO errors data'),
                      '6to9.9' = readRDS('filepath to 6to9.9 DO errors data'),
                      '3to5.9' = readRDS('filepath to 3to5.9 DO errors data'),
                      '1.5to2.9' = readRDS('filepath to 1.5to2.9 DO errors data'),
                      'custom' = readRDS(do_fix_custom_filepath))

  # Parallel operations for a later date - RM 12/15/2020
  if(runparallel == TRUE){

    cores = ifelse(is.na(cores), detectCores()/2, cores)

    if(cores > detectCores()){
      warning('Desired number of cores is greater than actual cores available on machine. Running parallel processes using the actual cores available on machine.')
    }
  } else {
    cores = 1
  }

  # Parallel operations for a later date - RM 12/15/2020
  # foreach(iii = 1:length(ag_do_indicator), .packages = c('tidyr','stringr','lubridate','dplyr', 'readxl', 'doParallel','foreach','data.table')) %dopar% {

  for(iii in 1:length(ag_do_indicator)){
    ag_index = str_which(ag_filepaths, ag_do_indicator[iii])
    do_index = str_which(do_filepaths, ag_do_indicator[iii])

    cl = makeCluster(cores)

    registerDoParallel(cl)

    # Read in and timestamp DO data
    foreach(jjj = 1:length(do_index), .packages = c('tidyr','stringr','lubridate','dplyr', 'readxl', 'doParallel','foreach','data.table', 'MOCAfunctions', 'ggplot2')) %dopar% {
    # for(jjj in 1:length(do_index)){
      time_index = str_which(timestamps$participant, do_time_indicator[do_index[jjj]])

      session_start_time = timestamps$start[time_index]
      session_date = ymd(timestamps$date[time_index])

      noldus_data = MOCAfunctions::DO_descriptives(do_filepaths[do_index[jjj]])
      noldus_data = MOCAfunctions::DO_msec_transform(noldus_data) # expand DO data to hundredths of seconds
      noldus_data = MOCAfunctions::DO_timestamp(noldus_data, session_start_time)
      noldus_data$index = rep(seq(1, ceiling(nrow(noldus_data)/100)), each = 100)[1:nrow(noldus_data)]

      noldus_data = data.frame(Participant = do_time_indicator[iii],
                               Date = session_date,
                               Time = noldus_data$Time[seq(1, (nrow(noldus_data))+1, by = 100)][1:length(unique(noldus_data$index))],
                               Behavior = tapply(noldus_data$Behavior, INDEX = noldus_data$index, FUN =  MOCAfunctions::majority_string),
                               METs = tapply(noldus_data$METs, INDEX = noldus_data$index, FUN =  MOCAfunctions::majority_string),
                               Modifier_2 = tapply(noldus_data$Modifier_2, INDEX = noldus_data$index, FUN =  MOCAfunctions::majority_string),
                               Modifier_3 = tapply(noldus_data$Modifier_3, INDEX = noldus_data$index, FUN =  MOCAfunctions::majority_string),
                               MET.level = tapply(noldus_data$MET.level, INDEX = noldus_data$index, FUN =  MOCAfunctions::majority_string))

      noldus_data =  MOCAfunctions::DO_cleaning_18to20(noldus_data, do_fix)

      noldus_data$Time =  MOCAfunctions::strip_time_from_fulldate(noldus_data$Time)

      noldus_start = ymd_hms(str_c(noldus_data$Date[1], noldus_data$Time[1]))
      noldus_end_raw = ymd_hms(str_c(noldus_data$Date[1], noldus_data$Time[nrow(noldus_data)])) + seconds(1)
      noldus_end_count = ymd_hms(str_c(noldus_data$Date[1], noldus_data$Time[nrow(noldus_data)]))

      noldus_data = noldus_data %>% select(Date, Time, Behavior, METs, Modifier_2, Omit_me) %>% dplyr::rename(Modifier_1 = METs) %>%
        mutate(Date = as.character(Date), Time =as.character(Time))

      do_name_append = str_split(do_time_indicator[do_index[jjj]], ag_do_indicator[iii], simplify = T)[,2]

      # Read in ActiGraph data, append DO data, export
      # foreach(aaa = 1:length(ag_index), .packages = c('tidyr','stringr','lubridate','dplyr', 'readxl', 'doParallel','foreach','data.table', 'MOCAfunctions')) %dopar% {
      for(aaa in 1:length(ag_index)){
        ag_data = read_ag(ag_filepaths[ag_index[aaa]], ENMO_calibrate = F, device_serial_calibrate = F)

        ag_data = ag_data %>% dplyr::mutate(Full_date = ymd_hms(str_c(Date, Time, sep = ' '))) %>%
          filter(inrange(Full_date, noldus_start, noldus_end_raw))%>% select(-Full_date)
        if(nrow(ag_data)%%samp_freq!=0){
          excess_remainder = nrow(ag_data)%%samp_freq
          ag_data = ag_data[-((nrow(ag_data)-excess_remainder):nrow(ag_data)),]
        }


        ag_data = left_join(ag_data, noldus_data)
        ag_data = cbind(Participant = do_time_indicator[iii], ag_data)

        ag_name_append = str_replace(str_split(basename(ag_filepaths[ag_index[aaa]]), '\\.', simplify = T)[,1], ag_do_indicator[iii], '')

        saveRDS(ag_data, paste(output_filepath, '/', ag_do_indicator[iii], do_name_append, ag_name_append, '.rds', sep = ''))

    }

    # Parallel operations for a later date - RM 12/15/2020
    }
    stopCluster(cl)

  }
}
