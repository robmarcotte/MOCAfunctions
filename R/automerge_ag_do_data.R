# automerge_ag_do_data
#
# Function to automate the process of merging ActiGraph and Noldus direct observation data
# Note: do_screen as 'sequential' can only be completed if doparallel is FALSE. Otherwise, it has to be done as batch
#
# Library dependencies: foreach, doParallel, stringr, lubridate, dplyr, tidyr

automerge_ag_do_data = function(ag_filepaths = NA, do_filepaths = NA, timestamps = NA, unique_indicator = NA,
                                output_filepath = NA, visual_plots = TRUE,
                                do_screen = c('none','sequential','batch'), doparallel = TRUE, cores = NA){

  if(doparallel == TRUE){

    cores = ifelse(is.na(cores), detectCores()/2, cores)

    if(cores > detectCores()){
      warning('Desired number of cores is greater than actual cores available on machine. Running parallel processes using the actual cores available on machine.')
    }
  }
  else {
    cores = 1
  }

  cl = makeCluster(cores)

  registerDoParallel(cl)

  if(doparallel == T & do_screen == 'batch'){
    do_data = foreach(iii = 1:length(do_filepaths), .packages = c('MOCAfunctions','tidyr','stringr','lubridate','dplyr'),
                      .combine = 'rbind') %dopar% {
                        noldus_data = DO_descriptives(do_filepaths[iii])
                      }

    do_data = DO_cleaning_18to20(do_data, manual_fix)

    # Find unique coding combinations
    do_unique = do_data %>% group_by(Behavior, Modifier2, Modifier1) %>% dplyr::summarize(n = n())

  }

  foreach(iii = 1:length(unique_indicator), .packages = c('tidyr','stringr','lubridate','dplyr')) %dopar% {
    ag_index = str_which(ag_filepaths, unique_indicator[iii])
    do_index = str_which(do_filepaths, unique_indicator[iii])
    time_index = str_which(timestamps$participant, unique_indicator[iii])

    session_start_time = timestamps$start[time_index]
    session_date = ymd(timestamps$date[time_index])

    # Read in and timestamp DO data
    for(jjj in 1:length(do_index)){
      noldus_data = DO_descriptives(do_filepaths[do_index[jjj]])
      noldus_data = DO_msec_transform(noldus_data) # expand DO data to hundredths of seconds
      noldus_data = DO_timestamp(noldus_data, session_start_time)
      noldus_data$index = rep(seq(1, ceiling(nrow(noldus_data)/100)), each = 100)[1:nrow(noldus_data)]

      noldus_data = data.frame(Participant = unique_indicator[iii],
                               Date = session_date,
                               Time = noldus_data$Time[seq(1, (nrow(noldus_data))+1, by = 100)][1:length(unique(noldus_data$index))],
                               Behavior = tapply(noldus_data$Behavior, INDEX = noldus_data$index, FUN = majority_string),
                               METs = tapply(noldus_data$METs, INDEX = noldus_data$index, FUN = majority_string),
                               Modifier_2 = tapply(noldus_data$Modifier_2, INDEX = noldus_data$index, FUN = majority_string),
                               Modifier_3 = tapply(noldus_data$Modifier_3, INDEX = noldus_data$index, FUN = majority_string),
                               MET.level = tapply(noldus_data$MET.level, INDEX = noldus_data$index, FUN = majority_string))

      noldus_data$Time = strip_time_from_fulldate(noldus_data$Time)

      # Read in ActiGraph data, append DO data, export
      for(aaa in 1:length(ag_index)){
        ag_data = read_ag(ag_filepaths[ag_index[aaa]])


      }
    }
  }

}
