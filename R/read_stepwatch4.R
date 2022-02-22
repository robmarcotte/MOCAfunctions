# Read Stepwatch4 Data
#
# Accepts the folder that contains the bin files and exports data

read_stepwatch4 = function(primary_folderpath, bin_select = c('1 Min','15 Sec','10 Sec')){

  folders = dir(primary_folderpath, full.names = T)

  if(length(bin_select) >1){
    message('...Bin length not selected. Defaulting to 1 minute bin...')
    bin_select = '1 Min'
  }

  if(all(str_detect(folders, bin_select) == F)){
    stop(paste0('Folder for ', bin_select, ' bin length not found.'))

  }

  # Determine bin duration in seconds
  bin_duration = as.numeric(str_split(bin_select, ' ', simplify = T)[,1])

  if(str_detect(str_to_lower(bin_select), 'min')){
    bin_duration = bin_duration*60
  }

  primary_dir = folders[str_which(folders, bin_select)]

  filepaths = dir(primary_dir, full.names = T)

  for(i in 1:length(filepaths)){

    temp_data = data.table::fread(filepaths[i])

    temp_data = temp_data %>% dplyr::rename(Timestamp = BINNED_STEPS_TIMESTAMP,
                                            Stepwatch_Steps = BINNED_STEPS_STEPS_IN_BIN) %>%
      dplyr::select(Timestamp, Stepwatch_Steps)

    temp_data = temp_data %>% dplyr::mutate(Timestamp = lubridate::ymd_hms(Timestamp),
                                            # Round timestamp to nearest
                                            Timestamp = lubridate::round_date(Timestamp, paste0(bin_duration, ' seconds')))

    if(any(is.na(temp_data$Timestamp))){
      temp_data = temp_data %>% dplyr::mutate(Timestamp_temp = seq(Timestamp[1], (Timestamp[1] + lubridate::days(1)), by = paste0(bin_duration, ' secs'))[1:nrow(temp_data)],
                                              Timestamp = Timestamp_temp) %>%
        dplyr::select(-Timestamp_temp) %>% dplyr::filter(!is.na(Timestamp),
                                                         !is.na(Stepwatch_Steps))
    }


    if(i == 1){
      stepwatch_data = temp_data
    } else {
      stepwatch_data = dplyr::bind_rows(stepwatch_data, temp_data)
    }

  }
  return(stepwatch_data)

}
