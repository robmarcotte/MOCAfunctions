#' Function to re-integrate 1-second actigraph count data into a larger epoch
#' @param ag_data_1sec ActiGraph count data exported in 1-second format
#' @param epoch Numeric integer for the desired reintegrated epoch length
#'
#'
#'
ag_epochr = function(ag_data_1sec,epoch = 60){

  rows = nrow(ag_data_1sec)
  new_rows = ceiling(rows/epoch)

  ag_data_1sec$index = rep(seq(1, new_rows, by = 1), each = epoch)[1:rows]

  first_only_colnames = which(str_detect(colnames(ag_data_1sec), paste('file','stamp', 'Date','Time',sep = '|')))

  epoch_data = ag_data_1sec[seq(1, nrow(ag_data_1sec), by = epoch), first_only_colnames]

  count_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                     Axis1 = sum(Axis1, na.rm = T),
                                                                     Axis2 = sum(Axis2, na.rm = T),
                                                                     Axis3 = sum(Axis3, na.rm = T)) %>% select(-index) %>%
    mutate(VM = sqrt(Axis1^2 + Axis2^2+ Axis3^2))

  epoch_data = left_join(epoch_data, count_data)

  # If there's step data,  reaggregate to epoch level
  if(any(str_detect(colnames(ag_data_1sec), 'Step'))){
    step_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                       Steps = sum(Steps, na.rm = T)) %>% select(-index)

    epoch_data = left_join(epoch_data, step_data)
  }

  # If there's inclinometer data, reaggregate to epoch level
  if(any(str_detect(colnames(ag_data_1sec), 'Inclinometer'))){
    inclinometer_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                      `Inclinometer Off` = sum(`Inclinometer Off`, na.rm = T),
                                                                      `Inclinometer Standing` = sum(`Inclinometer Standing`, na.rm = T),
                                                                      `Inclinometer Sitting` = sum(`Inclinometer Sitting`, na.rm = T),
                                                                      `Inclinometer Lying` = sum(`Inclinometer Lying`, na.rm = T)) %>%
      select(-index)

    epoch_data = left_join(epoch_data, inclinometer_data)
  }

  # If there's lux data, reaggregate to epoch level
  if(any(str_detect(colnames(ag_data_1sec), 'Lux'))){
    lux_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                              Lux = mean(Lux, na.rm =T)) %>%
      select(-index)

    epoch_data = left_join(epoch_data, lux_data)
  }

  # if theres LFE data, repeat all steps for count axes and step data
  if(any(str_detect(colnames(ag_data_1sec), 'LFE'))){
    count_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                       Axis1_LFE = sum(Axis1_LFE, na.rm = T),
                                                                       Axis2_LFE = sum(Axis2_LFE, na.rm = T),
                                                                       Axis3_LFE = sum(Axis3_LFE, na.rm = T)) %>% select(-index) %>%
      mutate(VM_LFE = sqrt(Axis1^2 + Axis2^2+ Axis3^2))

    epoch_data = left_join(epoch_data, count_data)

    # If there's step data, also reaggregate to epoch level
    if(any(str_detect(colnames(ag_data_1sec), 'Step'))){
      step_data = ag_data_1sec %>% group_by(index) %>% dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                                                                        Steps_LFE = sum(Steps_LFE, na.rm = T)) %>% select(-index)

      epoch_data = left_join(epoch_data, step_data)
    }


  }

  epoch_data$VM = sqrt(epoch_data$Axis1^2 + epoch_data$Axis2^2 + epoch_data$Axis3^2)

  return(epoch_data)

}
