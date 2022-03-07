#' Soj-g function from Marcotte et al 2021
#'
#' @param data A dataframe with raw wrist accelerometer data with column names Timestamp, AxisX, AxisY, AxisZ, VM
#' @param freq Sampling frequency of raw accelerometer data. 80 Hz by default
#' @param step1_sd_threshold Threshold used to classify likely inactive periods
#' @param step2_nest_length Window length for classifying inactive vs active using random forest model
#' @param step3_nest_length Window length for partitioning excessively long sojourns as determined by step3_orig_soj_length_min
#' @param step3_orig_soj_length_min Maximum sojourn window duration allowed until partitioning into nested sojourns
#' @param min_soj_length Minimum sojourn window duration allowed. Any sojourns smaller will be combined with the
#'
#' @example
#'
#' Library Dependencies: matrixStats, data.table, zoo, dplyr, randomForest, tools, AGread

soj_g = function(data = NA, export_format = 'session', freq = 80, step1_sd_threshold = .00375, step2_min_window_length = 0, step2_nest_length = 5, step3_nest_length = 60, step3_orig_soj_length_min = 180){

  # Remove last partial fraction of a second if number of observations is not a clean multiple of the sampling frequency
  if(nrow(data)%%freq!= 0){
    data = data[1:(nrow(data)-(nrow(data)%%freq)),]
  }

  #
  # Step 1 - Identify likely inactive periods----
  #
  message(paste0('...Identifying likely inactive periods using sd_vm threshold = ', step1_sd_threshold))
  data$index = rep(1:ceiling(nrow(data)/freq), each = freq)[1:nrow(data)]
  data_summary = data %>% group_by(index) %>% dplyr::summarize(sd_vm = sd(VM, na.rm = T))
  data_summary$step1_estimate = ifelse(data_summary$sd_vm <=step1_sd_threshold, 1, 0) # 1 = inactive, 0 = unclassified

  seconds_index = seq(1, nrow(data), by = freq)
  diffs = which((dplyr::lag(data_summary$step1_estimate) != data_summary$step1_estimate) == T)
  diffs = c(1, diffs)
  data_summary$step2_sojourn_index = NA
  data_summary$step2_sojourn_duration = NA

  #
  # Step 2 - Segment remaining unlabeled periods into smaller windows, identify whether inactive or active----
  #
  message('...Segmenting remaining unlabeled periods into smaller windows')
  data_summary$step2_sojourn_index = data.table::rleid(data_summary$step1_estimate)
  data_summary$step2_sojourn_duration[diffs] = rle(data_summary$step2_sojourn_index)[[1]]
  data_summary$step2_sojourn_duration = zoo::na.locf(data_summary$step2_sojourn_duration)
  data_summary = data_summary %>% group_by(step2_sojourn_index) %>%
    mutate(step2_sojourn_index = nest_sojourn(step2_sojourn_index, orig_soj_length_min = step2_nest_length, nest_length = step2_nest_length))
  # data_summary$step2_sojourn_index = sort(unlist(tapply(data_summary$step2_sojourn_index, data_summary$step2_sojourn_index, nest_sojourn, nest_length = step2_nest_length)))

  # Repopulate original data with step1 and 2 sojourn ID
  data$VM_sd_1sec = rep(data_summary$sd_vm, each = freq)[1:nrow(data)]
  data$step1_estimate = rep(data_summary$step1_estimate, each = freq)[1:nrow(data)]
  data$step2_sojourn_index = rep(data_summary$step2_sojourn_index, each = freq)[1:nrow(data)]
  data$step2_sojourn_duration = rep(data_summary$step2_sojourn_duration, each = freq)[1:nrow(data)]

  # Compute features within nested sojourns
  message('...Computing signal features in nested sojourn windows')
  ag_step2_summary = ag_feature_calc(data %>% dplyr::rename(sojourn = step2_sojourn_index,
                                                            seconds = step2_sojourn_duration), samp_freq = freq, window = 'sojourns') # , soj_colname = 'step2_sojourn_index', seconds_colname = 'step2_sojourn_duration')

  ag_step2_summary = ag_step2_summary %>% dplyr::rename(step2_sojourn_index = sojourn,
                                                        step2_sojourn_duration = seconds)
  ag_step2_summary$step2_durations = rle(data_summary$step2_sojourn_index)[[1]]
  ag_step2_summary$seconds = ag_step2_summary$step2_durations
  ag_step2_summary$step2_estimate = predict(MOCAModelData::sojg_stage2_unclassified_rf, newdata = ag_step2_summary, type = 'class')

  ag_step2_summary = ag_step2_summary %>% dplyr::select(-seconds)

  # Append the step2 activity state estimate to the 1-sec summary dataframe
  data_summary$step2_estimate = rep(ag_step2_summary$step2_estimate, times = ag_step2_summary$step2_durations)
  data_summary$step3_sojourn_index = NA
  data_summary$step3_sojourn_duration = NA
  diffs = which((dplyr::lag(data_summary$step2_estimate) != data_summary$step2_estimate) == T)
  diffs = c(1, diffs)

  # Verify that labels under step2_estimate are character dummy variables
  if(all(!is.na(as.numeric(levels(data_summary$step2_estimate)))))
    data_summary$step2_estimate = factor(data_summary$step2_estimate, levels = c(1,2), labels =c('Stationary','Active'))
  data_summary$step3_sojourn_index = data.table::rleid(data_summary$step2_estimate)
  data_summary$step3_sojourn_duration[diffs] = rle(data_summary$step3_sojourn_index)[[1]]
  data_summary$step3_sojourn_duration =zoo::na.locf(data_summary$step3_sojourn_duration)

  message('...Looking for too-short sojourn windows')
  if(any(data_summary$step3_sojourn_duration < step2_min_window_length)){
    too_short = data_summary %>% group_by(step2_estimate, step3_sojourn_index, step3_sojourn_duration) %>% dplyr::summarize(n = n()) %>%
      dplyr::arrange(step3_sojourn_index)

    # First, combine string of short sojourns together
    too_short$too_short = too_short$step3_sojourn_duration<step2_min_window_length
    too_short$too_short_index = data.table::rleid(too_short$too_short)

    # This section may introduce time jumbling. Verify this works fine with other min_window lengths
    temp = too_short %>% dplyr::group_by(too_short_index, too_short) %>% dplyr::filter(too_short == T) %>%
      dplyr::summarize(step3_sojourn_index = dplyr::first(step3_sojourn_index),
                       step3_sojourn_duration = sum(step3_sojourn_duration),
                       step2_estimate = factor(max(as.numeric(step2_estimate), na.rm = T), levels = c(1,2), labels = c('Stationary','Active')))


    too_short_updated = bind_rows(too_short %>% dplyr::filter(too_short == F), temp) %>% dplyr::arrange(step3_sojourn_index)

    # too_short_updated = too_short %>% dplyr::group_by(too_short_index, too_short) %>% dplyr::summarize(step3_sojourn_duration = sum(step3_sojourn_duration),
    #                                                                                                    step2_estimate = factor(max(as.numeric(step2_estimate), na.rm = T), levels = c(1,2), labels = c('Stationary','Active')))

    # Second, check to see if there are any remaining sojourns that are still too short
    if(any(too_short_updated$step3_sojourn_duration < step2_min_window_length)){
      too_short_updated$too_short = too_short_updated$step3_sojourn_duration < step2_min_window_length
      too_short_updated$neighbor_lag = dplyr::lag(too_short_updated$step3_sojourn_duration)
      too_short_updated$neighbor_lead = dplyr::lead(too_short_updated$step3_sojourn_duration)

      too_short_updated$updated_duration = too_short_updated$step3_sojourn_duration

      too_short_indices = which(too_short_updated$too_short == T)

      for(i in 1:length(too_short_indices)){
        if(too_short_updated$neighbor_lag[too_short_indices[i]] <= too_short_updated$neighbor_lead[too_short_indices[i]]){
          too_short_updated$updated_duration[(too_short_indices[i]-1)] = too_short_updated$updated_duration[(too_short_indices[i]-1)] + too_short_updated$step3_sojourn_duration[(too_short_indices[i])]
        } else {
          too_short_updated$updated_duration[(too_short_indices[i]+1)] = too_short_updated$updated_duration[(too_short_indices[i]+1)] + too_short_updated$step3_sojourn_duration[(too_short_indices[i])]
        }
      }

      too_short_updated$perc.original.duration = too_short_updated$step3_sojourn_duration/too_short_updated$updated_duration
      # If the original sojourn length is not >= 70% of the newly merged duration, assign the estimate to be the other intensity (Stationary vs Active)
      too_short_updated$step2_estimate = ifelse(too_short_updated$step3_sojourn_duration/too_short_updated$updated_duration< .7,
                                                        ifelse(too_short_updated$step2_estimate == 'Stationary', 2,too_short_updated$step2_estimate),
                                                        too_short_updated$step2_estimate)
      too_short_updated$step2_estimate = factor(too_short_updated$step2_estimate, levels = c(1,2), labels = c('Stationary','Active'))

      too_short_updated = too_short_updated %>% dplyr::select(-step3_sojourn_duration) %>% rename(step3_sojourn_duration = updated_duration)

      too_short_updated = too_short_updated[-too_short_indices,]

    }

    too_short_updated$step3_sojourn_index = data.table::rleid(too_short_updated$step2_estimate)

    too_short_updated = too_short_updated %>% group_by(step3_sojourn_index, step2_estimate) %>% dplyr::summarize(step3_sojourn_duration = sum(step3_sojourn_duration))

    data_summary$step3_sojourn_index = rep(too_short_updated$step3_sojourn_index, times = too_short_updated$step3_sojourn_duration)[1:nrow(data_summary)]
    data_summary$step3_sojourn_state = rep(too_short_updated$step2_estimate, times = too_short_updated$step3_sojourn_duration)[1:nrow(data_summary)]
    data_summary$step3_sojourn_duration = rep(too_short_updated$step3_sojourn_duration, times = too_short_updated$step3_sojourn_duration)[1:nrow(data_summary)]

  } else {
    data_summary$step3_sojourn_state = step2_estimate
  }

  data_summary = data_summary %>% group_by(step3_sojourn_index) %>% mutate(step3_sojourn_index = nest_sojourn(step3_sojourn_index, orig_soj_length_min = step3_orig_soj_length_min, nest_length = step3_nest_length))
  # data_summary$step3_sojourn_index = sort(unlist(tapply(data_summary$step3_sojourn_index, data_summary$step3_sojourn_index, nest_sojourn2, orig_soj_length_min = step3_orig_soj_length_min, nest_length = step3_nest_length)))

  data$step2_estimate = rep(data_summary$step2_estimate, each = freq)[1:nrow(data)]
  data$step3_sojourn_index = rep(data_summary$step3_sojourn_index, each = freq)[1:nrow(data)]
  data$step3_sojourn_duration = rep(data_summary$step3_sojourn_duration, each = freq)[1:nrow(data)]

  # Compute features in final sojourns
  ag_step3_summary = ag_feature_calc(data %>% dplyr::rename(sojourn = step3_sojourn_index,
                                                            seconds = step3_sojourn_duration), samp_freq = freq, window = 'sojourns') #, soj_colname = 'step3_sojourn_index', seconds_colname = 'step3_sojourn_duration')

  ag_step3_summary = ag_step3_summary %>% dplyr::rename(step3_sojourn_index = sojourn,
                                                        step3_sojourn_duration = seconds)
  ag_step3_summary$step3_durations = rle(data_summary$step3_sojourn_index)[[1]]
  final_step2_estimate = data_summary %>% group_by(step3_sojourn_index) %>%
    summarize(step2_estimate = dplyr::first(step2_estimate)) %>% select(step2_estimate) %>% ungroup() %>% as.vector()
  ag_step3_summary$step2_estimate = final_step2_estimate$step2_estimate

  ag_step3_summary$seconds = ag_step3_summary$step3_sojourn_duration
  ag_step3_summary$step3_estimate_intensity = predict(MOCAModelData::sojg_stage3_intensity_rf, newdata = ag_step3_summary, type = 'class')
  # ag_step3_summary$step3_estimate_type = predict(MOCAModelData::sojg_stage3_activity_rf, newdata = ag_step3_summary, type = 'class')
  # ag_step3_summary$step3_estimate_locomotion = predict(MOCAModelData::sojg_stage3_locomotion_rf, newdata = ag_step3_summary, type = 'class')

  if(export_format == 'session'){
    session_summary = data.frame(Date = NA, Total_minutes = NA, Sedentary = NA, Light = NA, Moderate = NA, Vigorous = NA)
    temp = ag_step3_summary %>% dplyr::ungroup() %>% dplyr::mutate(Date = lubridate::date(Timestamp)) %>%
      group_by(Date, step3_estimate_intensity) %>% dplyr::summarize(minutes = round(sum(step3_durations)/60, 2)) %>%
      tidyr::spread(step3_estimate_intensity, minutes)

    session_summary = bind_rows(session_summary, temp) %>% dplyr::filter(!is.na(Date))
    session_summary = session_summary %>% dplyr::ungroup() %>%
      tidyr::replace_na(replace = list(Sedentary = 0,
                                       Light = 0,
                                       Moderate = 0,
                                       Vigorous = 0)) %>%
      dplyr::rowwise() %>% dplyr::mutate(Total_minutes = rowSums(across(Sedentary:Vigorous)),
                                         MVPA = Moderate + Vigorous)

    return(session_summary)
  }

  if(export_format == 'sojourn'){
    return(ag_step3_summary)
  }

  if(export_format == 'seconds'){
    data_summary$step3_estimate_intensity = rep(ag_step3_summary$step3_estimate_intensity, times = ag_step3_summary$step3_durations)
    # data_summary$step3_estimate_type = rep(ag_step3_summary$step3_estimate_type, times = ag_step3_summary$step3_durations)
    # data_summary$step3_estimate_locomotion = rep(ag_step3_summary$step3_estimate_locomotion, times = ag_step3_summary$step3_durations)

    data_summary$Timestamp = data$Timestamp[seq(1, nrow(data), by = freq)[1:nrow(data_summary)]]
    data_summary = data_summary %>% dplyr::relocate(Timestamp)

    return(data_summary)
  }

  if(export_format == 'raw'){
    data_summary$step3_estimate_intensity = rep(ag_step3_summary$step3_estimate_intensity, times = ag_step3_summary$step3_durations)
    # data_summary$step3_estimate_type = rep(ag_step3_summary$step3_estimate_type, times = ag_step3_summary$step3_durations)
    # data_summary$step3_estimate_locomotion = rep(ag_step3_summary$step3_estimate_locomotion, times = ag_step3_summary$step3_durations)

    data$step3_estimate_intensity = rep(data_summary$step3_estimate_intensity, each = freq)[1:nrow(data)]
    # data$step3_estimate_type = rep(data_summary$step3_estimate_type, each = freq)[1:nrow(data)]
    # data$step3_estimate_locomotion = rep(data_summary$step3_estimate_locomotion, each = freq)[1:nrow(data)]

    return(data)
  }

}
