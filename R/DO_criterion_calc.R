# DO_criterion_calc
#
# Function to extract either
# Library Dependencies: dplyr

DO_criterion_calc = function(noldus_data, participant, samp_freq = 1, window = 15, soj_colname = NA, seconds_colname = NA,
                             intensity = T, type = T, locomotion = T, export_format = 'window'){
  n <- dim(noldus_data)[1]

  if(window == 'sojourns'){
    # If sojourn windows are used, it is assumed that a column already exists in the data provided that identifies the sojourn indices
    soj_colindex = which(colnames(noldus_data) == soj_colname)
    seconds_colindex = which(colnames(noldus_data) == seconds_colname)

    # Compute majority direct observations within sojourns
    noldus_data.sum = noldus_data %>% dplyr::group_by_at(soj_colindex) %>% dplyr::summarize(sojourn_index = dplyr::first(.[[soj_colindex]]), # dplyr::first(step3_sojourn_index),
                                                                                            sojourn_duration = dplyr::first(.[[seconds_colindex]]), # dplyr::first(step3_sojourn_duration),
                                                                                            Behavior = majority_string(Behavior, piece = 'coded_strings'),
                                                                                            Modifier_1 = majority_string(Modifier_1, piece = 'coded_strings'),
                                                                                            Modifier_2 = majority_string(Modifier_2, piece = 'coded_strings'),
                                                                                            Omit_me = majority_string(Omit_me, piece = 'coded_strings'))

    if(intensity == T){

      noldus_data$Criterion_Intensity = ifelse(((noldus_data$Behavior == 'Sitting' |
                                                   noldus_data$Behavior == 'Lying') &
                                                       (as.numeric(noldus_data$Modifier_1) <=1.5)),
                                                    'Sedentary',
                                                    as.character(cut(as.numeric(noldus_data$Modifier_1), breaks = c(0, 3, 6, Inf), labels = c('Light','Moderate','Vigorous'), include.lowest = F)))

      intensity_data = noldus_data %>% dplyr::group_by_at(soj_colindex) %>% dplyr::summarize(Criterion_Intensity = majority_string(Criterion_Intensity, piece = 'coded_strings'))

      noldus_data.sum = left_join(noldus_data.sum, intensity_data)
    }

    if(type == T){

      noldus_data$Criterion_Activity = ifelse((noldus_data$Behavior == 'Sitting' |
                                                      noldus_data$Behavior == 'Lying'), 'Sitting_Lying', NA)
      noldus_data$Criterion_Activity = ifelse((str_detect(str_to_lower(noldus_data$Behavior), pattern = "walk")== T), 'Walking', noldus_data$Criterion_Activity)
      noldus_data$Criterion_Activity = ifelse((str_detect(str_to_lower(noldus_data$Behavior), pattern = "run")== T), 'Running', noldus_data$Criterion_Activity)
      noldus_data$Criterion_Activity = ifelse(is.na(noldus_data$Criterion_Activity), 'Stationary+', noldus_data$Criterion_Activity)

      type_data = noldus_data %>% dplyr::group_by_at(soj_colindex) %>% dplyr::summarize(Criterion_Activity = majority_string(Criterion_Activity, piece = 'coded_strings'))

      noldus_data.sum = left_join(noldus_data.sum, type_data)
    }

    if(locomotion == T){
      noldus_data$Criterion_Locomotion = ifelse(((str_detect(str_to_lower(noldus_data$Behavior), pattern = "walk")== T) | (str_detect(str_to_lower(noldus_data$Behavior), pattern = "run")== T)), 'Locomotion', 'No Locomotion')
      locomotion_data = noldus_data %>% dplyr::group_by_at(soj_colindex) %>% dplyr::summarize(Criterion_Locomotion = majority_string(Criterion_Locomotion, piece = 'coded_strings'))

      noldus_data.sum = left_join(noldus_data.sum, locomotion_data)
    }

    if(export_format == 'window'){
      return(noldus_data.sum)
    }

    if(export_format == 'seconds'){

      nested_sojourn_lengths = table(noldus_data$step3_sojourn_index)/samp_freq
      seconds = data.frame(seconds = seq(1,n/samp_freq))

      noldus_data.sum = noldus_data.sum[rep(seq_len(nrow(noldus_data.sum)), times = nested_sojourn_lengths), ]

      noldus_data.sum = cbind(seconds, noldus_data.sum)

      return(noldus_data.sum)
    }

    if(export_format == 'raw'){
      return(noldus_data)
    }

  } else {

    epoch <- ceiling(n/(samp_freq*window))
    noldus_data$epoch <- rep(1:epoch,each=window*samp_freq)[1:n]

    noldus_data.sum = noldus_data %>% dplyr::group_by(epoch) %>% dplyr::summarize(epoch = dplyr::first(epoch),
                                                                                            Behavior = majority_string(Behavior, piece = 'coded_strings'),
                                                                                            Modifier_1 = majority_string(Modifier_1, piece = 'coded_strings'),
                                                                                            Modifier_2 = majority_string(Modifier_2, piece = 'coded_strings'),
                                                                                            Omit_me = majority_string(Omit_me, piece = 'coded_strings'))

    if(intensity == T){

      noldus_data$Criterion_Intensity = ifelse(((noldus_data$Behavior == 'Sitting' |
                                                   noldus_data$Behavior == 'Lying') &
                                                  (as.numeric(noldus_data$Modifier_1) <=1.5)),
                                               'Sedentary',
                                               as.character(cut(as.numeric(noldus_data$Modifier_1), breaks = c(0, 3, 6, Inf), labels = c('Light','Moderate','Vigorous'), include.lowest = F)))

      intensity_data = noldus_data %>% dplyr::group_by(epoch) %>% dplyr::summarize(Criterion_Intensity = majority_string(Criterion_Intensity, piece = 'coded_strings'))

      noldus_data.sum = left_join(noldus_data.sum, intensity_data)
    }

    if(type == T){

      noldus_data$Criterion_Activity = ifelse((noldus_data$Behavior == 'Sitting' |
                                                 noldus_data$Behavior == 'Lying'), 'Sitting_Lying', NA)
      noldus_data$Criterion_Activity = ifelse((str_detect(str_to_lower(noldus_data$Behavior), pattern = "walk")== T), 'Walking', noldus_data$Criterion_Activity)
      noldus_data$Criterion_Activity = ifelse((str_detect(str_to_lower(noldus_data$Behavior), pattern = "run")== T), 'Running', noldus_data$Criterion_Activity)
      noldus_data$Criterion_Activity = ifelse(is.na(noldus_data$Criterion_Activity), 'Stationary+', noldus_data$Criterion_Activity)

      type_data = noldus_data %>% dplyr::group_by(epoch) %>% dplyr::summarize(Criterion_Activity = majority_string(Criterion_Activity, piece = 'coded_strings'))

      noldus_data.sum = left_join(noldus_data.sum, type_data)
    }

    if(locomotion == T){
      noldus_data$Criterion_Locomotion = ifelse(((str_detect(str_to_lower(noldus_data$Behavior), pattern = "walk")== T) | (str_detect(str_to_lower(noldus_data$Behavior), pattern = "run")== T)), 'Locomotion', 'No Locomotion')
      locomotion_data = noldus_data %>% dplyr::group_by(epoch) %>% dplyr::summarize(Criterion_Locomotion = majority_string(Criterion_Locomotion, piece = 'coded_strings'))

      noldus_data.sum = left_join(noldus_data.sum, locomotion_data)
    }

    if(export_format == 'window'){
      return(noldus_data.sum)
    }

    if(export_format == 'seconds'){

      seconds = data.frame(seconds = seq(1,n/samp_freq))

      noldus_data.sum = noldus_data.sum[rep(seq_len(nrow(noldus_data.sum)), each = window), ]

      noldus_data.sum = cbind(seconds, noldus_data.sum[1:max(seconds),])

      return(noldus_data.sum)
    }

    if(export_format == 'raw'){
      return(noldus_data)
    }

  }
}
