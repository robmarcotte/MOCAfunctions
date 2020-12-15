# DO_screen
#
# Function to screen Noldus direct observation data by comparing against a reference data frame of previously observed errors
#
# Library dependencies: none

DO_screen = function(runparallel = T, do_filescreen_approach = c('sequential','batch'), do_error_reference = c('18to20','15to17.9','13to14.9','10to12.9','6to9.9','3to5.9','1.5to2.9','custom'), do_error_custom_filepath = NA, do_screen = c('interactive','.csv')){

  if(runparallel == T & do_screen == 'batch'){
    do_data = foreach(iii = 1:length(do_filepaths), .packages = c('MOCAfunctions','tidyr','stringr','lubridate','dplyr'),
                      .combine = 'rbind') %dopar% {
                        noldus_data = DO_descriptives(do_filepaths[iii])
                      }

    do_data = DO_cleaning_18to20(do_data, do_fix)

    # Find unique coding combinations
    do_unique = do_data %>% group_by(Behavior, Modifier2, Modifier1) %>% dplyr::summarize(n = n())
    existing_combos = str_c(do_fix$Behavior, do_fix$Activity, do_fix$Coded_MET_Value, sep = '_')
    coded_combos = str_c(do_data$Behavior, do_data$Modifier_2, do_data$METs)

    new_combos_index = which((coded_combos %in% existing_combos) == F)

    if(length(new_combos_index >0)){
      new_combos= coded_combos[new_combos_index]

      clean_behavior_activity$full_code_combo = str_c(clean_behavior_activity$Behavior, clean_behavior_activity$Modifier2, clean_behavior_activity$Modifier1, sep = '_')

      need_to_screen = clean_behavior_activity[which((clean_behavior_activity$full_code_combo %in% new_combos) == T),]
      need_to_screen = need_to_screen %>%
        dplyr::group_by(Behavior, Modifier2, Modifier1, Behavior_Compendium_MET, Activity_Compendium_MET) %>%
        dplyr::summarize(Frequency_.seconds.= sum(Frequency_.seconds., na.rm = T))
      need_to_screen$criterion_met = str_c(need_to_screen$Behavior_Compendium_MET, need_to_screen$Activity_Compendium_MET, sep = ', ')

      need_to_screen$MET_error = as.numeric(str_detect(need_to_screen$criterion_met, need_to_screen$Modifier1) ==  F)
      need_to_screen$Combo_error = ''

      write.table(need_to_screen,
                  file = paste(moca_drive, 'MOCA/CSV Files/DO_Cleaning_Output', cohort_output_folder_all[i], '000 New Combinations for screening.csv', sep = '/'),
                  sep = ',', col.names = T, row.names = F)

      status = 'stop'
      while(status != 'Ready'){
        temp_status = svDialogs::dlg_list(choices = c('Ready'), title = 'After you screen/fix the 000 New Combinations for screening.csv, type in Ready')
        status = as.character(temp_status$res)
      }
      combo_fix = read.csv(paste(moca_drive, 'MOCA/CSV Files/DO_Cleaning_Output', cohort_output_folder_all[i], '000 New Combinations for screening.csv', sep = '/'),
                           header = T, stringsAsFactors = F, )
      na_index = which(is.na(combo_fix$MET_error))

      if(length(na_index) > 0)
        combo_fix$MET_error[na_index] = 0

      na_index = which(is.na(combo_fix$Combo_error))

      if(length(na_index) > 0)
        combo_fix$Combo_error[na_index] = 0

      combo_fix = combo_fix %>% dplyr::rename(Activity = Modifier2,
                                              Coded_MET_Value = Modifier1) %>%
        select(-criterion_met)

      # Should already be in the environment
      screened_combo = read_xlsx(paste(moca_drive, 'MOCA/CSV Files/DO_Cleaning_Output', cohort_output_folder_all[i], previously_screened_filename_all[i], sep = '/'), col_names = T)

      combo_fix$Error_Present = ifelse(combo_fix$MET_error == 1 | combo_fix$Combo_error == 1, 1, 0)
      combo_fix$Reason = ifelse((combo_fix$MET_error == 1 & combo_fix$Combo_error == 1), 'MET and Behav/Act Combo',
                                ifelse(combo_fix$MET_error == 1, 'MET',
                                       ifelse((combo_fix$Combo_error == 1 & combo_fix$Activity == 'Quiet'), 'Dont use Quiet as Activity Type',
                                              ifelse(combo_fix$Error_Present == 0, '', 'Behav/Act Combo'))))

      combo_fix = combo_fix %>% dplyr::select(-MET_error, -Combo_error)

      combo_fix$Coded_MET_Value = as.character(format(combo_fix$Coded_MET_Value, digits = 2))
      combo_fix$Coded_MET_Value = str_trim(combo_fix$Coded_MET_Value, side = 'left')

      combo_fix$Behavior_Compendium_MET = as.character(format(combo_fix$Behavior_Compendium_MET, digits = 2))
      combo_fix$Behavior_Compendium_MET = str_trim(combo_fix$Behavior_Compendium_MET, side = 'left')
      combo_fix$Behavior_Compendium_MET = str_trim(combo_fix$Behavior_Compendium_MET, side = 'right')
      combo_fix$Activity_Compendium_MET = as.character(combo_fix$Activity_Compendium_MET)

      screened_combo$Coded_MET_Value = as.numeric(screened_combo$Coded_MET_Value)
      combo_fix$Coded_MET_Value = as.numeric(combo_fix$Coded_MET_Value)

      screened_combo = bind_rows(screened_combo, combo_fix)

      # Want to easily find the new observations, so don't want to modify the arrangement
      # screened_combo = screened_combo %>% arrange(Behavior, Activity)

      write_xlsx(screened_combo,
                 paste(moca_drive, 'MOCA/CSV Files/DO_Cleaning_Output', cohort_output_folder_all[i], previously_screened_filename_all[i], sep = '/'),
                 col_names = T)
    }



  }

}
