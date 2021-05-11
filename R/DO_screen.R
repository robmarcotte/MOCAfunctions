# DO_screen
#
# Function to screen Noldus direct observation data by comparing against a reference data frame of previously observed errors
#
# Library dependencies: svDialogs, stringr, dplyr, tidyr

DO_screen = function(do_filepaths, do_filescreen_approach = c('sequential'),
                     do_fix_reference = c('18to20','15to17.9','13to14.9','10to12.9','6to9.9','3to5.9','1.5to2.9','custom', 'new'),
                     age_group = c('18to20','15to17.9','13to14.9','10to12.9','6to9.9','3to5.9','1.5to2.9'),
                     do_fix_custom_filepath, do_fix_export_filepath,
                     do_screen = c('interactive','.csv'),
                     do_fix_update = c(TRUE, FALSE)){

  do_fix = switch(do_fix_reference,
                  '18to20' = do_fix_18to20,
                  '15to17.9' = readRDS('filepath to 15to17.9 DO errors data'),
                  '13to14.9' = readRDS('filepath to 13to14.9 DO errors data'),
                  '10to12.9' = readRDS('filepath to 10to12.9 DO errors data'),
                  '6to9.9' = readRDS('filepath to 6to9.9 DO errors data'),
                  '3to5.9' = readRDS('filepath to 3to5.9 DO errors data'),
                  '1.5to2.9' = readRDS('filepath to 1.5to2.9 DO errors data'),
                  'custom' = readRDS(do_fix_custom_filepath),
                  'new' = 'create_new')

  for(iii in 1:length(do_filepaths)){
    noldus_data = DO_descriptives(do_filepaths[iii])

    # Custom DO typo fixes specific to MOCA Cohort Noldus Templates
    switch(age_group,
           '18to20' = {
             noldus_data$Behavior = str_replace(noldus_data$Behavior, pattern = 'WalkLoad 4.5', replacement = 'WalkLoad (4.5')
             noldus_data$Modifier_2 = str_replace(noldus_data$Modifier_2, pattern = 'Carrying Small Child 2.3\\) ', replacement = 'Carrying Small Child (2.3)')},
           '15to17.9' = {
             noldus_data$Modifier_2 = str_replace(noldus_data$Modifier_2, pattern = 'Active Video Games (2.4. 5.9)', replacement = 'Active Video Games (2.4, 5.9)')},
           '13to14.9' = {},
           '10to12.9' = {},
           '6to9.9' = {},
           '3to5.9' = {},
           '1.5to2.9' = {})


    # Parse the compendium values from the coded behaviors and modifiers
    nums = c(unique(str_extract_all(noldus_data$Behavior, paste('\\d\\.\\d', '\\d\\d.\\d', sep = '|'), simplify = T)))
    if(any(nums == '', na.rm = T)){
      nums = nums[-which(nums =='')]
    }
    if(length(nums) == 0)
      nums = 'UNIQUE NO NUMS'
    behavior = str_replace_all(noldus_data$Behavior, paste(nums, collapse = '|'), '')
    behavior = str_trim(str_replace_all(behavior, '[:punct:]', ''))

    behavior_compendium = str_extract_all(noldus_data$Behavior, paste('\\d\\.\\d', '\\d\\d.\\d', sep = '|'), simplify = T)
    behavior_compendium[behavior_compendium == ''] = 'NA'
    behavior_compendium = apply(behavior_compendium, 1, str_c, collapse = ', ')
    behavior_compendium = str_replace_all(behavior_compendium, ', NA', '')

    nums = c(unique(str_extract_all(noldus_data$Modifier_2, paste('\\d\\.\\d', '\\d\\d.\\d', sep = '|'), simplify = T)))
    if(any(nums == '', na.rm = T)){
      nums = nums[-which(nums =='')]
    }
    if(length(nums) == 0)
      nums = 'UNIQUE NO NUMS'
    modifier2 = str_replace_all(noldus_data$Modifier_2, paste(nums, collapse = '|'), '')
    modifier2 = str_trim(str_replace_all(modifier2, '[:punct:]', ''))

    modifier2_compendium = str_extract_all(noldus_data$Modifier_2, paste('\\d\\.\\d', '\\d\\d.\\d', sep = '|'), simplify = T)
    modifier2_compendium[modifier2_compendium == ''] = 'NA'
    modifier2_compendium = apply(modifier2_compendium, 1, str_c, collapse = ', ')
    modifier2_compendium = str_replace_all(modifier2_compendium, ', NA', '')

    do_data = data.frame(Time_Relative_sf = noldus_data$Time_Relative_sf,
                         Behavior = behavior,
                         Modifier2 = modifier2,
                         Modifier1 = noldus_data$METs,
                         Behavior_Compendium_MET = behavior_compendium,
                         Activity_Compendium_MET = modifier2_compendium,
                         stringsAsFactors = F)

    # Replace the NA values with nothing
    do_data$Modifier1[do_data$Modifier1 == 'NA'] = NA
    do_data$Modifier2[do_data$Modifier2 == 'NA'] = NA
    do_data$Behavior_Compendium_MET[is.na(do_data$Behavior_Compendium_MET)] = 'NA'
    do_data$Activity_Compendium_MET[is.na(do_data$Activity_Compendium_MET)] = 'NA'

    # Fix template issues with behaviors
    switch(age_group,
           '18to20' = {},
           '15to17.9' = {
             # Some old templates had wrong Behavior MET values, thus they need to be fixed
             do_data$Behavior_Compendium_MET = ifelse(str_detect(do_data$Behavior, 'WalkLoad'), '4.5', do_data$Behavior_Compendium_MET)
             do_data$Behavior_Compendium_MET = ifelse(str_detect(do_data$Behavior, 'Walking'), '5.0', do_data$Behavior_Compendium_MET)
             do_data$Behavior_Compendium_MET = ifelse(str_detect(do_data$Behavior, 'Elliptical'), '5.0', do_data$Behavior_Compendium_MET)
             do_data$Behavior_Compendium_MET = ifelse(str_detect(do_data$Behavior, 'Skipping'), '9.9', do_data$Behavior_Compendium_MET)
             do_data$Behavior_Compendium_MET = ifelse(str_detect(do_data$Behavior, 'Crawling'), '6.0', do_data$Behavior_Compendium_MET)
             do_data$Behavior_Compendium_MET = ifelse(str_detect(do_data$Behavior, 'Jumping'), '4.7', do_data$Behavior_Compendium_MET)
             do_data$Behavior_Compendium_MET = ifelse(str_detect(do_data$Behavior, 'Dancing'), '4.0', do_data$Behavior_Compendium_MET)},
           '13to14.9' = {},
           '10to12.9' = {},
           '6to9.9' = {},
           '3to5.9' = {},
           '1.5to2.9' = {})
    # Find unique coding combinations
    coded_combos = unique(str_c(do_data$Behavior, do_data$Modifier2, do_data$Modifier1, sep = '_'))

    if(do_fix_reference == 'new'){
      new_combos_index = 1:length(coded_combos)
    } else {
      existing_combos = str_c(do_fix$Behavior, do_fix$Modifier_2, do_fix$METs, sep = '_')
      new_combos_index = which((coded_combos %in% existing_combos) == F)
    }

    if(length(new_combos_index) >0){
      new_combos= coded_combos[new_combos_index]

      do_data$full_code_combo = str_c(do_data$Behavior, do_data$Modifier2, do_data$Modifier1, sep = '_')

      need_to_screen = do_data[which((do_data$full_code_combo %in% new_combos) == T),]

      need_to_screen$criterion_met = str_c(need_to_screen$Behavior_Compendium_MET, need_to_screen$Activity_Compendium_MET, sep = ', ')

      need_to_screen$MET_error = as.numeric(str_detect(need_to_screen$criterion_met, need_to_screen$Modifier1) ==  F)
      need_to_screen$Combo_error = ''
      need_to_screen$MET_Fix = NA

      # Remove duplicates
      need_to_screen$duplicate_check = str_c(need_to_screen$Behavior, need_to_screen$Modifier2, need_to_screen$Modifier1, need_to_screen$Behavior_Compendium_MET, need_to_screen$Activity_Compendium_MET,
                                             sep = '_')
      duplicates = which(duplicated(need_to_screen$duplicate_check) == T)

      if(length(duplicates)>0)
        need_to_screen = need_to_screen[-duplicates,]

      need_to_screen = need_to_screen %>% select(-duplicate_check)

      if(do_screen == 'interactive'){

        met_errors = which(need_to_screen$MET_error == 1)
        for(i in met_errors){
          print(paste('File ', iii, ' of ', length(do_filepaths),': MET Error 1 of ', length(met_errors),' --> ',need_to_screen$Behavior[i], ', ', need_to_screen$Modifier2[i], ', ', need_to_screen$Modifier1[i], ' METs was coded.', sep = ''))
          print(paste('File ', iii, ' of ', length(do_filepaths),': METs should have been either the Behavior Compendium (', need_to_screen$Behavior_Compendium_MET[i], ') or Activity Compendium (', need_to_screen$Activity_Compendium_MET[i],') METs', sep = ''))
          met_fix = readline('File ', iii, ' of ', length(do_filepaths),': Enter the proper MET value for the DO combination above: ')
          while(is.na(as.numeric(met_fix))){
            met_fix = readline('File ', iii, ' of ', length(do_filepaths),': Entry needs to be a number. Enter the proper MET value for the DO combination above: ')
          }
          need_to_screen$MET_Fix[i] = met_fix
        }

        print('Done with screening MET anomalies. Moving on to manual inspection for implausible coding combinations (e.g. Standing while sitting)')

        for(i in 1:nrow(need_to_screen)){
          print(paste('File ', iii, ' of ', length(do_filepaths),': ', need_to_screen$Behavior[i], ' while ', need_to_screen$Modifier2[i], ' was coded (Whole Body Movement and Activity Type, respectively)', sep = ''))
          behav_error = readline(paste('File ', iii, ' of ', length(do_filepaths),': Is this an IMPLAUSIBLE behavior? (Type 1 for "Yes, this is an error" or 0 for "No, this is reasonable", then press Enter): ', sep = ''))

          while(is.na(as.numeric(behav_error)) | abs(as.numeric(behav_error)) > 1){
            print('File ', iii, ' of ', length(do_filepaths),': Entry needs to be either 1 or 0.')
            print(paste('File ', iii, ' of ', length(do_filepaths),': ', need_to_screen$Behavior[i], ' while ', need_to_screen$Modifier2[i], ' was coded (Whole Body Movement and Activity Type, respectively)', sep = ''))
            behav_error = readline(paste('File ', iii, ' of ', length(do_filepaths),': Is this an IMPLAUSIBLE behavior? (Type 1 for "Yes, this is an error" or 0 for "No, this is reasonable", then press Enter): ', sep = ''))

          }
          need_to_screen$Combo_error[i] = behav_error
        }

        combo_fix = need_to_screen
      }

      # Only supporting interactive within R for now. Consider revisiting .csv approach in a later date - RTM 4/6/2021
      # if(do_screen == '.csv'){
      #
      #   need_to_screen$MET_Fix = ifelse(need_to_screen$MET_error == 1, 'NEED A MET VALUE FIX', NA)
      #   write.table(need_to_screen,
      #               file = paste(dirname(do_fix_custom_filepath), '000 New Combinations for screening.csv', sep = '/'),
      #               sep = ',', col.names = T, row.names = F, na = '')
      #
      #   status = 'stop'
      #   while(status != 'Ready'){
      #     temp_status = svDialogs::dlg_list(choices = c('Ready'), title = 'After you screen/fix the 000 New Combinations for screening.csv, type in Ready')
      #     status = as.character(temp_status$res)
      #   }
      #
      #   combo_fix = read.csv(paste(dirname(do_fix_custom_filepath), '000 New Combinations for screening.csv', sep = '/'),
      #                        header = T, stringsAsFactors = F, )
      #
      # }

      na_index = which(is.na(combo_fix$MET_error))

      if(length(na_index) > 0)
        combo_fix$MET_error[na_index] = 0

      na_index = which(is.na(combo_fix$Combo_error))

      if(length(na_index) > 0)
        combo_fix$Combo_error[na_index] = 0

      combo_fix = combo_fix %>% dplyr::rename(Modifier_2 = Modifier2,
                                              METs = Modifier1) %>%
        select(-criterion_met)

      combo_fix$Error_Present = ifelse(combo_fix$MET_error == 1 | combo_fix$Combo_error == 1, 1, 0)
      combo_fix$Reason = ifelse((combo_fix$MET_error == 1 & combo_fix$Combo_error == 1), 'MET and Behav/Act Combo',
                                ifelse(combo_fix$MET_error == 1, 'MET',
                                       ifelse((combo_fix$Combo_error == 1 & combo_fix$Activity == 'Quiet'), 'Dont use Quiet as Activity Type',
                                              ifelse(combo_fix$Error_Present == 0, '', 'Behav/Act Combo'))))

      combo_fix$Reason  = ifelse((combo_fix$Reason == '' | is.na(combo_fix$Reason)), 0, combo_fix$Reason)

      combo_fix = combo_fix %>% dplyr::select(-MET_error, -Combo_error, -full_code_combo)

      combo_fix$METs = as.character(format(combo_fix$METs, digits = 2))
      combo_fix$METs = str_trim(combo_fix$METs, side = 'left')

      combo_fix$Behavior = as.character(combo_fix$Behavior)
      combo_fix$Modifier_2 = as.character(combo_fix$Modifier_2)
      combo_fix$METs = as.character(combo_fix$METs)
      combo_fix$Reason = as.character(combo_fix$Reason)
      combo_fix$MET_Fix = as.character(combo_fix$MET_Fix)
      combo_fix$Behavior_Compendium_MET = as.character(combo_fix$Behavior_Compendium_MET)
      combo_fix$Activity_Compendium_MET = as.character(combo_fix$Activity_Compendium_MET)
      combo_fix$Error_Present = as.character(combo_fix$Error_Present)

      do_fix$Behavior = as.character(do_fix$Behavior)
      do_fix$Modifier_2 = as.character(do_fix$Modifier_2)
      do_fix$METs = as.character(do_fix$METs)
      do_fix$Reason = as.character(do_fix$Reason)
      do_fix$MET_Fix = as.character(do_fix$MET_Fix)
      do_fix$Behavior_Compendium_MET = as.character(do_fix$Behavior_Compendium_MET)
      do_fix$Activity_Compendium_MET = as.character(do_fix$Activity_Compendium_MET)
      do_fix$Error_Present = as.character(do_fix$Error_Present)

      if(do_fix_reference != 'new'){
        do_fix$Behavior = as.character(do_fix$Behavior)
        do_fix$Modifier_2 = as.character(do_fix$Modifier_2)
        do_fix$METs = as.character(do_fix$METs)
        do_fix$Reason = as.character(do_fix$Reason)
        do_fix$MET_Fix = as.character(do_fix$MET_Fix)
        do_fix$Behavior_Compendium_MET = as.character(do_fix$Behavior_Compendium_MET)
        do_fix$Activity_Compendium_MET = as.character(do_fix$Activity_Compendium_MET)
        do_fix$Error_Present = as.character(do_fix$Error_Present)

        do_fix = bind_rows(do_fix, combo_fix)
      } else {
        do_fix = bind_rows(do_fix, combo_fix)
      }

    }

    error_indicator = do_fix %>% dplyr::select(Behavior, Modifier_2, METs, Error_Present, Reason) %>%
      dplyr::rename(Modifier2 = Modifier_2, Modifier1 = METs) %>% dplyr::filter(str_detect(Reason, 'Behav/Act Combo'))

    do_data = left_join(do_data, error_indicator)
    noldus_errors = do_data %>% filter(Error_Present == 1)

    if(nrow(noldus_errors)>0){
      View(noldus_errors)
      screen_done = readline(paste('Finished screening ', basename(do_filepaths[iii]), ' and some errors were found. Please fix them, then press Enter to move on.', sep = ''))
    }

    print(paste('Finished ', iii, ' of ', length(do_filepaths),sep = ''))
  }


  if(do_fix_update == T){
    do_fix = do_fix %>% arrange(Behavior, Modifier_2)

    # Need to fix this later since the internal DO fix data frames shouldn't be rewritten and any new do_fix dataframes should be exported to a custom filepath
    do_fix = switch(do_fix_reference,
                    '18to20' = do_fix_18to20,
                    '15to17.9' = saveRDS(do_fix,'filepath to 15to17.9 DO errors data'),
                    '13to14.9' = saveRDS(do_fix,'filepath to 13to14.9 DO errors data'),
                    '10to12.9' = saveRDS(do_fix,'filepath to 10to12.9 DO errors data'),
                    '6to9.9' = saveRDS(do_fix,'filepath to 6to9.9 DO errors data'),
                    '3to5.9' = saveRDS(do_fix,'filepath to 3to5.9 DO errors data'),
                    '1.5to2.9' = saveRDS(do_fix,'filepath to 1.5to2.9 DO errors data'),
                    'custom' = saveRDS(do_fix,do_fix_custom_filepath),
                    'new' = saveRDS(do_fix, do_fix_export_filepath))



  }
}



