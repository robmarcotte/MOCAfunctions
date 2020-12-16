# DO_screen
#
# Function to screen Noldus direct observation data by comparing against a reference data frame of previously observed errors
#
# Library dependencies: svDialogs, stringr, dplyr, tidyr

DO_screen = function(do_filepaths, runparallel = T, do_filescreen_approach = c('sequential','batch'),
                     do_fix_reference = c('18to20','15to17.9','13to14.9','10to12.9','6to9.9','3to5.9','1.5to2.9','custom'),
                     do_fix_custom_filepath, do_fix_export_filepath,
                     do_screen = c('interactive','.csv'), cores = 2){

  do_fix = switch(do_fix_reference,
                  '18to20' = do_fix_18to20,
                  '15to17.9' = readRDS('filepath to 15to17.9 DO errors data'),
                  '13to14.9' = readRDS('filepath to 13to14.9 DO errors data'),
                  '10to12.9' = readRDS('filepath to 10to12.9 DO errors data'),
                  '6to9.9' = readRDS('filepath to 6to9.9 DO errors data'),
                  '3to5.9' = readRDS('filepath to 3to5.9 DO errors data'),
                  '1.5to2.9' = readRDS('filepath to 1.5to2.9 DO errors data'),
                  'custom' = readRDS(do_fix_custom_filepath))

  if(runparallel == TRUE & do_filescreen_approach != 'sequential'){

    cores = ifelse(is.na(cores), detectCores()/2, cores)


    cl = makeCluster(cores)

    registerDoParallel(cl)

    if(cores > detectCores()){
      warning('Desired number of cores is greater than actual cores available on machine. Running parallel processes using the actual cores available on machine.')
    }
  } else {
    cores = 1
  }


  if(runparallel == TRUE & do_filescreen_approach == 'batch'){
    do_data = foreach(iii = 1:length(do_filepaths), .packages = c('MOCAfunctions','tidyr','stringr','lubridate','dplyr'),
                      .combine = 'rbind') %dopar% {
                        noldus_data = DO_descriptives(do_filepaths[iii])

                        # Custom DO typo fixes specific to MOCA Cohort Noldus Templates
                        switch(do_fix_reference,
                               '18to20' = {noldus_data$Behavior = str_replace(noldus_data$Behavior, pattern = 'WalkLoad 4.5', replacement = 'WalkLoad (4.5')
                               noldus_data$Modifier_2 = str_replace(noldus_data$Modifier_2, pattern = 'Carrying Small Child 2.3\\) ', replacement = 'Carrying Small Child (2.3)')},
                               '15to17.9' = {},
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

                        clean_behavior_activity = data.frame(Behavior = behavior,
                                                             Modifier2 = modifier2,
                                                             Modifier1 = noldus_data$METs,
                                                             Behavior_Compendium_MET = behavior_compendium,
                                                             Activity_Compendium_MET = modifier2_compendium,
                                                             stringsAsFactors = F)

                        return(clean_behavior_activity)

                      }
    stopCluster(cl)

  } else {
    noldus_data = DO_descriptives(do_filepaths[iii])

    # Custom DO typo fixes specific to MOCA Cohort Noldus Templates
    switch(do_fix_reference,
           '18to20' = {noldus_data$Behavior = str_replace(noldus_data$Behavior, pattern = 'WalkLoad 4.5', replacement = 'WalkLoad (4.5')
           noldus_data$Modifier_2 = str_replace(noldus_data$Modifier_2, pattern = 'Carrying Small Child 2.3\\) ', replacement = 'Carrying Small Child (2.3)')},
           '15to17.9' = {},
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

    do_data = data.frame(Behavior = behavior,
                         Modifier2 = modifier2,
                         Modifier1 = noldus_data$METs,
                         Behavior_Compendium_MET = behavior_compendium,
                         Activity_Compendium_MET = modifier2_compendium,
                         stringsAsFactors = F)
  }

  # Replace the NA values with nothing
  do_data$Modifier1[do_data$Modifier1 == 'NA'] = NA
  do_data$Modifier2[do_data$Modifier2 == 'NA'] = NA
  do_data$Behavior_Compendium_MET[is.na(do_data$Behavior_Compendium_MET)] = 'NA'
  do_data$Activity_Compendium_MET[is.na(do_data$Activity_Compendium_MET)] = 'NA'

  # Find unique coding combinations
  coded_combos = unique(str_c(do_data$Behavior, do_data$Modifier2, do_data$Modifier1, sep = '_'))
  existing_combos = str_c(do_fix$Behavior, do_fix$Modifier_2, do_fix$METs, sep = '_')

  new_combos_index = which((coded_combos %in% existing_combos) == F)

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
        print(paste('Coded Behavior, Activity Type, and METs = ', need_to_screen$Behavior[i], ', ', need_to_screen$Modifier2[i], ', ', need_to_screen$Modifier1[i], sep = ''))
        print(paste('Behavior Compendium METs: ', need_to_screen$Behavior_Compendium_MET[i],' - Activity Compendium METs: ', need_to_screen$Activity_Compendium_MET[i], sep = ''))
        met_fix = readline('Enter the proper MET value for the DO combination above: ')
        while(is.na(as.numeric(met_fix))){
          met_fix = readline('Entry needs to be a number. Enter the proper MET value for the DO combination above: ')
        }
        need_to_screen$MET_Fix[i] = met_fix
      }

      print('Done with screening MET anomalies. Moving on to manual inspection for implausible coding combinations (e.g. Standing while sitting)')

      for(i in 1:nrow(need_to_screen)){
        behav_error = readline(paste('Is the coded observation [', need_to_screen$Behavior[i], ', ', need_to_screen$Modifier2[i], ', ', need_to_screen$Modifier1[i],
                       ' - (Behavior, Activity, METs)] an implausible behavior combination? Type 1 for Yes or 0 for No:', sep = ''))

        while(is.na(as.numeric(behav_error))){
          print('Entry needs to be either 1 or 0.')
          behav_error = readline(paste('Is the coded observation [', need_to_screen$Behavior[i], ', ', need_to_screen$Modifier2[i], ', ', need_to_screen$Modifier1[i],
                                       ' - (Behavior, Activity, METs)] an implausible behavior combination? Type 1 for Yes or 0 for No:', sep = ''))
        }
        need_to_screen$Combo_error[i] = behav_error
      }

      combo_fix = need_to_screen
    }

    if(do_screen == '.csv'){

      need_to_screen$MET_Fix = ifelse(need_to_screen$MET_error == 1, 'NEED A MET VALUE FIX', NA)
      write.table(need_to_screen,
                  file = paste(dirname(do_fix_custom_filepath), '000 New Combinations for screening.csv', sep = '/'),
                  sep = ',', col.names = T, row.names = F, na = '')

      status = 'stop'
      while(status != 'Ready'){
        temp_status = svDialogs::dlg_list(choices = c('Ready'), title = 'After you screen/fix the 000 New Combinations for screening.csv, type in Ready')
        status = as.character(temp_status$res)
      }

      combo_fix = read.csv(paste(dirname(do_fix_custom_filepath), '000 New Combinations for screening.csv', sep = '/'),
                           header = T, stringsAsFactors = F, )

    }

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
    combo_fix$Reason[combo_fix$Reason== ''] = 0

    combo_fix = combo_fix %>% dplyr::select(-MET_error, -Combo_error, -full_code_combo)

    combo_fix$METs = as.character(format(combo_fix$METs, digits = 2))
    combo_fix$METs = str_trim(combo_fix$METs, side = 'left')

    # combo_fix$Behavior_Compendium_MET = as.character(format(combo_fix$Behavior_Compendium_MET, digits = 2))
    # combo_fix$Behavior_Compendium_MET = str_trim(combo_fix$Behavior_Compendium_MET)

    do_fix$Behavior = as.character(do_fix$Behavior)
    do_fix$Modifier_2 = as.character(do_fix$Modifier_2)
    do_fix$METs = as.character(do_fix$METs)
    do_fix$Reason = as.character(do_fix$Reason)
    do_fix$MET_Fix = as.character(do_fix$MET_Fix)
    do_fix$Behavior_Compendium_MET = as.character(do_fix$Behavior_Compendium_MET)
    do_fix$Activity_Compendium_MET = as.character(do_fix$Activity_Compendium_MET)
    do_fix$Error_Present = as.character(do_fix$Error_Present)

    combo_fix$Behavior = as.character(combo_fix$Behavior)
    combo_fix$Modifier_2 = as.character(combo_fix$Modifier_2)
    combo_fix$METs = as.character(combo_fix$METs)
    combo_fix$Reason = as.character(combo_fix$Reason)
    combo_fix$MET_Fix = as.character(combo_fix$MET_Fix)
    combo_fix$Behavior_Compendium_MET = as.character(combo_fix$Behavior_Compendium_MET)
    combo_fix$Activity_Compendium_MET = as.character(combo_fix$Activity_Compendium_MET)
    combo_fix$Error_Present = as.character(combo_fix$Error_Present)

    do_fix = bind_rows(do_fix, combo_fix)

    if(exists('do_fix_export_filepath')){
      saveRDS(do_fix, do_fix_export_filepath)
    }

    return(do_fix)

  }
}



