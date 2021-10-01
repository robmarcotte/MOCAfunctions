# DO_cleaning_18to20
#
# Function to clean up noldus data with a data frame of known errors.
DO_cleaning_18to20 = function(noldus_data, manual_fix, age_group = c('18to20','15to17','13to14','10to12','6to9','1to5')){

  # Custom DO typo fixes specific to MOCA Cohort Noldus Templates
  switch(age_group,
         '18to20' = {
           noldus_data$Behavior = str_replace(noldus_data$Behavior, pattern = 'WalkLoad 4.5', replacement = 'WalkLoad (4.5')
           noldus_data$Modifier_2 = str_replace(noldus_data$Modifier_2, pattern = 'Carrying Small Child 2.3\\) ', replacement = 'Carrying Small Child (2.3)')},
         '15to17' = {
           noldus_data$Modifier_2 = str_replace(noldus_data$Modifier_2, pattern = 'Active Video Games (2.4. 5.9)', replacement = 'Active Video Games (2.4, 5.9)')},
         '13to14' = {
           # Some templates have a METS modifier even though it was never coded. Fix the colnames so WBM = Behavior; METs = Modifier_1, Activity Type = Modifier_2, Locomotion = Modifier_3
           if('None' %in% unique(noldus_data$METs)){
             noldus_data = noldus_data %>% select(-METs) %>%
               rename(Behavior = Behavior,
                      METs = Modifier_2,
                      Modifier_2 = Modifier_3,
                      Modifier_3 = Modifier_4)
           }

           # Fix the activity type Slow Walking to be lowercase "W". Makes it so that when we account for template MET errors for this age group they don't get confused with Walking due to case sensitivity
           noldus_data$Modifier_2 = str_replace(noldus_data$Modifier_2, pattern = 'Slow Walking', replacement = 'Slow walking')

           # Some templates have Walking (Slow) as the activity type. Change it to Slow walking (2.9) for consistency
           noldus_data$Modifier_2 = str_replace(noldus_data$Modifier_2, pattern = 'Walking \\(Slow\\)', replacement = 'Slow walking')

         },
         '10to12' = {},
         '6to9' = {},
         '1to5' = {})


  if(age_group != '1to5'){
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
  } else {
    # Different because intensity information is categorical not numeric METs

    # Handle the behavior variable
    behavior = str_split(noldus_data$Behavior, ' \\(', simplify = T)[,1]

    behavior_compendium = str_split(noldus_data$Behavior, ' \\(', simplify = T)[,2]

    # Need to remove close parentheses
    behavior_compendium = str_replace(behavior_compendium, '\\)','')

    # Handle the modifier 2 or activity type variable
    modifier2 = str_split(noldus_data$Modifier_2, ' \\(', simplify = T)[,1]
    modifier2_compendium = str_split(noldus_data$Modifier_2, ' \\(', simplify = T)[,2]

    # Need to remove close parentheses
    modifier2_compendium = str_replace(modifier2_compendium, '\\)','')
  }

  clean_behavior_activity = data.frame(Behavior = behavior,
                                       Modifier2 = modifier2,
                                       Modifier1 = noldus_data$METs,
                                       Behavior_Compendium_MET = behavior_compendium,
                                       Activity_Compendium_MET = modifier2_compendium,
                                       stringsAsFactors = F)

  noldus_data$Behavior = clean_behavior_activity$Behavior
  noldus_data$METs = clean_behavior_activity$Modifier1
  noldus_data$Modifier_2 = clean_behavior_activity$Modifier2

  noldus_data = left_join(noldus_data, manual_fix)
  noldus_data$Modifier_2 = ifelse(noldus_data$Modifier_2 == 'Quiet', noldus_data$Behavior, noldus_data$Modifier_2)

  noldus_data$Omit_me = 0

  if(is.na(any(noldus_data$Reason != 0))| any(noldus_data$Reason != 0) == T){
    noldus_data$METs = ifelse(!is.na(noldus_data$Reason) & str_detect(noldus_data$Reason, 'MET') == T,
                              ifelse(!is.na(noldus_data$MET_Fix), noldus_data$MET_Fix, noldus_data$METs), noldus_data$METs)
    noldus_data$Omit_me = ifelse(str_detect(noldus_data$Reason, 'Behav/Act')== T, 1, 0)
  }

  noldus_data = noldus_data %>% select(-Behavior_Compendium_MET, -Activity_Compendium_MET, -Error_Present, -Reason, -MET_Fix)

  return(noldus_data)
}
