# DO_cleaning_18to20
#
# Function to clean up noldus data with a data frame of known errors.
DO_cleaning_18to20 = function(noldus_data, manual_fix, age_group = c('18to20','15to17','13to14','10to12','6to9','3to5','1to2')){

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
