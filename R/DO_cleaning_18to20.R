# DO_cleaning_18to20
#
# Function to clean up noldus data with a data frame of known errors.
DO_cleaning_18to20 = function(noldus_data, manual_fix){

  # MOCA 18to20 Noldus template had a typo for WalkLoad. Replace WalkLoad periods with proper format
  noldus_data$Behavior = str_replace(noldus_data$Behavior, pattern = 'WalkLoad ', replacement = 'WalkLoad (')
  noldus_data$Modifier_2 = str_replace(noldus_data$Behavior, pattern = 'Carrying Small Child 2.3\\) ', replacement = 'Carrying Small Child (2.3)')

  # Create a new data frame that will be used as a reference for cleaned noldus data
  clean_behavior_activity = data.frame(Behavior = str_split(noldus_data$Behavior, ' \\(', simplify = T)[,1],
                                       Modifier1 = noldus_data$METs,
                                       Behavior_Compendium_MET = str_split(noldus_data$Behavior, ' \\(', simplify = T)[,2],
                                       stringsAsFactors = F)

  # Need to fix Modifier 2 and Activity Type Compendium METs separatly because of an error in the MOCA 18to20 template for these modifiers
  Modifier2 = as.data.frame(noldus_data$Modifier_2)
  Modifier2_first_rough = as.data.frame(str_split(Modifier2[,1], pattern = '\\(', simplify = T), stringsAsFactors = F)

  # Fix column contents because they have mixed information (e.g. additional activity descriptor vs MET value)
  if(ncol(Modifier2_first_rough) >2){
    blank_index = which(Modifier2_first_rough[, ncol(Modifier2_first_rough)] == '')
    if(length(blank_index)>0){
      Modifier2_first_rough[blank_index, ncol(Modifier2_first_rough)] = Modifier2_first_rough[blank_index, (ncol(Modifier2_first_rough)-1)] # slide all MET values to last column
      Modifier2_first_rough[blank_index, (ncol(Modifier2_first_rough)-1)] = '' # replace rows under V2 with blank in V3 with a blank
      Modifier2_first = data.frame(Modifier2 = ifelse(Modifier2_first_rough[,(ncol(Modifier2_first_rough)-1)] == '',
                                                      Modifier2_first_rough$V1,
                                                      str_c(Modifier2_first_rough$V1, Modifier2_first_rough$V2, sep = '(')))
    }
  } else {
    Modifier2_first = data.frame(Modifier2 = Modifier2_first_rough[,1])
  }

  # Create fixed Activity Type Compendium MET variable
  blank_index = which(Modifier2[,ncol(Modifier2)] == '')

  if(length(blank_index)>0){
    # Fix the blank pieces
    Modifier2[blank_index, ncol(Modifier2)] = Modifier2[blank_index, (ncol(Modifier2)-1)]
    Modifier2[blank_index, (ncol(Modifier2)-1)] = ''

    # Fix the multi-MET activities
    Modifier2[-blank_index, ncol(Modifier2)] = str_c(Modifier2[-blank_index, (ncol(Modifier2)-1)], Modifier2[-blank_index, ncol(Modifier2)], sep = '.')
    Modifier2[-blank_index, (ncol(Modifier2)-1)] = ''
  }

  if(ncol(Modifier2_first_rough) >2){
    act_type_METs = str_replace(Modifier2_first_rough[, ncol(Modifier2_first_rough)], pattern = '\\)', '')
  } else {
    act_type_METs = str_replace(Modifier2_first_rough[, ncol(Modifier2_first_rough)], pattern = '\\)', '')
    # act_type_METs = str_c(Modifier2_first_rough[,ncol(Modifier2_first_rough)], Modifier2[,ncol(Modifier2)], sep = '.')
  }

  # Remove the act_type_METs that have a '.' and replace with empty cell
  point_periods = which(act_type_METs == '.')

  if(length(point_periods) >0){
    act_type_METs[point_periods] = ''
  }

  # Combine the fixed Modifier2 and Activity type METs to main clean_behav_act
  clean_behavior_activity$Modifier2 = Modifier2_first$Modifier2
  clean_behavior_activity$Activity_Compendium_MET = act_type_METs

  clean_behavior_activity = clean_behavior_activity %>%
    dplyr::select(Behavior, Modifier2, Modifier1, Behavior_Compendium_MET, Activity_Compendium_MET)

  # Clear the extra white space at the end of activity types
  clean_behavior_activity$Modifier2 = str_trim(clean_behavior_activity$Modifier2, side = 'right')

  noldus_data$Behavior = clean_behavior_activity$Behavior
  noldus_data$METs = clean_behavior_activity$Modifier1
  noldus_data$Modifier_2 = clean_behavior_activity$Modifier2

  noldus_data = left_join(noldus_data, manual_fix)
  noldus_data$Modifier_2 = ifelse(noldus_data$Modifier_2 == 'Quiet', noldus_data$Behavior, noldus_data$Modifier_2)

  noldus_data$Omit_me = 0

  if(is.na(any(noldus_data$Reason != 0))| any(noldus_data$Reason != 0) == T){
    noldus_data$METs = ifelse(str_detect(noldus_data$Reason, 'MET') == T,
                              ifelse(!is.na(noldus_data$MET_Fix), noldus_data$MET_Fix, noldus_data$METs), noldus_data$METs)
    noldus_data$Omit_me = ifelse(str_detect(noldus_data$Reason, 'Behav/Act')== T, 1, 0)
  }

  noldus_data = noldus_data %>% select(-Reason, -MET_Fix)

  return(noldus_data)
}
