# DO_descriptives
#
# Function to extract the relevant information from Noldus Observer XT annotation software
# Keeps the Event_Type == State start and the following variables: Duration_sf, Behavior, and Modifiers
#
# noldus_data = Either an already imported Noldus observer data or a filepath
# read_first = TRUE if the noldus_data object is a filepath and data needs to be imported
# columnn_METs = Column name for the variable that contains METs information
#
# Returns a data frame of the Noldus data
#
# Library dependencies: stringr, readxl

DO_descriptives = function(noldus_data, column_METs = 'Modifier_1', read_first = T){
  if(read_first == T & is.character(noldus_data))
    noldus_data = readxl::read_xlsx(noldus_data)

  noldus_data <- noldus_data[noldus_data$Event_Type=="State start",] # remove all stop times since they're 0 duration

  important_names = str_which(colnames(noldus_data),paste('Time_Relative_sf', 'Duration_sf','Behavior','Modifier', sep = '|'))
  names = names(noldus_data)[important_names]
  names[str_which(names, column_METs)] = 'METs'

  noldus_data = noldus_data[,important_names]
  colnames(noldus_data) = names

  return(noldus_data)

}
