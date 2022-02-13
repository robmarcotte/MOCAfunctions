# read_stepwatch
#
# Function to read in stepwatch data.

read_stepwatch = function(filepath){

  stepwatch_start_full = str_c(read.table(filepath, nrows = 1, skip = 10, stringsAsFactors = F)[2:4], collapse = ' ')

  stepwatch_start_date = str_split(stepwatch_start_full, pattern = ' ', simplify =T)[1]
  stepwatch_start_time = str_c(str_split(stepwatch_start_full, pattern = ' ', simplify = T)[2:3], collapse = ' ')

  stepwatch_end_full = str_c(read.table(filepath, nrows = 1, skip = 12, stringsAsFactors = F)[2:4], collapse = ' ')
  stepwatch_end_date = str_split(stepwatch_end_full, pattern = ' ', simplify = T)[1]
  stepwatch_end_time = str_c(str_split(stepwatch_end_full, pattern = ' ', simplify = T)[2:3], collapse = ' ')

  stepwatch_dates = read.table(filepath, nrows = 1, skip = 29, stringsAsFactors = F)[-1]

  stepwatch_data = read.table(filepath, skip = 31, stringsAsFactors = F)
  colnames(stepwatch_data) = c('Time','AM_PM', stepwatch_dates)

  stepwatch_data$Time= str_c(stepwatch_data$Time, stepwatch_data$AM_PM, sep = ' ')
  stepwatch_data = stepwatch_data[,-2] # Removes the am/pm column

  stepwatch_data = stepwatch_data %>% tidyr::gather(2:ncol(stepwatch_data), key = 'Date', value = 'Stepwatch_Steps') # creates tidy format for data with multiple days

  stepwatch_data$Full_date = str_c(stepwatch_data$Date,stepwatch_data$Time, sep = ' ')
  stepwatch_data$Full_date = ymd_hms(stepwatch_data$Full_date)

  stepwatch_data = stepwatch_data %>% dplyr::filter(Full_date >= ymd_hms(stepwatch_start_full),
                                                    Full_date <= ymd_hms(stepwatch_end_full))

  stepwatch_data$Time = paste(as.character(hour(stepwatch_data$Full_date)) %>% str_pad(width = 2, side = 'left',pad = '0'),
                              as.character(minute(stepwatch_data$Full_date)) %>% str_pad(width = 2, side = 'left',pad = '0'),
                              as.character(second(stepwatch_data$Full_date)) %>% str_pad(width = 2, side = 'left',pad = '0'),sep = ':')

  stepwatch_data = stepwatch_data %>% dplyr::select(-Full_date) %>% dplyr::select(Date, Time, Stepwatch_Steps)

  return(stepwatch_data)

}
