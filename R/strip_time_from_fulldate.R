# strip_time_from_fulldate
#
# Function to extract the time object from a vector of full date in POSIXct format
#
# fulldate_vector = Vector with a POSIXct object with Date and Time (%Y-%M-%D HH:MM:SS)
#
# Returns a character vector of the time object

strip_time_from_fulldate = function(fulldate_vector){
  x = str_split(fulldate_vector, pattern = ' ', simplify = T)[,2]

  return(x)
  }
