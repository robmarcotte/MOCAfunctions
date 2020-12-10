# DO_timestamp
#
# Function to append timestamps to Noldus data previously processed by DO_msec_transform
#
# DO_msec_data = Noldus data expanded to hundredths of a second observations
# session_start_time = A single POSIXct Date-Time value (%Y-%M-%D HH:MM:SS)
#
# Returns a data frame of the timestamped Noldus data
#
# Library dependencies: stringr, readxl

DO_timestamp = function(DO_msec_data, session_start_time){
  msecs = nrow(DO_msec_data)/100

  time = rep(session_start_time+0:(ceiling(msecs)-1), each = 100)

  DO_msec_data$Time = time[1:nrow(DO_msec_data)]

  return(DO_msec_data)
}
