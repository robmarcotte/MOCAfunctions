# read_ax
#
# Function to read in resampled raw axivity data

read_ax = function(axivity_filepath, pretty_seconds = T, sf = 100){
  data = fread(axivity_filepath)

  data$Date = ymd(str_split(data$Time, pattern = ' ', simplify = T)[,1])
  data$Time = str_split(data$Time, pattern = ' ', simplify = T)[,2]

  if(pretty_seconds == T){
    data$Time = str_c(str_split(data$Time, pattern ='\\.', simplify = T)[,1], str_pad(as.character(round(as.numeric(str_split(data$Time, pattern ='\\.', simplify = T)[,2])/10,0)), width = 2, side = 'left',pad = '0'), sep = '.')
  }

  data = data %>% select(Date, Time, `Accel-X (g)`:`Accel-Z (g)`)

  colnames(data) = c('Date','Time','AxisX','AxisY','AxisZ')

  data$VM = sqrt(data$AxisX^2 + data$AxisY^2 + data$AxisZ^2)
  return(data)

}
