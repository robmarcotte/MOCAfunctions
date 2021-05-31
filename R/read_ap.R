# read_ap
#
# Function to read in activPAL data

read_ap = function(activpal_filepath, raw_data = F){
  data = data.table::fread(activpal_filepath)
  data$Time = openxlsx::convertToDateTime(data$Time) # function from openxlsx package

  data$Date = lubridate::ymd(str_split(data$Time, pattern = ' ', simplify = T)[,1])
  data$Time = stringr::str_split(data$Time, pattern = ' ', simplify = T)[,2]

  if(raw_data == T){
    data = data %>% dplyr::select(Date, Time, X:Z)

    # Convert axis data to g's
    data$X = as.numeric(as.character(factor(data$X, levels = seq(0, 253), labels =seq(-2, 2, by = 4/253))))
    data$Y = as.numeric(as.character(factor(data$Y, levels = seq(0, 253), labels =seq(-2, 2, by = 4/253))))
    data$Z = as.numeric(as.character(factor(data$Z, levels = seq(0, 253), labels =seq(-2, 2, by = 4/253))))

    colnames(data) = c('Date','Time','AxisX','AxisY','AxisZ')
  }
  return(data)
} # best for raw activpal data. for Activpal events, use activpal.file.reader from activpalProcessing package
