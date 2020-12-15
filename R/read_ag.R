# read_ag
#
# Function to read in ActiGraph accelerometer data.
# Filepath provided should be to an excel .csv format file
# Accelerometer data should be exported with steps, lux, and inclinometer data. Otherwise, may result in improper column naming
#
# filepath = Character string of complete filepath for a single ActiGraph accelerometer excel file
# ENMO_calibrate = Binary indicator (T/F) of whether ENMO calibrated acceleration data should be computed. Uses g.calibrate function from GGIR (broken with recent GGIR updates)
# device_serial_calibrate = Binary indicator (T/F) of whether to use device-specific correction factor coefficients when there is insufficient non-movement periods in the data file for auto-calibration
# calibration_file = Imported data frame with the device-specifi correction factor coefficients
#
# Library dependencies: GGIR, stringr, data.table, dplyr

read_ag = function(filepath, ENMO_calibrate = T, device_serial_calibrate = T, calibration_file){

  # Accounts for exported data that includes the header or not
  # 9/4/18 Greg chnaged line below from read.csv to fread
  check_data = fread(filepath,header = F,skip = 10, nrows = 1)[1,]
  if(is.numeric(check_data[1,])){
    file_data = fread(filepath,header = F, skip = 10, stringsAsFactors = F)
  } else {
    file_data = fread(filepath,header = T, skip = 10, stringsAsFactors = F)
  }

  ag_header = read.csv(filepath,header = F,stringsAsFactors = F, nrows = 10)
  device_serial = str_split(ag_header[2,],'Number: ')[[1]][2]
  start = str_split(ag_header[3,],'Time ')[[1]][2]
  date = str_split(ag_header[4,],'Date ')[[1]][2]
  frequency = as.numeric(str_split(str_split(ag_header[1,],'at ')[[1]][3],' Hz')[[1]][1])

  epoch_full = str_split(ag_header[5,],' 00:')[[1]][2]
  epoch_temp = str_split(epoch_full,':')
  epoch = (as.numeric(epoch_temp[[1]][1])*60) + (as.numeric(epoch_temp[[1]][2]))

  file_length = nrow(file_data)
  date_time_start = mdy_hms(paste(date,start, sep = ' '))

  if(epoch < 1){
    # For Raw data
    file_data = file_data %>% mutate(`Accelerometer X` = as.numeric(`Accelerometer X`),
                                     `Accelerometer Y` = as.numeric(`Accelerometer Y`),
                                     `Accelerometer Z` = as.numeric(`Accelerometer Z`))

    date_time_end = date_time_start + (file_length/frequency)
    Timestamp = seq(from = date_time_start,to = date_time_end, by = 1/frequency)
    Timestamp = Timestamp[1:length(Timestamp)-1]

    file_data = mutate(file_data,
                       VM = sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2),
                       VMcorrG = abs(sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2)-1))

    if(ENMO_calibrate == T){
      C = g.calibrate(filepath,use.temp = F, printsummary=F)

      if(device_serial_calibrate == T){
        if(C$offset[1] == 0 & C$scale[1] == 1){

          device_serial_index = str_which(calibration_file$Serial,device_serial)

          if(length(device_serial_index) == 0){

          } else {
            C$offset[1] = calibration_file$Offset_X[device_serial_index]
            C$offset[2] = calibration_file$Offset_Y[device_serial_index]
            C$offset[3] = calibration_file$Offset_Z[device_serial_index]

            C$scale[1] = calibration_file$Scale_X[device_serial_index]
            C$scale[2] = calibration_file$Scale_Y[device_serial_index]
            C$scale[3] = calibration_file$Scale_Z[device_serial_index]

          }
        }
      }

      file_data = mutate(file_data,
                         calX = `Accelerometer X`*C$scale[1] + C$offset[1],
                         calY = `Accelerometer Y`*C$scale[2] + C$offset[2],
                         calZ = `Accelerometer Z`*C$scale[3] + C$offset[3],
                         ENMO = sqrt(calX^2 + calY^2 + calZ^2)-1)

      file_data = mutate(file_data, ENMO = ifelse(ENMO < 0,0,ENMO))

    }

  } else {
    # For Count data
    date_time_end = date_time_start + (file_length*epoch)
    Timestamp = seq(from = date_time_start, to = date_time_end, by = epoch)
    Timestamp = Timestamp[1:length(Timestamp)-1]
    # If Count data does not have column names when being read in
    if(is.numeric(check_data[1,])){
      colnames(file_data) <- c('Axis1', 'Axis2','Axis3', 'Steps', 'Lux','Inclinometer Off','Inclinometer Standing','Inclinometer Sitting','Inclinometer Lying')[1:ncol(file_data)]

    }
    file_data = mutate(file_data, VM = sqrt(Axis1^2 + Axis2^2 + Axis3^2))
  }

  Dates = as.character(date(Timestamp))
<<<<<<< HEAD
=======
  Time = strip_time_from_fulldate(Timestamp)
>>>>>>> 7d34cf3b5d95820f2aac1f2eb793ae7f23516954
  Hour = as.character(hour(Timestamp)) %>% str_pad(width = 2, side = 'left',pad = '0')
  Minute = as.character(minute(Timestamp)) %>% str_pad(width = 2, side = 'left',pad = '0')
  Second = as.character(second(Timestamp)) %>% str_pad(width = 2, side = 'left',pad = '0')

  ag_data = cbind(Dates,paste(Hour,Minute,Second,sep = ':'),file_data, stringsAsFactors = F)
<<<<<<< HEAD
=======
  # ag_data = cbind(Dates,Time,file_data, stringsAsFactors = F)
>>>>>>> 7d34cf3b5d95820f2aac1f2eb793ae7f23516954

  if(epoch<1){
    if(ENMO_calibrate == T){
      colnames(ag_data) = c('Date','Time','AxisX','AxisY','AxisZ', 'VM', 'VMcorrG', 'CalibratedX','CalibratedY','CalibratedZ','ENMO')
    } else {
      colnames(ag_data) = c('Date','Time','AxisX','AxisY','AxisZ', 'VM', 'VMcorrG')
    }
  } else{
    colnames(ag_data) = c('Date','Time',
                          c('Axis1', 'Axis2','Axis3', 'Steps', 'Lux','Inclinometer Off','Inclinometer Standing','Inclinometer Sitting','Inclinometer Lying')[1:ncol(file_data)-1], 'VM')
  }

  return(ag_data)
}
