# read_ax
#
# Function to read in resampled raw axivity data

read_ax = function(axivity_filepath, ENMO_calibrate = F, device_serial_calibrate = F, calibration_file, sf = 100, parse_timestamp = F){
  data = data.table::fread(axivity_filepath)

  data = data %>% dplyr::rename(Timestamp = Time,
                                AxisX = `Accel-X (g)`,
                                AxisY = `Accel-Y (g)`,
                                AxisZ = `Accel-Z (g)`) %>%
    mutate(VM = sqrt(AxisX^2 + AxisY^2 + AxisZ^2))

  if(ENMO_calibrate == T){
    temp_timestamp = data$Timestamp

    data = data %>% dplyr::select(-Timestamp)

    if(nrow(data) > 12*60*60*sf){
      C = g.calibrate(axivity_filepath,use.temp = F, printsummary=F)
    } else {
      C = list(offset = c(0,0,0),
               scale = c(1,1,1))
    }

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
                       calX = AxisX*C$scale[1] + C$offset[1],
                       calY = AxisY*C$scale[2] + C$offset[2],
                       calZ = AxisZ*C$scale[3] + C$offset[3],
                       ENMO = (sqrt(calX^2 + calY^2 + calZ^2)-1)*1000) # convert to milli-gravitational units

    file_data = mutate(file_data, ENMO = ifelse(ENMO < 0,0,ENMO))

    colnames(file_data) = c('AxisX','AxisY','AxisZ', 'VM','VMcorrG','calX','calY','calZ','ENMO')
  }

  if(parse_timestamp == T){
    data = data %>% mutate(Filename = basename(axivity_filepath),
                           Date = lubridate::date(Timestamp),
                           Time = format(Timestamp, format = "%H:%M:%S")) %>%
      dplyr::relocate(Filename, Timestamp, Date, Time)
  }

  return(data)

}
