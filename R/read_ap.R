#' Function to read in activPAL data
#'
#' @param activpal_filepath A .csv filepath containing either event data processed by the activPAL algorithm or raw accelerometer data
#' @param raw_data  A logical indicating whether the data is raw data. Default is F and event data is expected
#' @param parse_timestamp A logical indicating whether the Timestamp in ymd_hms format should be further separated into Date and Time columns
#' @param epoch
#'
#'
#'
#'
#'

read_ap = function(activpal_filepath, raw_data = F, parse_timestamp = F, epoch = NA, samp_freq = 20){

  if(!is.na(epoch)){

    data =activpalProcessing::activpal.file.reader(activpal_filepath)
    data = activpalProcessing::second.by.second(data)
    data = data %>% dplyr::rename(cumulative.steps = steps) %>% dplyr::mutate(steps = cumulative.steps-dplyr::lag(cumulative.steps))

    data = data %>% dplyr::rename(Timestamp =time) %>% dplyr::mutate(Timestamp = ymd_hms(Timestamp)) %>%
      dplyr::mutate(ap.posture = factor(ap.posture, levels = c(0,1,2,2.1, 3.1, 3.2, 4, 5), labels = c('sedentary','standing','stepping','cycling','primary lying','secondary lying','non-wear','travelling')))

    if(epoch != 1){
      offset = epoch-(second(data$Timestamp[1])%%epoch)

      data$index = c(rep(1, times = offset), rep(seq(2, ceiling(nrow(data)/epoch)), each = epoch)[1:(nrow(data)-offset)])

      data = data %>% dplyr::rename(ap.posture_old = ap.posture) %>%
        dplyr::group_by(index) %>%
        dplyr::summarize(Timestamp = dplyr::first(Timestamp),
                         ap.posture = unique(ap.posture_old)[which.max(tabulate(match(ap.posture_old, unique(ap.posture_old))))],
                         mets = mean(mets, na.rm = T),
                         met.hours = sum(met.hours, na.rm =T),
                         steps = sum(steps, na.rm = T),
                         Perc_ap.posture = mean(ap.posture == ap.posture_old, na.rm = T)) %>% dplyr::select(-index)

      # change first timestamp to account for offset
      data$Timestamp[1] = data$Timestamp[1]-(epoch-offset)
    }
  } else {
    data =activpalProcessing::activpal.file.reader(activpal_filepath)
    data = data %>% dplyr::rename(Timestamp = time, ap.posture = activity) %>% dplyr::mutate(Timestamp = ymd_hms(Timestamp)) %>%
      dplyr::mutate(ap.posture = factor(ap.posture, levels = c(0,1,2,2.1, 3.1, 3.2, 4, 5), labels = c('sedentary','standing','stepping','cycling','primary lying','secondary lying','non-wear','travelling')))

  }


  if(raw_data == T){
    # Activpal accelerometers contain 8-bit A/D converters with +/- 2g dynamic range sensitivity and sample at 20 Hz
    # Activpal manual states that uncalibrated computer units range from 1 to 254 and 128 is ~ 0g and 172 is ~1g
    data = data.table::fread(activpal_filepath, header = T)
    data$Time <- as.POSIXct(as.Date(data$Time, origin = "1899-12-30"))

    start_time = round_date(data$Time[1], unit = 'second')

    data$Timestamp = seq(start_time, start_time+ceiling(nrow(data)/samp_freq), by = 1/samp_freq)[1:nrow(data)]

    data = data %>% dplyr::relocate(Timestamp) %>% dplyr::select(Timestamp, X:Z) %>% dplyr::mutate(Timestamp = ymd_hms(Timestamp)) %>%

    # Convert un-calibrated computer units data to g's
    data$X = as.numeric(as.character(factor(data$X, levels = seq(0, 253), labels =seq(-2, 2, by = 4/253))))
    data$Y = as.numeric(as.character(factor(data$Y, levels = seq(0, 253), labels =seq(-2, 2, by = 4/253))))
    data$Z = as.numeric(as.character(factor(data$Z, levels = seq(0, 253), labels =seq(-2, 2, by = 4/253))))
    data$VM = sqrt(data$X^2 + + data$Y^2 + data$Z^2)

    colnames(data) = c('Timestamp','AxisX','AxisY','AxisZ', 'VM')
  }

  if(parse_timestamp == T){
    data$Date = lubridate::ymd(str_split(data$Timestamp, pattern = ' ', simplify = T)[,1])
    data$Time = stringr::str_split(data$Timestamp, pattern = ' ', simplify = T)[,2]
    data = data %>% dplyr::relocate(Timestamp, Date, Time)
  }

  return(data)
}
