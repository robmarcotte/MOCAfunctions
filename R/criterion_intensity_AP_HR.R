#' Function to create criterion measure from activpal and heart rate data
#'
#' @param HR_data Data with a Timestamp in ymd_hms format and heart rate data labelled as 'HR'
#' @param AP_data ActivPAL data with a Timestamp column in ymd_hms format and event data (sitting/lying, standing, stepping)
#' @param HR_flex Individualized HR-flex threshold. Periods with HR data above this threshold will use HR to determine intensity while periods below will use activPAL event data
#' @param MPA_HRR Individualized threshold for determining moderate-intensity activity from HR data
#' @param VPA_HRR Individualized threshold for determining vigorous-intensity activity from HR data
#'

criterion_intensity_AP_HR = function(HR_data, AP_data, HR_flex, MPA_HRR, VPA_HRR){

  criterion_data = left_join(HR_data, AP_data)

  if(!is.na(HR_flex)){

  criterion_data = criterion_data %>% dplyr::mutate(Criterion_Intensity = ifelse(HR >= MPA_HRR,
                                                                                 ifelse(HR >= VPA_HRR, 4, 3),
                                                                                 ifelse(HR >= HR_flex, 2,
                                                                                        ifelse(str_detect(ap.posture, 'sedentary') | str_detect(ap.posture, 'lying') | str_detect(ap.posture, 'travel'), 1, 2))),
                                                    Criterion_Intensity = factor(Criterion_Intensity, levels = c(1:4), labels = c('Sed','LPA','MPA','VPA')))
  }

  if(is.na(HR_flex)){
    criterion_data = criterion_data %>% dplyr::mutate(Criterion_Intensity = ifelse(HR >= MPA_HRR,
                                                                                   ifelse(HR >= VPA_HRR, 4, 3),
                                                                                          ifelse(str_detect(ap.posture, 'sedentary') | str_detect(ap.posture, 'lying') | str_detect(ap.posture, 'travel'), 1, 2)),
                                                      Criterion_Intensity = factor(Criterion_Intensity, levels = c(1:4), labels = c('Sed','LPA','MPA','VPA')))
  }

  return(criterion_data)

}
