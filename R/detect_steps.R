#' Custom function to detect steps using Ducharme et al 2021 JMPB algorithm
#'
#' @param acc_data_raw Raw accelerometer data containing AxisX, AxisY, AxisZ, and VM data columns
#' @param samp_freq Sampling frequency of the raw accelerometer data. Defaults to 80 Hz
#' @param method String indicator of which method should be applied. Default is the Ducharme et al 2021 algorithm, but 'GENEA' is also an option using the GENEAclassify package
#' @param bpfilt_low Lower bound critical frequency for the bandpass filter. Default is 0.25 Hz
#' @param bpfilt_high Upper bound critical frequency for the bandpass filter. Default is 2.5 Hz
#' @param bw_order Order of the Butterworth filter. Default is 4th order
#' @param detrend Boolean indicator to determine if raw VM signal should be detrended by subtracting the mean. Default is TRUE
#' @param acc_peak_thresh Acceleration threshold (in gravitational units) to identify valid peaks. Wrist = 0.0359; Hip = 0.0267
#
#' Library dependencies: tidyverse, signal, zoo, GENEAclassify
#'

detect_steps = function(acc_data_raw, samp_freq = 80, method = "Ducharme2021", bpfilt_low = .25, bpfilt_high = 2.5, bw_order= 4, detrend = T, acc_peak_thresh = 0.0359){

  if(method == 'Ducharme2021'){
    # Detrend the VM signal by removing the mean of the overall time series
    if(detrend == T){
      acc_data_raw = acc_data_raw %>% dplyr::mutate(VM_new = VM-mean(VM))
    }

    # Create the bandpass butterworth filter
    delta_t = 1/samp_freq
    f.Nyquist = 1/2/delta_t

    bf = signal::butter(n = bw_order, W = c(bpfilt_low/f.Nyquist, bpfilt_high/f.Nyquist), type = 'pass') # Verify the order of critical frequencies is bpfilt_high, bpfilt_low

    # Apply filter to VM signal
    acc_data_raw = acc_data_raw %>% dplyr::mutate(VM_filt = signal::filtfilt(bf, VM_new))

    # Detect local maxima of surrounding points
    peak_binary = zoo::rollapply(as.zoo(acc_data_raw$VM_filt), 3, function(x){which.max(x) == 2}, align = 'center')
    peak_locs = zoo::index(peak_binary)[zoo::coredata(peak_binary)]

    # Remove peaks that are less than the acceleration threshold
    peak_locs = peak_locs[which(acc_data_raw$VM_filt[peak_locs] >= acc_peak_thresh)]

    # Create the step column that indicates whether a step is or is not detect at that given time point
    acc_data_raw$Step = FALSE
    acc_data_raw$Step[peak_locs] = TRUE

    # Consider a step detection refinement step that requires either:
    # 1) Minimum duration of stepping to be considered steps OR
    # 2) Minimum time elapsed between steps to omit erroneous peaks

  }

  # Keeping this here for future exploration
  if(method == 'GENEA'){
    steps = GENEAclassify::stepCounter(acc_data_raw$VM, samplefreq = samp_freq)[1]
  }

  return(acc_data_raw)

  # Test base plotting for filtered signal with remaining peaks plotted
  # plot(5000:10000, acc_data_raw$VM_filt[5000:10000], type = 'l')
  # points(peak_locs[which(peak_locs <= 10000 & peak_locs >= 5000)], acc_data_raw$VM_filt[peak_locs[which(peak_locs <= 10000 & peak_locs >= 5000)]], col = 'red', cex = 1.5)

  }



