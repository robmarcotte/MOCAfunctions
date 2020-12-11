# automerge_ag_do_data
#
# Function to automate the process of merging ActiGraph and Noldus direct observation data
#
# Library dependencies: foreach, doParallel, stringr, lubridate, dplyr, tidyr

automerge_ag_do_data = function(ag_filepaths = NA, do_filepaths = NA, timestamps = NA,
                                output_filepath = NA, visual_plots = TRUE,
                                do_screen = c('sequential','batch'), doparallel = TRUE, cores = NA){

  if(doparallel == T){

    cores = ifelse(is.na(cores), detectCores()/2, cores)

    if(cores > detectCores()){
      warning('Desired number of cores is greater than actual cores available on machine. Running parallel processes using the actual cores available on machine.')
    }
  }
  else {
    cores = 1
  }

  # insert automation of merging, running the parallel loop on the number of direct observation files

}
