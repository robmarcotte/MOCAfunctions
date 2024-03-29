% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AG_DO_merge.R
\name{AG_DO_merge}
\alias{AG_DO_merge}
\title{Function to automate the process of merging ActiGraph and Noldus direct observation data
Assumes that screening of observations has already occurred and either a default do_fix data or a custom filepath with how to approach novel observations is provided. See DO_screen for further details.}
\usage{
AG_DO_merge(
  ag_filepaths,
  do_filepaths,
  timestamps,
  do_time_indicator,
  ag_do_indicator,
  samp_freq = 80,
  do_fix_reference = c("18to20", "15to17.9", "13to14.9", "10to12.9", "6to9.9",
    "3to5.9", "1.5to2.9", "custom"),
  do_fix_custom_filepath,
  output_filepath,
  visual_plots = TRUE,
  runparallel = FALSE,
  cores = NA
)
}
\arguments{
\item{samp_freq}{Sampling frequency of raw accelerometer data}

\item{do_fix_reference}{Character string to identify which lookup table should be used for correct direct observation annotations}

\item{do_fix_custom_filepath}{Filepath containing a custom lookup table for correcting direct observation annotations}

\item{output_filepath}{Filepath where merged actigraph and direct observation data should be stored

Library dependencies: foreach, doParallel, stringr, lubridate, dplyr, tidyr, data.table, readxl}

\item{ag_filepath}{Filepath containing ActiGraph data}

\item{do_filepath}{Filepath containing Noldus Observer XT data coded using the MOCA coding template}

\item{timestart}{Time object (ymd_hms format) identifying the start of the direct observation}

\item{participant_id}{Character string of the filename or participant ID}
}
\description{
Function to automate the process of merging ActiGraph and Noldus direct observation data
Assumes that screening of observations has already occurred and either a default do_fix data or a custom filepath with how to approach novel observations is provided. See DO_screen for further details.
}
