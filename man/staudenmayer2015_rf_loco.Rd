% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/staudenmayer2015_rf_loco.R
\name{staudenmayer2015_rf_loco}
\alias{staudenmayer2015_rf_loco}
\title{Tree models from Staudenmayer et al 2015}
\usage{
staudenmayer2015_rf_loco(
  acc_data_raw,
  mods_filepath = NA,
  samp_freq = 80,
  epoch = 15,
  expand_1sec = F
)
}
\arguments{
\item{acc_data_raw}{Raw tri-axial wrist accelerometer data with column names Timestamp, AxisX, AxisY, AxisZ}

\item{mods_filepath}{Filepath that contains rf.loc.model in an RData object. Assuming by default that model is already loaded in the environment.}

\item{samp_freq}{Sampling frequency of the raw accelerometer data. Default is 80 hz}

\item{epoch}{Non-overlapping window size in seconds. Default is 15-seconds}

\item{expand_1sec}{Binary indicator of whether only SedSphere estimates should be returned as a second-by-second vector}
}
\value{
Aggregated data in 15-second epochs with accelerometer values and SedSphere estimate
}
\description{
Tree models from Staudenmayer et al 2015
}
