% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Normalize.R
\name{normalize.bysignal}
\alias{normalize.bysignal}
\title{Normalize signal intensity}
\usage{
normalize.bysignal(
  AssayDat,
  Signal = "BckSubSignal",
  SignalStat = "SDSignal",
  NormalizationSignalType = NULL,
  NameofNormalizationSignal = NULL,
  GroupBy = c("DNA", "Minutes", "Protein")
)
}
\arguments{
\item{AssayDat}{Df of the long data table input file}

\item{Signal}{The quoted column name of the assay intensity signal}

\item{SignalStat}{The quoted column name ofsecond signal to normalize. Often the Standard Deviation}

\item{NormalizationSignalType}{The variable type of the signal to divide all other signals}

\item{NameofNormalizationSignal}{The quoted name of the signal to divide all other signals}

\item{GroupBy}{The names of the variables over which the assay data is separated including minutes. Defaults to DNA, Protein and Minutes.
@return long Df of all signals divided by the maximum value of the chosen signal}
}
\description{
Divides all signals by a single signal. Defaults to the largest signal and makes all signals between 0-1.
}
