#' Normalize signal intensity
#'
#' Divides all signals by a single signal. Defaults to the largest signal and makes all signals between 0-1.
#'
#' @param AssayDat Df of the long data table input file
#' @param Signal The quoted column name of the assay intensity signal
#' @param SignalStat The quoted column name ofsecond signal to normalize. Often the Standard Deviation
#' @param NormalizationSignalType The variable type of the signal to divide all other signals
#' @param NameofNormalizationSignal The quoted name of the signal to divide all other signals
#' @param GroupBy The names of the variables over which the assay data is separated including minutes. Defaults to DNA, Protein and Minutes.
#'  @return long Df of all signals divided by the maximum value of the chosen signal
#' @export
normalize.bysignal <- function(AssayDat,
                              Signal = "BckSubSignal",
                              SignalStat = "SDSignal",
                              NormalizationSignalType = NULL,
                              NameofNormalizationSignal = NULL,
                              GroupBy = c("DNA", "Minutes", "Protein")
                              ) {
  AssayDat %>%
    dplyr::group_by(dplyr::across(GroupBy)) %>%
    normalize.byvalue(Signal = Signal,
                      SignalStat = SignalStat,
                      Value = find.maxsignal(
                        .,
                        Signal = {{Signal}},
                        NormalizationSignalType = {{NormalizationSignalType}},
                        NameofNormalizationSignal = NameofNormalizationSignal
                        )
    )
  }

#' Make signal relative
#'
#'
#' Divides all signals by a chosen signal to show relative intensity. Useful for normalizing over several experiments.
#'
#' @param AssayDat A data frame containing long assay data
#' @param Signal Column name of the assay data to transform
#' @param SignalStat Column name of the assay statistics data to transform
#' @param Value A vector to divide the Signal by. Defaults to maximum value of selected Signal
#' @return Df with intensity divided by the maximum intensity of the normalization signal
#' @export
normalize.byvalue <- function(AssayDat,
                              Signal = Signal,
                              SignalStat = SignalStat,
                              Value = MaxSignal
                              ) {
  Signal <- (as.name(Signal))
  SignalStat <- (as.name(SignalStat))

  AssayDat %>%
    dplyr::ungroup() %>%
    dplyr::mutate(NormalizedSignal = {{Signal}}/Value,
                  NormalizedSignalStat = {{SignalStat}}/Value
                  )
}


#' Find Biggest Signal
#'
#' Finds the maximum intensity of a single signal.
#' If NameofNormalizationSignal is Null, will return the max Signal for the enter Df.
#'
#' @param AssayDat Df of the long data table input file
#' @param Signal Column name of signal
#' @param NormalizationSignalType The variable type of the signal to divide all other signals
#' @param NameofNormalizationSignal The quoted name of the signal to divide all other signals
#' @return A vector, the max intensity of the normalization
#' @export
find.maxsignal <- function(AssayDat,
                           Signal = Signal,
                           NormalizationSignalType = NULL,
                           NameofNormalizationSignal = NULL
                           ) {

  Signal <- (as.name(Signal))

  #ungroup to remove by-minute, filter is applied conditionally.
  AssayDat %>%
    dplyr::ungroup() %>%
    dplyr::filter(if (is.null(NameofNormalizationSignal) == FALSE) {{NormalizationSignalType}} == NameofNormalizationSignal else TRUE) %>%
    dplyr::summarise(MaxSignal = max({{Signal}})) %>%
    dplyr::pull(MaxSignal)
}


