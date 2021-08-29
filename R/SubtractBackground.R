#' Subtracts a background signal from all other signals
#'
#'
#' @param AssayDat A df of Assay data
#' @param Signal The colname of the Assay intensity signal
#' @param GroupBy The names of the variables over which the assay data is separated. Defaults to minutes, cannot include BackgroundSignalType variable.
#' @param NameofBackgroundSignal The quoted name of the signal to subtract from all other signals
#' @param BackgroundSignalType The variable type of the signal to subtract from all other signals
#' @return A df of Assay data with the intensity of the Background Signal subtracted
#' @export
subtract.background <- function(AssayDat,
                                Signal = MeanSignal,
                                GroupBy = c("Minutes"),
                                NameofBackgroundSignal = "noDNA",
                                BackgroundSignalType = DNA
                                ) {
  # subtracting the baseline per minute; references a DNA name.
  AssayDat %>%
    dplyr::group_by(dplyr::across(GroupBy)) %>%
    dplyr::mutate(BckSubSignal = {{Signal}} - {{Signal}}[{{BackgroundSignalType}} == NameofBackgroundSignal]
    )
}
