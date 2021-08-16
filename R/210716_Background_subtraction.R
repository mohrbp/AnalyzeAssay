#' Subtracts a background signal from all other signals
#'
#'
#' @param kdat A df of Assay data
#' @param mean_signal The colname of the Assay intensity signal
#' @param NameofBackgroundSignal The protein name of the signal to subtract from all other signals
#' @return A df of Assay data with the intensity of the Background Signal subtracted
#' @export
subtract_background_kinetic <- function(kdat, mean_signal = avg_RFU,
                                        NameofBackgroundSignal = "noDNA",
                                        groups = c("minutes")){
  # subtracting the baseline per minute; references a DNA name.
  kdat %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(groups)) %>%
    dplyr::mutate(bck_sub_RFU = {{mean_signal}} - {{mean_signal}}[DNA == NameofBackgroundSignal])
}


#need to test this line to generalize
#function(kdat, mean_signal = avg_RFU, NameofBackgroundSignal = "noDNA", BackgroundSampleType = "DNA", groups = c("minutes"))
# dplyr::mutate(bck_sub_RFU = {{mean_signal}} - {{mean_signal}}[{{BackgroundSampleType}} == NameofBackgroundSignal])
