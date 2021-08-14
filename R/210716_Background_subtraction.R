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
