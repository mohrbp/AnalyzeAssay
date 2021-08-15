#' Normalize
#'
#' Divides all kinetic signals by a single kinetic signal. If the largest signal is chosen, makes all signals between 0-1.
#'
#' @param kdat long kinetic data table with central tendencies calculated
#' @return a long kinetic data table with all values normalized to "Normal"
#' @export
normalizetoSignal <- function(kdat){

  kdat %>%
  normalize_TxTl(mean_RFUs = bck_sub_RFU,
                 max_RFU = find_biggest_signal(.,
                                               mean_RFUs = bck_sub_RFU,
                                               Normal = "BLAP protease")
                 )
}


#' Find Biggest Signal
#'
#' Finds the maximum intensity of a single signal
#'
#' @param kinetic_dataframe Data frame of
#' @param mean_RFUs Column name of signal
#' @param Normal Signal to find maximum of, defaults to maximum signal.
#' @return
#' @export
find_biggest_signal <- function(kinetic_dataframe,
                                mean_RFUs = "avg_RFU",
                                Normal = NULL) {
  #ungroup to remove by-minute, filter is applied conditionally. If Normal isn't a protein name, will return
  #max "mean_RFUs" for all proteins
  kinetic_dataframe %>%
    dplyr::ungroup() %>%
    dplyr::filter(if (is.null(Normal) == FALSE) protein == Normal else TRUE) %>%
    dplyr::summarise(max_RFU = max({{mean_RFUs}})) %>%
    dplyr::pull(max_RFU)
}

#' @export
normalize_TxTl <- function(kinetic_dataframe,
                           mean_RFUs = avg_RFU,
                           stnd_dev_RFUs = sd_RFU,
                           max_RFU){
  kinetic_dataframe %>%
    dplyr::mutate(relative_RFU = {{mean_RFUs}}/max_RFU,
           relative_stnd_dev = {{stnd_dev_RFUs}}/max_RFU)
}
