normalizetoSignal <- function(kdat, ){
  
  kdat %>%
  normalize_TxTl(mean_RFUs = bck_sub_RFU, 
                 max_RFU = find_biggest_signal(., 
                                               mean_RFUs = bck_sub_RFU, 
                                               Normal = "BLAP protease")
                 )
}


find_biggest_signal <- function(kinetic_dataframe, 
                                mean_RFUs = "avg_RFU", 
                                Normal = NULL) {
  #ungroup to remove by-minute, filter is applied conditionally. If Normal isn't a protein name, will return
  #max "mean_RFUs" for all proteins
  kinetic_dataframe %>%
    ungroup() %>%
    dplyr::filter(if (is.null(Normal) == FALSE) protein == Normal else TRUE) %>% 
    dplyr::summarise(max_RFU = max({{mean_RFUs}})) %>%
    dplyr::pull(max_RFU)
}
  
  
normalize_TxTl <- function(kinetic_dataframe, 
                           mean_RFUs = avg_RFU, 
                           stnd_dev_RFUs = sd_RFU, 
                           max_RFU){
  kinetic_dataframe %>%
    dplyr::mutate(relative_RFU = {{mean_RFUs}}/max_RFU,
           relative_stnd_dev = {{stnd_dev_RFUs}}/max_RFU)
}