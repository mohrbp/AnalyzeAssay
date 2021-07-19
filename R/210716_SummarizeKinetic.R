SummarizeKinetic <- function(kdat){
  
  summarize_TxTl(mean_RFUs = avg_RFU, 
                stnd_dev_RFUs = sd_RFU, 
                groups = c("protein", "promoter")
                ) %>%
    export_TxTl_summary(ids = identifiers, 
                        saveas = "SD_Bac_1_FlAsH")
}

summarize_TxTl <- function(data_frame, 
                           mean_RFUs = avg_RFU, 
                           stnd_dev_RFUs = sd_RFU, 
                           groups = c("protein")){
  data_frame %>%
    dplyr::group_by(across(groups)) %>%
    dplyr::summarise(max_signal_per_protein = max({{mean_RFUs}}),
                     # max mean RFU may occur at multiple points, the following takes the earliest point
                     time_at_max = min(minutes[{{mean_RFUs}} == max({{mean_RFUs}})]),
                     stnd_dev_at_max = {{stnd_dev_RFUs}}[minutes == time_at_max],
                     confidence_interval_95_at_max = qnorm(0.975)*stnd_dev_at_max/(sqrt(3)),
                     logic = if_else({{mean_RFUs}}[minutes == minutes[1]] < {{mean_RFUs}}[minutes == minutes[3]], "true", "false")) -> TxTlSum
  return(TxTlSum)
}

export_TxTl_summary <- function(TxTl_summary,
                                ids = identifiers,
                                saveas = NULL){
  
  unique(ids$protein) -> ProteinOrder
  date <- format(Sys.Date(), "%y%m%d")
  
  if (is.null(saveas) == FALSE) {
    TxTl_summary %>%
      dplyr::arrange(factor(protein, levels = ProteinOrder)) %>%
      utils::write.csv(file = paste(date, saveas, ".csv", sep = '_'))
  } else {
    TxTl_summary %>%
      dplyr::arrange(factor(protein, levels = ProteinOrder)) %>%
      print()
  }
}