X210602_BASF_SD_Bac_1_analysis <- function(fl_dat){
  #run with output from importdatafromList
  
  fl_dat %>%
    txtl_stats(groups = c("DNA", "minutes", "protein", "promoter")) %>%
    subtract_background_kinetic() %>%
    normalize_TxTl(mean_RFUs = bck_sub_RFU, 
                   max_RFU = find_biggest_signal(., mean_RFUs = bck_sub_RFU, Normal = "BLAP protease")) %>%
    filter(protein != "Amy316") %>%
#    summarize_TxTl(mean_RFUs = avg_RFU, 
#                   stnd_dev_RFUs = sd_RFU, 
#                   groups = c("protein", "promoter")) %>%
#      export_TxTl_summary(ids = identifiers, 
#                         saveas = "SD_Bac_1_FlAsH")
#    filter(protein %in% c("Amy316", "Amy296", "BLAP protease", "MBP", "Xyl07", "Xyl74")) %>%
    plot_TxTl(y = relative_RFU, stnd_dev_RFUs = relative_stnd_dev)
    
}


plot_TxTl <- function(fl_dat, 
                      y = avg_RFU, 
                      stnd_dev_RFUs = sd_RFU) {
  fl_dat %>%
    ggplot(aes(x = minutes,
               y = {{y}},
               ymin = {{y}} - {{stnd_dev_RFUs}}, 
               ymax = {{y}} + {{stnd_dev_RFUs}},
               color = protein)
    ) +
    geom_pointrange(fatten = 0.5) +
    facet_grid(cols = vars(promoter)) +
    theme_minimal() -> P
  return(P)
}


importdatafromList <- function(excels = c("yymmdd_run_1A","yymmdd_run_1B"), 
                               ids = identifiers){
  #requires purrr, read_xlsx, and join_TxTl
  
  #import excels based on file path, must be path from working directory
  #Adds info stored in identifiers 
  #Pivots non-minutes columns long and joins all files to a single df
  
  map(excels, read_xlsx) %>%
    map(join_Txtl, ids = identifiers) %>%
    bind_rows() 
  }


join_Txtl <- function(fl_dat, 
                      ids){
  #pivots RFUS to long format then adds info stored in identifiers (protein name, DNA, promoter, replicate, etc.)
  fl_dat %>%
    pivot_longer(cols = -minutes, names_to = "well", values_to = "RFU") %>%
    left_join(ids, by = "well")
}

txtl_stats <- function(fl_dat, 
                       groups = c("DNA", "minutes", "protein")){
  #computes stats of central tendency; averages replicate(s)
  fl_dat %>%
    group_by(across(groups)) %>%
    dplyr::summarise(avg_RFU = mean(RFU), sd_RFU = sd(RFU), CI_95 = qnorm(0.975)*sd_RFU/(sqrt(3))) 
}


subtract_background_kinetic <- function(kinetic_dataframe, mean_RFUs = avg_RFU, 
                                        NameofBackgroundSignal = "noDNA", 
                                        groups = c("minutes")){
  # subtracting the baseline per minute; references a DNA name. 
  kinetic_dataframe %>%
    ungroup() %>%
    group_by(across(groups)) %>%
    dplyr::mutate(bck_sub_RFU = {{mean_RFUs}} - {{mean_RFUs}}[DNA == NameofBackgroundSignal])
}
    
find_biggest_signal <- function(kinetic_dataframe, 
                                mean_RFUs = "avg_RFU", 
                                Normal = NULL) {
  #ungroup to remove by-minute, filter is applied conditionally. If Normal isn't a protein name, will return
  #max "mean_RFUs" for all proteins
  kinetic_dataframe %>%
    ungroup() %>%
    filter(if (is.null(Normal) == FALSE) protein == Normal else TRUE) %>% 
    summarise(max_RFU = max({{mean_RFUs}})) %>%
    pull(max_RFU)
}


normalize_TxTl <- function(kinetic_dataframe, 
                           mean_RFUs = avg_RFU, 
                           stnd_dev_RFUs = sd_RFU, 
                           max_RFU){
  kinetic_dataframe %>%
    mutate(relative_RFU = {{mean_RFUs}}/max_RFU,
           relative_stnd_dev = {{stnd_dev_RFUs}}/max_RFU)
}

summarize_TxTl <- function(data_frame, 
                           mean_RFUs = avg_RFU, 
                           stnd_dev_RFUs = sd_RFU, 
                           groups = c("protein")){
  data_frame %>%
    group_by(across(groups)) %>%
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
      arrange(factor(protein, levels = ProteinOrder)) %>%
      write.csv(file = paste(date, saveas, ".csv", sep = '_'))
  } else {
    TxTl_summary %>%
      arrange(factor(protein, levels = ProteinOrder)) %>%
      print()
  }
}

importdata_210601 <- function(){
  
  identifiers <- read_excel("210601_cleaned_data/210601_data_identifiers.xlsx")
  flash_1A <- read_excel("210601_cleaned_data/210521_SD_BAC_1A_FlAsH_cleaned_data.xlsx")
  flash_1B <- read_excel("210601_cleaned_data/210524_SD_BAC_1B_FlAsH_cleaned_data.xlsx")
  flash_1C <- read_excel("210601_cleaned_data/210525_SD_BAC_1C_FlAsH_cleaned_data.xlsx")
  
  mgapt_1A <- read_excel("210601_cleaned_data/210521_SD_BAC_1A_MgApt_cleaned_data_gain75.xlsx")
  mgapt_1B <- read_excel("210601_cleaned_data/210524_SD_BAC_1B_MgApt_cleaned_data_gain75.xlsx")
  mgapt_1C <- read_excel("210601_cleaned_data/210525_SD_BAC_1C_MgApt_cleaned_data_gain75.xlsx")
  
}