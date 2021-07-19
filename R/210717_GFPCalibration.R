uMFl_to_uMGFP_top510 <- function(fl_dat, uM_Fl, sd_uM_Fl){
  fl_dat %>%
    dplyr::mutate(
      uM_GFP = {{uM_Fl}} * 8.03 + 1.02,
      sd_uM_GFP = {{sd_uM_Fl}} * 8.03 + 1.02,
      percentAccuracy = uM_GFP/concentration * 100
    ) 
}

uMFl_to_uMGFP_bottom <- function(fl_dat, uM_Fl, sd_uM_Fl){
  fl_dat %>%
    dplyr::mutate(
      uM_GFP = uM_Fl * 5.85 + 0.33,
      sd_uM_GFP = {{sd_uM_Fl}} * 5.85 + 0.33,
      percentAccuracy = uM_GFP/concentration * 100
    ) 
}