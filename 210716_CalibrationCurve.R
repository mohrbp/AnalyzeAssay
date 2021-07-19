Calibration <- function(kdat, calibrant = "fluorescein", 
                        concentration = c(0,3,6,9,12,15), 
                        units = "uM",
                        variables = c("minutes", "sample")
                        ) {

k_dat %>%
  dplyr::filter(sample == calibrant) %>%
    Dynamic_FLStnd_LinearRegression() -> curve
  
k_dat %>%
  dplyr::filter(sample != calibrant) %>% 
  tidyr::crossing(subset(curve, method == "hand")) %>%
  RFU_to_uMFl()
                          
                          
  }
Dynamic_FLStnd_LinearRegression <- function(StndDat, variables = c("minutes", "sample")){
  # Multiple regressions performed every minute of the data
  StndDat %>%
    dplyr::ungroup() %>%
    tidyr::nest(data = -variables) %>%
    dplyr::mutate(
      fit = purr::map(data, ~ lm(concentration ~ avg_RFU, data = .x)),
      tidied = purr::map(fit, broom::tidy)
    ) %>%
    tidyr::unnest(tidied) %>%
    dplyr::group_by(across(variables)) %>%
    dplyr::summarise(
      slope = estimate[term == "avg_RFU"],
      intercept = estimate[term == "(Intercept)"]
    )
}

RFU_to_uMFl <- function(fl_dat){
  
  fl_dat %>%
    dplyr::mutate(
      uM_Fl = avg_RFU * slope + intercept,
      sd_uM_Fl = sd_RFU * slope + intercept
    )
}
                        