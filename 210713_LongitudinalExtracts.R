X210713_plotLongitudinalExtracts <- function(){
  
  read_excel("210713_ids.xlsx") -> identifiers
  importdatafromList(excels = "210713_ExtractsLongitude_bottom_cleaneddata.xlsx") -> fl_dat
 
  
  fl_dat %>%
    txtl_stats(groups = c("type", "minutes", "sample", "concentration")) -> fl_stats 
  
  
#   Quick plot
  fl_stats %>%
    filter(sample != "fluorescein") %>%
    plot_TxTl()
  
  fl_stats %>%
    filter(sample == "fluorescein") %>%
    Average_FLStnd_LinearRegression() -> curve
  
  fl_stats %>%
    filter(sample != "fluorescein") %>% 
    crossing(subset(curve, method == "hand")) %>%
    #using a relatively slow cross join because its not that many rows
    RFU_to_uMFl() %>%
    print()
    uMFl_to_uMGFP_bottom(uM_Fl = uM_Fl, sd_uM_Fl = sd_uM_Fl) %>% 
    plot_TxTl(y = uM_GFP, stnd_dev_RFUs = sd_uM_GFP)
  
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


plot_TxTl <- function(fl_dat, 
                      y = avg_RFU, 
                      stnd_dev_RFUs = sd_RFU) {
  fl_dat %>%
    filter(sample != "none") %>%
    ggplot(aes(x = minutes,
               y = {{y}},
               ymin = {{y}} - {{stnd_dev_RFUs}}, 
               ymax = {{y}} + {{stnd_dev_RFUs}},
               color = as.factor(concentration))
    ) +
    geom_pointrange(fatten = 0.5) +
#    geom_point() +
    facet_grid(rows = vars(sample)) +
    labs(color = "WT GFP DNA \nConcentration (uM)") +
    theme_minimal() -> P
  return(P)
}

Average_FLStnd_LinearRegression <- function(StndDat, 
                                            variables = c("sample", "concentration", "type")){
 
  #Regression performed from the average of the fluorescein signal over the time course of the experiment
  #this is still for multiple regressions, this time by type rather than by minute.
  #not sure if this will work for a single regression
  
  #this is bad because I folded the central tendency function 
  #into this which makes the summarise at the end need another option to recover the method
  
  StndDat %>%
    group_by(across(variables)) %>%
    summarise(avg_RFU = mean(avg_RFU)) %>%
    tidyr::nest(data = -type) %>%
    mutate(
      fit = map(data, ~ lm(concentration ~ avg_RFU, data = .x)),
      tidied = map(fit, broom::tidy)
    ) %>%
    tidyr::unnest(tidied) %>%
    summarise(
      slope = estimate[term == "avg_RFU"],
      intercept = estimate[term == "(Intercept)"],
      method = type[term == "avg_RFU"]
    ) 
}



Dynamic_FLStnd_LinearRegression <- function(StndDat, 
                                    variables = c("minutes", "sample")){
  # Multiple regressions performed every minute of the data
  StndDat %>%
    ungroup() %>%
    tidyr::nest(data = -variables) %>%
    mutate(
      fit = map(data, ~ lm(concentration ~ avg_RFU, data = .x)),
      tidied = map(fit, broom::tidy)
    ) %>%
    tidyr::unnest(tidied) %>%
    group_by(across(variables)) %>%
    summarise(
      slope = estimate[term == "avg_RFU"],
      intercept = estimate[term == "(Intercept)"]
    )
}


RFU_to_uMFl <- function(fl_dat){
  
  fl_dat %>%
    mutate(
      uM_Fl = avg_RFU * slope + intercept,
      sd_uM_Fl = sd_RFU * slope + intercept
    )
}

uMFl_to_uMGFP_top510 <- function(fl_dat, uM_Fl, sd_uM_Fl){
  fl_dat %>%
    mutate(
      uM_GFP = {{uM_Fl}} * 8.03 + 1.02,
      sd_uM_GFP = {{sd_uM_Fl}} * 8.03 + 1.02,
      percentAccuracy = uM_GFP/concentration * 100
    ) 
}

uMFl_to_uMGFP_bottom <- function(fl_dat, uM_Fl, sd_uM_Fl){
  fl_dat %>%
    mutate(
      uM_GFP = uM_Fl * 5.85 + 0.33,
      sd_uM_GFP = {{sd_uM_Fl}} * 5.85 + 0.33,
      percentAccuracy = uM_GFP/concentration * 100
    ) 
}