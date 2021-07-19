stats_for_techreps <- function(kdat, variables = c("DNA", "minutes", "protein"), signal = RFU) {
  #computes stats of central tendency; averages technical replicates 
  kdat %>%
    dplyr::group_by(across(groups)) %>%
    dplyr::summarise(mean_signal = mean({{RFU}}), SD_signal = sd({{RFU}}), tech_CI_95 = qnorm(0.975)*SD_signal/(sqrt(3))) 
}