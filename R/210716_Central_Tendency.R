#' Compute measures of central tendency over technical replicates
#'
#' This function loads kinetic data as long data table with descriptive variables at each timepoint.
#'
#' @param kdat Path of the long kinetic data table input file
#' @param variables The names of the variables over which the assay data is separated including minutes. Defaults to DNA, protein and minutes. Should not include "replicate"
#' @param signal The name of the assay signal being measured. Defaults to RFU from Join_Txtl
#' @return A long kinetic data table with Mean, SD and a 95% confidence internal of replicates
#' @export
stats_for_techreps <- function(kdat, variables = c("DNA", "minutes", "protein"), signal = RFU) {
  #computes stats of central tendency; averages technical replicates
  kdat %>%
    dplyr::group_by(dplyr::across(groups)) %>%
    dplyr::summarise(mean_signal = mean({{RFU}}), SD_signal = stats::sd({{RFU}}), tech_CI_95 = stats::qnorm(0.975)*SD_signal/(sqrt(3)))
}
