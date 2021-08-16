#' Dereplicate assay data
#'
#' This function loads assay data as long data table with descriptive variables at each time point. And de-replicates them over the chosen variable.
#'
#' @param AssayDat Df of the long data table input file
#' @param GroupBy The names of the variables over which the assay data is separated including minutes. Defaults to DNA, protein and minutes. Should not include "replicate"
#' @param Signal The name of the assay signal being measured.
#' @param method Method used to dereplicate data. CT calculates mean, standard deviation and a 95% confidence interval MM calculate max and min.
#' @return A long kinetic data table with Mean, SD and a 95% confidence internal of replicates
#' @export

Dereplicate <- function(AssayDat,
                        GroupBy = c("DNA", "minutes", "protein"),
                        Signal = intensity,
                        Method = "CT") {
  #computes stats of central tendency; averages technical replicates

  if(Method == "CT") {
  AssayDat %>%
    dplyr::group_by(dplyr::across(GroupBy)) %>%
    dplyr::summarise(mean_signal = mean({{Signal}}),
                     SD_signal = stats::sd({{Signal}}),
                     CI_95 = stats::qnorm(0.975)*SD_signal/(sqrt(3))
                     )
  } else if (Method == "MM") {
    AssayDat %>%
      dplyr::group_by(dplyr::across(GroupBy)) %>%
      dplyr::summarise(max_signal = max({{Signal}}),
                       min_signal = min({{Signal}})
      )
  }
}
