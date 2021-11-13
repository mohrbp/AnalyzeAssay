#' Dereplicate assay data
#'
#' This function loads assay data as long data table with descriptive variables at each time point. And de-replicates them over the chosen variable.
#'
#' @param AssayDat Df of the long data table input file
#' @param GroupBy The names of the variables over which the assay data is separated including minutes. Defaults to DNA, Protein and Minutes. Should not include "Replicate"
#' @param Signal The name of the assay signal being measured.
#' @param Method Method used to dereplicate data. CT calculates mean, standard deviation and a 95% confidence interval MM calculate max and min.
#' @return A long kinetic data table with Mean, SD and a 95% confidence internal of replicates
#' @export

dereplicate <- function(AssayDat,
                        GroupBy = c("DNA", "Minutes", "Protein"),
                        Signal = Intensity,
                        Method = "CT") {

  #computes stats of central tendency; averages technical replicates

  if (Method == "CT") {
  AssayDat %>%
    dplyr::group_by(dplyr::across(GroupBy)) %>%
    dplyr::summarise(MeanSignal = mean({{Signal}}),
                     SDSignal = stats::sd({{Signal}}),
                     CI95 = stats::qnorm(0.975)*SDSignal/(sqrt(3))
                     )
  } else if (Method == "MM") {
    AssayDat %>%
      dplyr::group_by(dplyr::across(GroupBy)) %>%
      dplyr::summarise(MaxSignal = max({{Signal}}),
                       MinSignal = min({{Signal}})
      )
  }
}
