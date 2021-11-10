#' ReportMax assay data in a standard format
#'
#' This function loads assay data as long data table with descriptive variables at each time point
#' and reports the Maximum value with a confidence interval around that value.
#' @param AssayDat Df of the long data table input file
#' @param GroupBy The names of the variables over which the assay data is separated including minutes. Defaults to DNA, protein and minutes. Should not include "replicate"
#' @param Signal The name of the assay signal being measured.
#' @param SDSignal The name of the standard deviaation of the assay signal.
#' @param SortBy The file path of the identifiers in Xlsx format used to sort the Report. Defaults to NULL.
#' @param SortGroup The descriptive variable contained in the identifier file used to sort. Quoted
#' @param SortFactor The descriptive variables contained in the identifier file used to sort. Not Quoted
#' @return A data table with Mean, SD and a 95% confidence internal of replicates
#' @export

ReportAssayMax <- function(AssayDat,
                           GroupBy = c("Protein"),
                           Signal = Intensity,
                           SDSignal = SDSignal,
                           SortBy = NULL,
                           SortGroup = "Protein",
                           SortFactor = Protein
                           )
                           {
   AssayDat %>%
    dplyr::group_by(dplyr::across(GroupBy)) %>%
    dplyr::summarise(MaxSignalperGroup = max({{Signal}}),
                     # max mean RFU may occur at multiple points, the following takes the earliest point
                     TimeatMax = min(Minutes[{{Signal}} == max({{Signal}})]),
                     SDatMax = {{SDSignal}}[Minutes == TimeatMax],
                     CI95atMax = qnorm(0.975)*SDatMax/(sqrt(3)),
                     SignalIncreasesoverTime = if_else({{Signal}}[Minutes == Minutes[1]] < {{Signal}}[Minutes == Minutes[3]], "True", "False")) -> Report

  # Sorts the Report in the same order as the file of identifiers. Useful for when certain data (controls) should be displayed first.

  if (is.null(SortBy) == FALSE) {

    read_xlsx(SortBy) -> Ids
    unique(Ids[[SortGroup]]) -> SortOrder
    Date <- format(Sys.Date(), "%y%m%d")


    Report %>%
      arrange(factor({{SortFactor}}, levels = SortOrder)) %>%
      write.csv(file = paste(Date,".csv", sep = ''))
  } else{
    print(Report)
  }
}

