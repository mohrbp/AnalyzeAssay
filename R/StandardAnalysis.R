#' Quick Analysis
#'
#' This method uses the `import.assaydat`, `dereplicate`, `subtract.background`, `normalize.bysignal` and `report.assaymax` functions to quickly process a standard dataset
#'
#' @param PathtoDatInfile A list of path names to the assay data to be imported.
#' @param PathtoIds A path to the file of identifiers for the assay data.
#' @param GroupBy The names of the variables over which the assay data is separated including minutes. Defaults to DNA, Protein and Minutes. Should not include "Replicate"
#' @param BackgroundSignalType The variable type of the signal to subtract from all other signals
#' @param NameofBackgroundSignal The quoted name of the signal to subtract from all other signals
#' @param NormalizationSignalType The variable type of the signal to divide all other signals
#' @param NameofNormalizationSignal The quoted name of the signal to divide all other signals
#' @param SortGroup The descriptive variable contained in the identifier file used to sort. Quoted
#' @param Writecsv A logical of whether to output a .csv in the current directory titled YYYYMMDD
#' @param Plot A logical of whether to print a ggplot of the kinetic trances of the assay to the console.
#' @param SavePlot A logical of whether to save the ggplot as a .png. Requires Plot = TRUE.
#' @return A report of the max normalized and background subtracted signal for the chosen group.
#' @export

analyzeassay.standard <- function(PathtoDatInfile,
                                  PathtoIds,
                                  GroupBy = c("DNA", "Protein", "Minutes"),
                                  BackgroundSignalType = Protein,
                                  NameofBackgroundSignal = "B",
                                  NormalizationSignalType = Protein,
                                  NameofNormalizationSignal = "C",
                                  SortGroup = "Protein",
                                  Writecsv = FALSE,
                                  Plot = TRUE,
                                  SavePlot = FALSE
                                  ) {
  import.assaydat(PathtoDatInfile,
                  PathtoIds
                  ) -> AssayDat

  print("imported")

  dereplicate(AssayDat,
              GroupBy = GroupBy
              ) -> DereplicatedAssayDat

  subtract.background(DereplicatedAssayDat,
                      Signal = MeanSignal,
                      GroupBy = "Minutes",
                      BackgroundSignalType = {{BackgroundSignalType}},
                      NameofBackgroundSignal = NameofBackgroundSignal
                      ) -> BackgroundSubtractedAssayDat

  print("background subtracted")


  normalize.bysignal(BackgroundSubtractedAssayDat,
                     Signal = "BckSubSignal",
                     SignalStat = "SDSignal",
                     GroupBy = "Minutes",
                     NormalizationSignalType = {{NormalizationSignalType}},
                     NameofNormalizationSignal = NameofNormalizationSignal
                     ) -> NormalizedAssayDat

  print("normalized")

  GroupBy %>% magrittr::extract(. != "Minutes") -> Report_GroupBy

  report.assaymax(NormalizedAssayDat,
                  GroupBy = Report_GroupBy,
                  Signal = NormalizedSignal,
                  SDSignal = NormalizedSignalStat,
                  SortBy = PathtoIds,
                  SortGroup = SortGroup,
                  Writecsv = Writecsv
                  )

  print("reported")


  if (isTRUE(Plot)) {

    PlotGroup <- as.name(SortGroup)

    ggplot2::ggplot(NormalizedAssayDat,
                    ggplot2::aes(x = Minutes,
                        y = NormalizedSignal,
                        color = {{PlotGroup}})
    ) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() -> p
      print(p)

    if (isTRUE(SavePlot)) {
      Date <- format(Sys.Date(), "%y%m%d")
      ggplot2::ggsave(filename = paste(Date, ".png", sep = ""), plot = p)
    }
  }

}
