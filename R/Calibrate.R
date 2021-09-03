# Not included in Rbuild, still a work in progress 20210902

#' Transform data with a calibration curve
#'
#' This function imports a data file and an identifiers file to create a calibration curve from well plate data.
#' Data is then transformed with the resulting calibration curve. Calibration curves can be generated for specific subsets of the assay data.
#'
#'




# Could be unified to create import.excel.
#' Import Calibration Data
#'
#' This function imports calibration curve data and is equivalent to import.assaydat with different defaults.
#'
#'
#' @param PathtoDatInfile A list of path names to the calibration data to be imported.
#' @param PathtoIds A path to the file of identifiers for the calibration data.
#' @param ColstoExclude Pass into make.long.join Columns not made long.
#' @param By Pass into make.long.join. Key to join calibration data and identifiers.
#' @return A long calibration data table of all of the input calibration data bound to the identifying information provided by Ids.
#'
#' @export
import.caldat <- function(PathtoDatInfile,
                            PathtoIds,
                            ColstoExclude = c("Minutes"),
                            By = "Well"
) {

  #import excels based on file path, must be path from working directory
  #Adds info stored in identifiers
  #Pivots non-minutes columns long and joins all files to a single df

  Identifiers <- readxl::read_xlsx(PathtoIds)
  purrr::map(.x = PathtoDatInfile, .f = readxl::read_xlsx) %>%
    purrr::map(make.long.join, Ids = Identifiers, ColstoExclude = ColstoExclude, By = By) %>%
    dplyr::bind_rows()
}
