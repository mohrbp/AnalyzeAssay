#' Import assay data and make long
#'
#' Imports wide assay data from excel files and binds the identifying information. Returns long data. Defaults to kinetic data divided by well.
#'
#' @param PathtoDatInfile A list of path names to the assay data to be imported.
#' @param PathtoIds A path to the file of identifiers for the assay data.
#' @param ColstoExclude Pass into make.long.join Columns not made long.
#' @param By Pass into make.long.join. Key to join assay data and identifiers.
#' @return A long assay data table of all of the input assay data bound to the identifying information provided by Ids.
#'
#' @export
import.assaydat <- function(PathtoDatInfile,
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


#' Make long and join
#'
#' Makes assay data long and joins with descriptive variables.
#'
#' @param AssayDat A wide data table of assay data by well and time.
#' @param Ids A long data table of descriptive variables for each assay well.
#' @param ColstoExclude Colnames of columns not to make long, defaults to "Minutes" for kinetic data.
#' @param By Names of the location variable and key for joining identifiers to Assay data; defaults to "Well".
#' @return A data table of assay data bound to the descriptive variables in the identifiers file.
#' @export
make.long.join <- function(AssayDat,
                      Ids,
                      ColstoExclude = c("Minutes"),
                      By = c("Well")
                      ) {
  AssayDat %>%
    tidyr::pivot_longer(cols = -(ColstoExclude), 
                        names_to = By, 
                        values_to = "Intensity", 
                        values_transform = list(Intensity = as.numeric)) %>%
    dplyr::right_join(Ids, by = By)
}
