#' Import assay data and make long
#'
#' Imports wide assay data from excel files and binds the identifying information. Returns long data. Defaults to kinetic data divided by well.
#'
#' @param PathtoDatInfile A list of path names to the assay data to be imported.
#' @param PathtoIds A path to the file of identifiers for the assay data.
#' @param ColstoExclude Pass into MakeLongandJoin.Columns not made long.
#' @param By Pass into MakeLongandJoin. Key to join assay data and identifiers.
#' @return A long assay data table of all of the input assay data bound to the identifying information provided by Ids.
#'
#' @export
Import_AssayDat <- function(PathtoDatInfile,
                           PathtoIds,
                           ColstoExclude = c("minutes"),
                           By = "well"
                           ){

  #import excels based on file path, must be path from working directory
  #Adds info stored in identifiers
  #Pivots non-minutes columns long and joins all files to a single df

  Identifiers <- read_excel(PathtoIds)
  purrr::map(.x = PathtoDatInfile, .f = readxl::read_xlsx) %>%
  purrr::map(MakeLongandJoin, Ids = Identifiers, ColstoExclude = ColstoExclude, By = By) %>%
    dplyr::bind_rows()
}


#' Make long and join
#'
#' Makes assay data long and joins with descriptive variables.
#'
#' @param AssayDat A wide data table of assay data by well and time.
#' @param Ids A long data table of descriptive variables for each assay well.
#' @param ColstoExclude Colnames of columns not to make long, defaults to "minutes" for kinetic data.
#' @param By Names of the location variable and key for joining identifiers to Assay data; defaults to "well".
#' @return A data table of assay data bound to the descriptive variables in the identifiers file.
#' @export
MakeLongandJoin <- function(AssayDat,
                      Ids,
                      ColstoExclude = c("minutes"),
                      By = c("well")
                      )
                      {
  AssayDat %>%
    tidyr::pivot_longer(cols = -(ColstoExclude), names_to = By, values_to = "intensity") %>%
    dplyr::left_join(Ids, by = By)
}
