#' Import and make long
#'
#' Imports wide kinetic data from excel files and binds the identifying information. Returns long data.
#'
#' @param excels A list of path names to the kinetic data to be imported
#' @param id A path to the file of identifiers for the kinetic data
#' @return A long kinetic data table of all of the input kinetic data bound to the identifying information provided by Ids.
#'
#' @export
import_kinetic <- function(excels = c("yymmdd_run_1A","yymmdd_run_1B"),
                           ids = identifiers){

  #import excels based on file path, must be path from working directory
  #Adds info stored in identifiers
  #Pivots non-minutes columns long and joins all files to a single df

  purrr::map(excels, readxl::read_excel()) %>%
    purrr::map(join_Txtl, ids = identifiers) %>%
    dplyr::bind_rows()
}


#' Make long and join
#'
#' Makes kinetic data long and joins with descriptive variables
#'
#' @param fl_dat A wide data table of Assay data by well and time
#' @param ids A long data table of descriptive variables for each Assay well
#' @return A data table of
#' @export
join_Txtl <- function(fl_dat,
                      ids){
  #pivots RFUS to long format then adds info stored in identifiers (protein name, DNA, promoter, replicate, etc.)
  fl_dat %>%
    tidyr::pivot_longer(cols = -minutes, names_to = "well", values_to = "RFU") %>%
    dplyr::left_join(ids, by = "well")
}
