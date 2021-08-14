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

#' @export
join_Txtl <- function(fl_dat,
                      ids){
  #pivots RFUS to long format then adds info stored in identifiers (protein name, DNA, promoter, replicate, etc.)
  fl_dat %>%
    tidyr::pivot_longer(cols = -minutes, names_to = "well", values_to = "RFU") %>%
    dplyr::left_join(ids, by = "well")
}
