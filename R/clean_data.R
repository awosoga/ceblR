clean_data <- function(dirty_data) {
  unname(sapply(dirty_data, stringr::str_remove_all,"[\n\t\\s]") )
}
