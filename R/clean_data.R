#' Remove spaces, tabs, and newlines from a string
#'
#' @param dirty_data A character string
#'
#' @return A character string  without spaces, tabs, and newline characters
#' @export
#'
#' @examples
#' dirty_data <- " \n CEBL "
#' clean_data(dirty_data)
clean_data <- function(dirty_data) {
  sapply(dirty_data, stringr::str_remove_all,"[\n\t\\s]") %>% unname()
}
