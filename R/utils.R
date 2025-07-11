#' Checks whether all the provided seasons are valid years
#' @noRd
#' @param seasons A vector of seasons (years) to filter the data.
#'                if NULL, defaults to the range from 2019 to the current year.
#'
#' @returns A logical value indicating whether the seasons are valid.
#'
validate_seasons <- function(seasons) {
  tryCatch({
    if (!is.vector(seasons) || is.list(seasons)) {
      stop("Expected a vector of years")
    }

    current_year <- as.integer(format(Sys.Date(), "%Y"))
    for (year in seasons) {
      if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
        stop(paste0("Expected a numeric year, got ", typeof(year)))
      }
      if (year < 2019 || year > current_year) {
        stop(paste0("Year ", year, " out of valid range (2019-", current_year, ")"))
      }
    }

    return(TRUE)
  }, error = function(e) {
    paste("Invalid 'seasons' input:", e$message)
    return(FALSE)
  })
}









read_rds <- function(file_url) {
  tryCatch(readRDS(url(file_url)), error = function(e) data.frame()) %>%
    suppressWarnings()
}
