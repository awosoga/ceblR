read_rds <- function(file_url) {
  tryCatch(readRDS(url(file_url)), error = function(e) data.frame()) %>%
    suppressWarnings()
}
