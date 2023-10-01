#' Get CEBL player boxscore data
#'
#' @param year The season(s) of interest.
#'
#' @return A dataframe of player boxscore data within the requested season(s).
#' @export
#'
#' @examples
#' try({
#' player_boxscores <- player_boxscore_data(2020)
#' player_boxscores_2019_and_2020 <- player_boxscore_data(c(2019:2020))
#' })
player_boxscore_data <- function(year) {
  sapply(year, function(season)
    paste0("https://github.com/awosoga/ceblR_data/releases/download/box_scores/player_boxscores_",
           season, ".rds")) %>% lapply(read_rds) %>% dplyr::bind_rows()
}

#' Get CEBL team boxscore data
#'
#' @param year The season(s) of interest.
#'
#' @return A dataframe of team boxscore data within the requested season(s).
#' @export
#'
#' @examples
#' try({
#' team_boxscores <- team_boxscore_data(2020)
#' team_boxscores_2019_and_2020 <- team_boxscore_data(c(2019:2020))
#' })
team_boxscore_data <- function(year) {
  sapply(year, function(season)
    paste0("https://github.com/awosoga/ceblR_data/releases/download/box_scores/team_boxscores_",
           season, ".rds")) %>% lapply(read_rds) %>% dplyr::bind_rows()
}

#' Get CEBL player advanced data
#'
#' @param year The season(s) of interest.
#'
#' @return A dataframe of player advanced data within the requested season(s).
#' @export
#'
#' @examples
#' try({
#' player_advanced_data <- player_advanced_data(2020)
#' player_advanced_data_2019_and_2020 <- player_advanced_data(c(2019:2020))
#' })
player_advanced_data <- function(year) {
  sapply(year, function(season)
    paste0("https://github.com/awosoga/ceblR_data/releases/download/advanced_data/advanced_data_",
           season, ".rds")) %>% lapply(read_rds) %>% dplyr::bind_rows()
}

