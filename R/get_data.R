#' load_cebl_schedule
#'
#' @description A function to load the Canadian Elite Basketball League (CEBL) schedule.
#'
#' @param seasons A vector of seasons (years) to filter the data.
#'                if NULL, defaults to the range from 2019 to the current year.
#'
#' @returns A data frame containing the CEBL schedule for the specified seasons.
#' @export
#'
#' @examples load_cebl_schedule(2020:2021)
#'
#' ==============================================
#' |          Column Name           |   Type    |
#' ==============================================
#' |fiba_id                         |   dbl     |
#' |season                          |   dbl     |
#' |start_time_utc                  |   dttm    |
#' |status                          |   chr     |
#' |competition                     |   chr     |
#' |venue_name                      |   chr     |
#' |period                          |   dbl     |
#' |home_team_id                    |   dbl     |
#' |home_team_name                  |   chr     |
#' |home_team_score                 |   dbl     |
#' |home_team_logo_url              |   chr     |
#' |home_team_url_stats_en          |   chr     |
#' |home_team_url_stats_fr          |   chr     |
#' |away_team_id                    |   dbl     |
#' |away_team_name                  |   chr     |
#' |away_team_score                 |   dbl     |
#' |away_team_logo_url              |   chr     |
#' |away_team_url_stats_en          |   chr     |
#' |away_team_url_stats_fr          |   chr     |
#' |stats_url_en                    |   chr     |
#' |stats_url_fr                    |   chr     |
#' |cebl_stats_url_en               |   chr     |
#' |cebl_stats_url_fr               |   chr     |
#' |tickets_url_en                  |   chr     |
#' |tickets_url_fr                  |   chr     |
#' |id                              |   dbl     |
#' |fiba_json_url                   |   chr     |
#' ================================  ===========|
#'
load_cebl_schedule <- function(seasons = NULL) {
  seasons <-
    if (is.null(seasons)) {
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      2019:current_year
    } else if (validate_seasons(seasons)) {
      seasons
    } else {
      stop("Invalid season(s)")
    }

  schedule <- readr::read_csv("https://github.com/ryanndu/cebl-data/releases/download/schedule/cebl_schedule.csv")
  schedule <- dplyr::filter(schedule, season %in% seasons)

  return(schedule)
}

#' load_cebl_team_boxscores
#'
#' @description A function to load the Canadian Elite Basketball League (CEBL) team boxscores.
#'
#' @param seasons A vector of seasons (years) to filter the data.
#'
#' @returns A data frame containing the CEBL team boxscores for the specified seasons.
#' @export
#'
#' @examples load_cebl_team_boxscores(2020:2021)
#'
#'
#' ===========================================================
#' |Column Name                                   |   Type   |
#' ===========================================================
#' |game_id                                       |   dbl    |
#' |season                                        |   dbl    |
#' |team_name                                     |   chr    |
#' |short_name                                    |   chr    |
#' |code                                          |   chr    |
#' |team_score                                    |   dbl    |
#' |minutes                                       |   chr    |
#' |field_goals_made                              |   dbl    |
#' |field_goals_attempted                         |   dbl    |
#' |field_goal_percentage                         |   dbl    |
#' |two_point_field_goals_made                    |   dbl    |
#' |two_point_field_goals_attempted               |   dbl    |
#' |two_point__percentage                         |   dbl    |
#' |three_point_field_goals_made                  |   dbl    |
#' |three_point_field_goals_attempted             |   dbl    |
#' |three_point_percentage                        |   dbl    |
#' |free_throws_made                              |   dbl    |
#' |free_throws_attempted                         |   dbl    |
#' |free_throw_percentage                         |   dbl    |
#' |offensive_rebounds                            |   dbl    |
#' |defensive_rebounds                            |   dbl    |
#' |rebounds                                      |   dbl    |
#' |assists                                       |   dbl    |
#' |steals                                        |   dbl    |
#' |turnovers                                     |   dbl    |
#' |blocks                                        |   dbl    |
#' |blocks_received                               |   dbl    |
#' |personal_fouls                                |   dbl    |
#' |fouls_drawn                                   |   dbl    |
#' |total_fouls                                   |   dbl    |
#' |bonus_fouls                                   |   dbl    |
#' |points_in_the_paint                           |   dbl    |
#' |second_chance_points                          |   dbl    |
#' |points_from_turnovers                         |   dbl    |
#' |bench_points                                  |   dbl    |
#' |fast_break_points                             |   dbl    |
#' |team_index_rating                             |   dbl    |
#' |team_index_rating_2                           |   dbl    |
#' |team_index_rating_3                           |   dbl    |
#' |team_index_rating_4                           |   dbl    |
#' |team_index_rating_5                           |   dbl    |
#' |team_index_rating_6                           |   dbl    |
#' |team_index_rating_7                           |   dbl    |
#' |team_fouls                                    |   dbl    |
#' |team_turnovers                                |   dbl    |
#' |team_rebounds                                 |   dbl    |
#' |team_defensive_rebounds                       |   dbl    |
#' |team_offensive_rebounds                       |   dbl    |
#' |period_1_score                                |   dbl    |
#' |period_2_score                                |   dbl    |
#' |period_3_score                                |   dbl    |
#' |period_4_score                                |   dbl    |
#' |biggest_lead                                  |   dbl    |
#' |biggest_scoring_run                           |   dbl    |
#' |time_leading                                  |   dbl    |
#' |lead_changes                                  |   dbl    |
#' |times_scores_level                            |   dbl    |
#' |timeouts_left                                 |   dbl    |
#' |head_coach                                    |   chr    |
#' |assistant_coach_1                             |   chr    |
#' |assistant_coach_2                             |   chr    |
#' |international_team_name                       |   chr    |
#' |international_short_name                      |   chr    |
#' |international_code                            |   chr    |
#' |logo                                          |   chr    |
#' |logo_t_url                                    |   chr    |
#' |logo_t_size                                   |   chr    |
#' |logo_t_height                                 |   dbl    |
#' |logo_t_width                                  |   dbl    |
#' |logo_t_bytes                                  |   dbl    |
#' |logo_s_url                                    |   chr    |
#' |logo_s_size                                   |   chr    |
#' |logo_s_height                                 |   dbl    |
#' |logo_s_width                                  |   dbl    |
#' |logo_s_bytes                                  |   dbl    |
#' ==============================================   ===========
#'
load_cebl_team_boxscores <- function(seasons = NULL) {
  if (is.null(seasons)) {
    current_year <- as.ineger(format(Sys.Date(), "%Y"))
    2019:current_year
  } else if (validate_seasons(seasons)) {
    seasons
  } else {
    stop("Invalid season(s)")
  }

  team_boxscore = readr::read_csv(
    "https://github.com/ryanndu/cebl-data/releases/download/team-boxscore/cebl_teams.csv"
  )
  team_boxscore <- dplyr::filter(team_boxscore, season %in% seasons)

  return(team_boxscore)
}

#' load_cebl_player_boxscores
#'
#' @description A function to load the Canadian Elite Basketball League (CEBL) player boxscores.
#'
#' @param seasons A vector of seasons (years) to filter the data.
#'
#' @returns A data frame containing the CEBL player boxscores for the specified seasons.
#' @export
#'
#' @examples load_cebl_player_boxscores(2020:2021)
#'
#' ======================================   ===========
#' | Column Name                          |    Type   |
#' ======================================   ===========
#' |game_id                               |   dbl    |
#' |season                                |   dbl    |
#' |team_name                             |   chr    |
#' |player_number                         |   dbl    |
#' |player_name                           |   chr    |
#' |player_position                       |   chr    |
#' |minutes                               |   chr    |
#' |podbls                                |   dbl    |
#' |field_goals_made                      |   dbl    |
#' |field_goals_attempted                 |   dbl    |
#' |field_goal_percentage                 |   dbl    |
#' |two_podbl_field_goals_made            |   dbl    |
#' |two_podbl_field_goals_attempted       |   dbl    |
#' |two_podbl__percentage                 |   dbl    |
#' |three_podbl_field_goals_made          |   dbl    |
#' |three_podbl_field_goals_attempted     |   dbl    |
#' |three_podbl_percentage                |   dbl    |
#' |free_throws_made                      |   dbl    |
#' |free_throws_attempted                 |   dbl    |
#' |free_throw_percentage                 |   dbl    |
#' |offensive_rebounds                    |   dbl    |
#' |defensive_rebounds                    |   dbl    |
#' |rebounds                              |   dbl    |
#' |assists                               |   dbl    |
#' |turnovers                             |   dbl    |
#' |steals                                |   dbl    |
#' |blocks                                |   dbl    |
#' |blocks_received                       |   dbl    |
#' |personal_fouls                        |   dbl    |
#' |fouls_drawn                           |   dbl    |
#' |plus_minus                            |   dbl    |
#' |index_rating                          |   dbl    |
#' |index_rating_2                        |   dbl    |
#' |index_rating_3                        |   dbl    |
#' |index_rating_4                        |   dbl    |
#' |index_rating_5                        |   dbl    |
#' |index_rating_6                        |   dbl    |
#' |index_rating_7                        |   lgl    |
#' |second_chance_points                  |   lgl    |
#' |fast_break_points                     |   lgl    |
#' |points_in_the_paint                   |   dbl    |
#' |first_name                            |   chr    |
#' |first_name_initial                    |   chr    |
#' |last_name                             |   chr    |
#' |last_name_initial                     |   chr    |
#' |international_first_name              |   chr    |
#' |international_first_name_initial      |   chr    |
#' |international_last_name               |   chr    |
#' |international_last_name_initial       |   chr    |
#' |scoreboard_name                       |   chr    |
#' |active                                |   lgl    |
#' |starter                               |   lgl    |
#' |captain                               |   lgl    |
#' |photo_t                               |   chr    |
#' |photo_s                               |   chr    |
#' ======================================  ===========

load_cebl_player_boxscores <- function(seasons = NULL) {
  if (is.null(seasons)) {
    current_year <- as.ineger(format(Sys.Date(), "%Y"))
    2019:current_year
  } else if (validate_seasons(seasons)) {
    seasons
  } else {
    stop("Invalid season(s)")
  }

  player_boxscore = readr::read_csv(
    "https://github.com/ryanndu/cebl-data/releases/download/player-boxscore/cebl_players.csv"
  )
  team_boxscore <- dplyr::filter(player_boxscore, season %in% seasons)

  return(player_boxscore)
}

#' load_cebl_officials
#'
#' @description A function to load the Canadian Elite Basketball League (CEBL) officials.
#'
#' @param seasons A vector of seasons (years) to filter the data.
#'
#' @returns A data frame containing the CEBL officials for the specified seasons.
#' @export
#'
#' @examples load_cebl_officials(2020:2021)
#' ================================    ===========
#' |Column Name                      |   Type    |
#' ================================    ===========
#' |game_id                          |   dbl     |
#' |season                           |   dbl     |
#' |officials_type                   |   chr     |
#' |officials_name                   |   chr     |
#' |first_name                       |   chr     |
#' |last_name                        |   chr     |
#' |scoreboard_name                  |   chr     |
#' |first_name_initial               |   chr     |
#' |last_name_initial                |   chr     |
#' |international_first_name         |   chr     |
#' |international_first_name_initial |   chr     |
#' |international_last_name          |   chr     |
#' |international_last_name_initial  |   chr     |
#' |scoreboard_name                  |   chr     |
#' |================================   ===========
#'
load_cebl_officials <- function(seasons = NULL) {
  if (is.null(seasons)) {
    current_year <- as.ineger(format(Sys.Date(), "%Y"))
    2019:current_year
  } else if (validate_seasons(seasons)) {
    seasons
  } else {
    stop("Invalid season(s)")
  }

  officials = readr::read_csv(
    "https://github.com/ryanndu/cebl-data/releases/download/officials/cebl_officials.csv"
  )
  officials <- dplyr::filter(officials, season %in% seasons)

  return(officials)
}

#' load_cebl_coaches
#'
#' @description A function to load the Canadian Elite Basketball League (CEBL) coaches.
#'
#' @param seasons A vector of seasons (years) to filter the data.
#'
#' @returns A data frame containing the CEBL coaches for the specified seasons.
#' @export
#'
#' @examples load_cebl_coaches(2020:2021)
#'
#'================================   ===========
# |Column Name                     |   Type    |
# ================================   ===========
# |game_id                         |   dbl     |
# |season                          |   dbl     |
# |team_name                       |   chr     |
# |coach_name                      |   chr     |
# |coach_type                      |   chr     |
# |first_name                      |   chr     |
# |first_name_initial              |   chr     |
# |last_name                       |   chr     |
# |last_name_initial               |   chr     |
# |international_first_name        |   chr     |
# |international_first_name_initial|   chr     |
# |international_last_name         |   chr     |
# |international_last_name_initial |   chr     |
# |scoreboard_name                 |   chr     |
# ================================  ============
load_cebl_coaches <- function(seasons = NULL) {
  if (is.null(seasons)) {
    current_year <- as.ineger(format(Sys.Date(), "%Y"))
    2019:current_year
  } else if (validate_seasons(seasons)) {
    seasons
  } else {
    stop("Invalid season(s)")
  }

  coaches = readr::read_csv(
    "https://github.com/ryanndu/cebl-data/releases/download/coaches/cebl_coaches.csv"
  )
  coaches <- dplyr::filter(coaches, season %in% seasons)

  return(coaches)
}

#' load_cebl_pbp
#'
#' @description A function to load the Canadian Elite Basketball League (CEBL) play-by-play data.
#'
#' @param seasons A vector of seasons (years) to filter the data.
#'
#' @returns A data frame containing the CEBL play-by-play data for the specified seasons.
#' @export
#'
#' @examples load_cebl_pbp(2020:2021)
#'
#' ================================   ===========
#' |Column Name                     |   Type   |
#' ================================   ===========
#' |game_id                         |   dbl    |
#' |season                          |   dbl    |
#' |game_time                       |   chr    |
#' |home_score                      |   dbl    |
#' |away_score                      |   dbl    |
#' |home_lead                       |   dbl    |
#' |team_id                         |   dbl    |
#' |period                          |   dbl    |
#' |period_type                     |   chr    |
#' |player_id                       |   dbl    |
#' |scoreboard_name                 |   chr    |
#' |success                         |   dbl    |
#' |action_type                     |   chr    |
#' |action_number                   |   dbl    |
#' |previous_action                 |   dbl    |
#' |sub_type                        |   chr    |
#' |scoring                         |   dbl    |
#' |shirt_number                    |   dbl    |
#' |player_name                     |   chr    |
#' |first_name                      |   chr    |
#' |last_name                       |   chr    |
#' |x                               |   dbl    |
#' |y                               |   dbl    |
#' |qualifier_0                     |   chr    |
#' |qualifier_1                     |   chr    |
#' |qualifier_2                     |   chr    |
#' |qualifier_3                     |   chr    |
#' |international_first_name        |   chr    |
#' |international_last_name         |   chr    |
#' |international_first_name_initial|   chr    |
#' |international_last_name_initial |   chr    |
#' ================================   ===========
load_cebl_pbp <- function(seasons = NULL) {
  if (is.null(seasons)) {
    current_year <- as.ineger(format(Sys.Date(), "%Y"))
    2019:current_year
  } else if (validate_seasons(seasons)) {
    seasons
  } else {
    stop("Invalid season(s)")
  }

  pbp <- sapply(seasons, function(season)
    paste0("https://github.com/ryanndu/cebl-data/releases/download/pbp/cebl_pbp_", seasons, ".csv")) |>
    lapply(readr::read_csv) |> dplyr::bind_rows()

  return(pbp)
}
