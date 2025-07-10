test_that("load_cebl_team_boxscores() works", {
  team_boxscore_data_2020 <- load_cebl_team_boxscores(2020)
  expect_type(team_boxscore_data_2020, "list")
  expect_true(nrow(team_boxscore_data_2020) != 0)
})

test_that("load_cebl_player_boxscores() works", {
  player_boxscore_data_2020 <- load_cebl_player_boxscores(2020)
  expect_type(player_boxscore_data_2020, "list")
  expect_true(nrow(player_boxscore_data_2020) != 0)
})

