test_that("team_boxscore_data() works", {
  team_boxscore_data_2020 <- team_boxscore_data(2020)
  expect_type(team_boxscore_data_2020, "list")
  expect_true(nrow(team_boxscore_data_2020) != 0)
})

test_that("player_boxscore_data() works", {
  player_boxscore_data_2020 <- player_boxscore_data(2020)
  expect_type(player_boxscore_data_2020, "list")
  expect_true(nrow(player_boxscore_data_2020) != 0)
})

test_that("player_advanced_data() works", {
  player_advanced_data_2020 <- player_advanced_data(2020)
  expect_type(player_advanced_data_2020, "list")
  expect_true(nrow(player_advanced_data_2020) != 0)
})

test_that("team_advanced_data() works", {
  team_advanced_data_2020 <- team_advanced_data(2020)
  expect_type(team_advanced_data_2020, "list")
  expect_true(nrow(team_advanced_data_2020) != 0)
})
