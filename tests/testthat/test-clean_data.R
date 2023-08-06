test_that("clean_data() removes unwanted tabs, spaces, and newlines from a string", {
  expect_equal(clean_data("\t cebl \n    "), "cebl")
})
