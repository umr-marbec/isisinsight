test_that("messages expected", {
  expect_error(object = outputs_simulations_settings(directory_path = "my/path/to/simulations/directory"),
                 regexp = "^.+ - Error, no input simulation available in the directory path.")
})
