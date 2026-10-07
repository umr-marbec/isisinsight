# Extracted from test-outputs_simulations_settings.R:3

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "isisinsight", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
expect_error(object = outputs_simulations_settings(directory_path = "my/path/to/simulations/directory"),
               regexp = "ERROR.*No input simulation available in the directory path.")
